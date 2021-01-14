
argF <- function(cohorts, predictors, asset_data, soil_data, val_start, val_end,
                 test_start, test_end, outages, maxN, minN, Nfailcutoff,
                 updateData, forceUpdate, mainDir) {
  subDir <- as.character(lubridate::year(val_end))
  path4 <- file.path(mainDir, subDir, paste0("Table1NHPP_cohorts_", subDir, ".RDS"))
  path1 <- file.path(mainDir, paste0("NHPPInputWO_Cohort_", subDir, ".RDS"))
  path2 <- file.path(mainDir, paste0("NHPPInputAssetCohort_", subDir, ".RDS"))
  modelSavePath <- paste0(mainDir, "/GBMModel_", Sys.Date(), "_ValEnd_", subDir, "_NFailCutoff_", Nfailcutoff, "_useRFE_", rfe)
  NHPPDir <- file.path(mainDir, "NHPP")

  list(
    cohorts = cohorts,
    asset_data = asset_data,
    imported_data = importedData,
    val_start = val_start,
    val_end = val_end,
    test_start = test_start,
    test_end = test_end,
    outages = FALSE,
    maxN = maxN,
    minN = minN,
    Nfailcutoff = Nfailcutoff,
    updateData = updateData,
    forceUpdate = forceUpdate,
    subDir = subDir,
    savePaths <- list(
      path4 = path4,
      path1 = path1,
      path2 = path2,
      modelSavePath = modelSavePath,
      mainDir = mainDir,
      NHPPDir = NHPPDir
    )
  )
}


#' SOB Model Train
#'
#' Main Function to Train a new SOB model
#'
#' @param workorder_data dataframe
#' @param asset_data dataframe
#' @param soil_data dataframe
#' @param SOB_data dataframe
#' @param predictors character
#' @param val_start date
#' @param val_end date
#' @param test_start date
#' @param test_end date
#' @param Nfailcutoff integer
#' @param outages logical
#' @param mainDir character
#' @param savePaths character
#'
#' @return  returns a list of dataframe outputs from Cohort, SOB and SOB modelling
#'
#' @export
#' @examples \dontrun{
#' SOBmodelTrain <- function(workorder_data, asset_data, soil_data, SOB_data,predictors, val_start,  val_end, test_start, test_end, Nfailcutoff,
#' outages = FALSE, mainDir, savePaths)
#' }
SOBmodelTrain <- function(workorder_data, asset_data, soil_data, SOB_data,predictors, val_start,  val_end, test_start, test_end, Nfailcutoff,
                          outages = FALSE, mainDir, modelUpdate, rmOutlier, forceUpdate, forcePreProcess, rfe, CVfolds,
                          Trainsplit) {

  dir.create(mainDir, showWarnings = FALSE)

  subDir <- as.character(lubridate::year(val_end))
  CohortWOs_path <- file.path(mainDir, paste0("NHPP_cohort_input_", subDir, ".RDS"))
  CohortTable_path <- file.path(mainDir, paste0("NHPP_cohort_outout_", subDir, ".RDS"))
  SOBInput_path <- file.path(mainDir, paste0("NHPP_SOB_input_", subDir, ".RDS"))
  SOBOutput_path <- file.path(mainDir, paste0("NHPP_SOB_outout_", subDir, ".RDS"))
  ModelInput_path <- file.path(mainDir, paste0("GBM_model_input_", subDir, ".RDS"))
  ModelOutput_path <- file.path(mainDir, paste0("GBM_model_output_", subDir, ".RDS"))
  modelSavePath <- paste0(mainDir, "/GBMModel_", Sys.Date(), "_ValEnd_", subDir, "_NFailCutoff_", Nfailcutoff, "_useRFE_", rfe)


  if(!file.exists(CohortWOs_path)|forceUpdate==TRUE){
  NHPP_loadDatacohort(
    work_order = workorder_data,
    outages = FALSE,
    maxN = 10000,
    minN = 5,
    val_start = val_start,
    val_end = val_end,
    test_start = test_start,
    test_end = test_end
  ) -> CohortInput
  saveRDS(CohortInput, CohortWOs_path)
  } else {readRDS(CohortWOs_path) -> CohortInput}


  if(!file.exists(CohortTable_path)|forceUpdate==TRUE){

  i <- 0

  startcohort <- i + 1

  out <- list()
  out2 <- list()
  out3 <- list()
  out4 <- list()

  #for (i in c(startcohort:length(cohort_IDs))) {
    for (i in c(startcohort:length(cohort_IDs))) {
    print(paste0("running ", i))
    suppressWarnings(try(NHPP_fit_cohort_update(asset_data = asset_data, work_Order_Data = CohortInput, cohortNr = i, TI1 = Ndays, TI2 = Ndays + (valyears * 365), FNRlow = 10, FNRupp = 250, plot = FALSE, minpkh = 5, rollingwin = 90, inclsoilmoist = FALSE))) -> temp

    as.data.frame(temp$parameters$cohort) -> out[[i]]
    temp$timeseries -> out2[[i]]
    temp$timeseries$WorkType -> out4[[i]]
    temp$clusteredFails -> out3[[i]]
  }

  do.call(rbind, out) -> Cohort_table_results

  B <- Cohort_table_results$PowerLaw1.beta
  a <- Cohort_table_results$PowerLaw1.eta

  Cohort_table_results$intensity <- (B / a) * (T / a)^(B - 1)
  saveRDS(Cohort_table_results, CohortTable_path)
  } else {readRDS(CohortTable_path) -> Cohort_table_results}


  if(!file.exists(SOBInput_path)|forceUpdate==TRUE){
  SOBNHPP_load(
    mainOnly = FALSE,
    outages = FALSE,
    maxNinSOB = 500,
    minNinSOB = 5,
    val_start = val_start,
    val_end = val_end,
    test_start = test_start,
    test_end = test_end,
    SOB_data = SOB_data,
    cohortResults = Cohort_table_results,
    work_Order_Data = workorder_data,
    asset_data = asset_data
  ) -> NHPPinput
    saveRDS(NHPPinput, SOBInput_path)
  }else{ readRDS(SOBInput_path) -> NHPPinput}


  if(!file.exists(SOBOutput_path)|forceUpdate==TRUE){
  i <- 0

  out <- list()
  out2 <- list()
  out3 <- list()
  out4 <- list()

  startSOB <- i + 1

  ### replacement with tryCatch to hopefully fix this loop failing

  for (i in startSOB:NSOBS) {
    print(paste0("running SOB ID ", i, " of ", length(all_SOBs)))

    tryCatch(
      {
        suppressWarnings(NHPP_fit(
          minNinSOB = 5,
          asset_data = asset_data,
          soil_data=soil_data,
          work_Order_Data = NHPPinput, # note: check this is the right one to use
          SOBNr = i,
          TI1 = Ndays,
          TI2 = Ndays + (valyears * 365),
          FNRlow = 1,
          FNRupp = 25,
          plot = FALSE,
          minpkh = 2,
          rollingwin = 90,
          inclsoilmoist = FALSE
        )) -> temp
      },
      error = function(e) {
        cat("ERROR :", conditionMessage(e), "\n")
      }
    )

    if (!inherits(temp, "error")) {

      as.data.frame(temp$parameters$SOB) -> out[[i]]
      temp$timeseries -> out2[[i]]
      temp$timeseries$WorkType -> out4[[i]]
      temp$clusteredFails -> out3[[i]]
    } else {
      out[[i]] <- NULL
      NULL -> out2[[i]]
      NULL -> out4[[i]]
      NULL -> out3[[i]]
    }
  }

  do.call(rbind, out) -> SOB_table_results
  saveRDS(SOB_table_results, SOBOutput_path)
  } else{ readRDS(SOBOutput_path) ->SOB_table_results}

  # model

  if(!file.exists(ModelInput_path)|forcePreProcess==TRUE){
  modelPreProc(
    combined.df = SOB_table_results,
    predictors = predictors,
    classification = TRUE,
    Nfailcutoff = Nfailcutoff,
    outlierRemove = rmOutlier
  ) -> DF
  saveRDS(DF, ModelInput_path)
} else{readRDS(ModelInput_path)->DF}

  DF$data[[1]] -> trainData
  DF$data[[2]] -> valData
  DF$data[[3]] -> SOBIDsTrain
  DF$data[[4]] -> SOBIDsTest

if(modelUpdate==TRUE){
  GBMstackedensemble(
    classification = TRUE,
    dftrain = trainData,
    dftest = valData,
    upsample = TRUE,
    SMOTE = FALSE,
    nfolds=CVfolds,
    R=Trainsplit,
    path = modelSavePath
  ) -> ModelOut


  saveRDS(ModelOut, ModelOutput_path)

} else{ModelOut =readRDS(ModelOutput_path)
       }
  list(CohortWOs=CohortInput, CohortTable = Cohort_table_results, SOBInput = NHPPinput, SOBOutput = SOB_table_results, ModelInput = DF, ModelOutput = ModelOut) -> resultList
  return(resultList)
}

#' SOB Model Predict
#'
#' Uses New data to make SOB predictions
#'
#' @param workorder_data dataframe
#' @param asset_data dataframe
#' @param SOB_data dataframe
#' @param soil_data dataframe
#' @param val_start date
#' @param val_end date
#' @param test_start date
#' @param test_end date
#' @param outages logical
#' @param predictors character
#' @param ModelObj character
#'
#' @return List of predictions and outputs from SOB modelling
#'
#' @export
#' @examples \dontrun{
#' SOBmodelPredict <- function(workorder_data, asset_data, SOB_data, soil_data, val_start,  val_end, test_start, test_end,
#' outages = FALSE, predictors, ModelObj)
#' }
SOBmodelPredict <- function(workorder_data, asset_data, SOB_data, soil_data, val_start,  val_end, test_start, test_end,
                            outages = FALSE, predictors, ModelObj, mainDir, ModelPath, forcePreProcess, forceUpdate, rmOutlier) {
  environment()->Env
  maxN<-10000
  minN<-5

  dir.create(mainDir, showWarnings = FALSE)

  subDir <- as.character(lubridate::year(val_end))
  CohortWOs_path <- file.path(mainDir, paste0("NHPP_cohort_input_", subDir, ".RDS"))
  CohortTable_path <- file.path(mainDir, paste0("NHPP_cohort_outout_", subDir, ".RDS"))
  SOBInput_path <- file.path(mainDir, paste0("NHPP_SOB_input_", subDir, ".RDS"))
  SOBOutput_path <- file.path(mainDir, paste0("NHPP_SOB_outout_", subDir, ".RDS"))
  ModelInput_path <- file.path(mainDir, paste0("GBM_model_input_", subDir, ".RDS"))

  Nfailcutoff<-ModelObj$ModelInput$param[[1]]
  ModelObj$ModelInput$Preprocess -> trained_rec

  if(!file.exists(CohortWOs_path)|forceUpdate==TRUE){

  NHPP_loadDatacohort(
    work_order = workorder_data,
    outages = FALSE,
    maxN = maxN,
    minN = minN,
    val_start = val_start,
    val_end = val_end,
    test_start = test_start,
    test_end = test_end
  ) -> CohortInput
    saveRDS(CohortInput, CohortWOs_path)
} else( readRDS(CohortWOs_path)->CohortInput)

  ###

  if(!file.exists(CohortTable_path)|forceUpdate==TRUE){
    # Run on all Cohort IDs
  i <- 0

  startcohort <- i + 1

  out <- list()
  out2 <- list()
  out3 <- list()
  out4 <- list()

  for (i in c(startcohort:length(cohort_IDs))) {
    print(paste0("running ", i))
    suppressWarnings(try(NHPP_fit_cohort_update(asset_data = asset_data, work_Order_Data = CohortInput, cohortNr = i, TI1 = Ndays, TI2 = Ndays + (valyears * 365), FNRlow = 10, FNRupp = 250, plot = FALSE, minpkh = 5, rollingwin = 90, inclsoilmoist = FALSE))) -> temp

    as.data.frame(temp$parameters$cohort) -> out[[i]]
    temp$timeseries -> out2[[i]]
    temp$timeseries$WorkType -> out4[[i]]
    temp$clusteredFails -> out3[[i]]
  }

  do.call(rbind, out) -> Cohort_table_results

  B <- Cohort_table_results$PowerLaw1.beta
  a <- Cohort_table_results$PowerLaw1.eta

  Cohort_table_results$intensity <- (B / a) * (T / a)^(B - 1)
  saveRDS(Cohort_table_results, CohortTable_path)
} else {readRDS(CohortTable_path)-> Cohort_table_results}
  ###

  if(!file.exists(SOBInput_path)|forceUpdate==TRUE){
  SOBNHPP_load(
    mainOnly = FALSE,
    outages = FALSE,
    maxNinSOB = 500,
    minNinSOB = 0,
    val_start = val_start,
    val_end = val_end,
    test_start = test_start,
    test_end = test_end,
    SOB_data = SOB_data,
    cohortResults = Cohort_table_results,
    work_Order_Data = workorder_data,
    asset_data = asset_data
  ) -> NHPPinput

    saveRDS(NHPPinput, SOBInput_path)
  } else{readRDS(SOBInput_path) -> NHPPinput}
    ## Delete file if it exists

  if(!file.exists(SOBOutput_path)|forceUpdate==TRUE){

  i <- 0

  out <- list()
  out2 <- list()
  out3 <- list()
  out4 <- list()

  startSOB <- i + 1

  ### replacement with tryCatch to hopefully fix this loop failing

  for (i in startSOB:length(all_SOBs)) {
    print(paste0("running SOB ID ", i, " of ", length(all_SOBs)))

    tryCatch(
      {
          suppressWarnings(NHPP_fit(
            minNinSOB = 5,
            asset_data = asset_data,
            work_Order_Data = NHPPinput,
            soil_data=soil_data,
            SOBNr = i,
            TI1 = Ndays,
            TI2 = Ndays + (valyears * 365),
            FNRlow = 1,
            FNRupp = 25,
            plot = FALSE,
            minpkh = 2,
            rollingwin = 90,
            inclsoilmoist = FALSE
          )) -> temp
        },
        error = function(e) {
          cat("ERROR :", conditionMessage(e), "\n")
        }
      )

      if (!inherits(temp, "error")) {
        as.data.frame(temp$parameters$SOB) -> out[[i]]
        temp$timeseries -> out2[[i]]
        temp$timeseries$WorkType -> out4[[i]]
        temp$clusteredFails -> out3[[i]]
      } else {
        out[[i]] <- NULL
        NULL -> out2[[i]]
        NULL -> out4[[i]]
        NULL -> out3[[i]]
      }
    }

  do.call(rbind, out) -> SOB_table_results
  saveRDS(SOB_table_results, SOBOutput_path)} else{readRDS(SOBOutput_path)->SOB_table_results}

  ###  Step4 - Note that accuracy and CM reporting only possible when running a full back test,  i.e where the second period is in the past.

  if(!file.exists(ModelInput_path)|forcePreProcess==TRUE)
 {modelPreProc_update(
      combined.df = SOB_table_results,
      predictors = predictors,
      classification = TRUE,
      Nfailcutoff = Nfailcutoff,
      outlierRemove = rmOutlier,
      trained_rec = trained_rec

    ) -> DF
  saveRDS(DF, ModelInput_path) } else{readRDS(ModelInput_path)->DF }

  DF$data[[1]] -> trainData
  DF$data[[2]] -> valData
  DF$data[[3]] -> SOBIDsTrain
  DF$data[[4]] -> SOBIDsTest

  #####  Prediction on next 12 month (as would be done in practice)
  as.data.frame(SOBIDsTrain) -> SOBIDsTrain
  as.data.frame(SOBIDsTest) -> SOBIDsTest

  rbind(trainData, valData) -> newData

  # delete this line once models retrained with Greater_Than_Equal and Less_Than, to avoid confusion with different N
  # levels(newData$ActualFail) <-c("Greater_than_Two","Zero_to_Two")

  colnames(SOBIDsTest) <- colnames(SOBIDsTrain)
  rbind(SOBIDsTrain, SOBIDsTest) -> newSOBIDS

  h2o::h2o.init()
  h2o::h2o.removeAll() ## clean slate - just in case the cluster was already running

  ModelData <- h2o::as.h2o(newData)

  h2o::h2o.loadModel(ModelPath) -> GBMModel  #path was removed from modelTrain output list

  GBMModel@parameters$x -> ModelFeatures
  pred <- h2o::h2o.predict(GBMModel, ModelData)

  as.data.frame(pred) -> pred

  #h2o::h2o.confusionMatrix(GBMModel, newdata = ModelData, metric = "f1") -> CM

  h2o::h2o.performance(GBMModel, newdata = ModelData) -> performance
  performance@metrics$max_criteria_and_metric_scores %>%
    dplyr::filter(metric == "max f2") %>%
    dplyr::select(threshold) -> maxf2_Th
  performance@metrics$max_criteria_and_metric_scores %>%
    dplyr::filter(metric == "max f0point5") %>%
    dplyr::select(threshold) -> maxf0.5_Th
  performance@metrics$max_criteria_and_metric_scores %>%
    dplyr::filter(metric == "max f1") %>%
    dplyr::select(threshold) -> maxf1_Th

  (maxf2_Th+ maxf1_Th + maxf0.5_Th) / 3 -> Th
   #maxf1_Th -> Th

 # Predicted class in predict not equal to the results in the confusion matrix using same ModelData.  #need to manually update predictions using a threshold.
  # using an average of threshold for maximum f1 and f0.5.

  #
  newPosClassLabel <- paste0(Nfailcutoff, " or more")
  newNegClassLabel <- paste0("Less than ", Nfailcutoff)

  as.character(pred[, 1]) -> pred[, 1]
  pred[, 1][grep("Less", pred[, 1])] <- newNegClassLabel # replace with Less
  pred[, 1][grep("Greater", pred[, 1])] <- newPosClassLabel

  SOB_table_results %>% dplyr::select(SOB, ActualPipeFailVal, ActualFailVal) -> temLfJoin

  cbind(newSOBIDS[, 1], pred) -> predDF
  as.data.frame(predDF) -> predDF
  colnames(predDF) <- c("SOB", "Prediction", "Prob Negative Outcome", "Prob Positive Outcome")

  dplyr::left_join(temLfJoin, predDF, by = "SOB") -> predDF

  ifelse(predDF$ActualFailVal >= Nfailcutoff, newPosClassLabel, newNegClassLabel) -> TrueLabel

  predDF$TrueOutcome <- TrueLabel

  predDF$TrueOutcome <- as.factor(predDF$TrueOutcome)
  predDF$Prediction <- as.factor(predDF$Prediction)

  as.numeric(predDF$`Prob Positive Outcome`) -> Predictions
  predDF$TrueOutcome -> Labels

  # Summary of Outcomes for the Test Year..

  factor(Labels, levels= c(newNegClassLabel,newPosClassLabel)) -> Labels

  roc2 <- pROC::roc(
    response = Labels,
    predictor = Predictions, percent = TRUE,
    # arguments for auc
    # arguments for ci
    ci = TRUE, boot.n = 100, ci.alpha = 0.6,
    # arguments for plot
    plot = TRUE, auc.polygon = FALSE, max.auc.polygon = FALSE, grid = FALSE,
    print.auc = TRUE, show.thres = TRUE
  )

  pROC::coords(roc2, "best", ret = c("threshold", "specificity", "1-npv"), transpose = FALSE) -> Metrs
  pROC::coords(roc2, "best", ret = "threshold", transpose = FALSE, best.method = "youden", best.weights = c(0.5, 0.1)) -> THold
  pROC::coords(roc2, "best", ret = "threshold", transpose = FALSE, best.method = "closest.topleft", best.weights = c(1, 0.1)) -> THold
  pROC::coords(roc2, "local maximas", ret = c("threshold", "sens", "spec", "ppv", "npv"), transpose = FALSE) -> ResultTable

  #Metrs[[1]]-> Thr
  #0.98-> Thr
  Th[, ] -> Thr
  final_preds <- ifelse(Predictions >= Thr, newPosClassLabel, newNegClassLabel)

  as.factor(final_preds) -> final_preds
  factor(final_preds, levels= c(newNegClassLabel,newPosClassLabel)) -> final_preds
  caret::confusionMatrix(final_preds, Labels, positive = levels(Labels)[2]) -> CM

  predDF$Prediction <- final_preds

 list(predDF=predDF, Predictions=Predictions, Labels=Labels, CohortWOs=CohortInput, CohortTable = Cohort_table_results, SOBInput = NHPPinput, SOBOutput = SOB_table_results, CM=CM, f1=  CM$byClass[7], Threshold=Thr, roc=roc2) -> resultList
  return(resultList)
}


ModPred<-function(Predictions, Labels, predDF, Thr, newPosClassLabel, newNegClassLabel){

  final_preds <- ifelse(Predictions >= Thr, newPosClassLabel, newNegClassLabel)
  as.factor(final_preds) -> final_preds
  factor(final_preds, levels= c(newNegClassLabel,newPosClassLabel)) -> final_preds
  caret::confusionMatrix(final_preds, Labels, positive = levels(Labels)[2]) -> CM

  predDF$Prediction <- final_preds

  list(CM, predDF)->out
  return(out)

}

VarImp<-function(ModelPath){
  h2o::h2o.loadModel(ModelPath) -> GBMModel
  h2o::h2o.varimp_plot(GBMModel, 10)
}
