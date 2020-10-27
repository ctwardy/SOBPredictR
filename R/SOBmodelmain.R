
#' Title
#'
#' @param cohorts
#' @param asset_data
#' @param imported_data
#' @param val_start
#' @param val_end
#' @param test_start
#' @param test_end
#' @param outages
#' @param maxN
#' @param minN
#' @param Nfailcutoff
#' @param rfe
#' @param updateData
#' @param forceUpdate
#' @param newDir
#' @param mainDir
#'
#' @return
#' @export
#'
#' @examples
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


SOBmodelTrain <- function(workorder_data, asset_data, soil_data, SOB_data,predictors, val_start,  val_end, test_start, test_end, Nfailcutoff,
                          outages = FALSE, mainDir, savePaths) {
  savePaths[[1]] -> path4
  savePaths[[2]] -> path1
  savePaths[[3]] -> path2
  savePaths[[4]] -> modelSavePath
  savePaths[[5]] -> mainDir
  savePaths[[6]] -> NHPPDir

  dir.create(mainDir, showWarnings = FALSE)

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


  i <- 0

  out <- list()
  out2 <- list()
  out3 <- list()
  out4 <- list()

  startSOB <- i + 1

  ### replacement with tryCatch to hopefully fix this loop failing

  for (i in startSOB:length(SB_IDs)) {
    print(paste0("running SOB ID ", i, " of ", length(SB_IDs)))

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


  # model

  modelPreProc(
    combined.df = SOB_table_results,
    predictors = predictors,
    classification = TRUE,
    Nfailcutoff = Nfailcutoff,
    outlierRemove = FALSE
  ) -> DF

  DF$data[[1]] -> trainData
  DF$data[[2]] -> valData
  DF$data[[3]] -> SOBIDsTrain
  DF$data[[4]] -> SOBIDsTest

  GBMstackedensemble(
    classification = TRUE,
    dftrain = trainData,
    dftest = valData,
    upsample = TRUE,
    SMOTE = FALSE,
    path = modelSavePath
  ) -> ModelOut

  list(CohortWOs=CohortInput, CohortTable = Cohort_table_results, SOBInput = NHPPinput, SOBOutput = SOB_table_results, ModelInput = DF, ModelOutput = ModelOut) -> resultList
  return(resultList)
}


#' Title
#'
#' @param workorder_data
#' @param asset_data
#' @param soil_data
#' @param val_start
#' @param predictors
#' @param val_end
#' @param test_start
#' @param test_end
#' @param outages
#' @param maxN
#' @param minN
#' @param rfe
#' @param mainDir
#' @param updateData
#' @param forceUpdate
#' @param savePaths
#'
#' @return
#' @export
#'
#' @examples
SOBmodelPredict <- function(workorder_data, asset_data, SOB_data, soil_data, val_start,  val_end, test_start, test_end,
                            outages = FALSE, predictors, ModelObj) {

  maxN<-10000
  minN<-5
  ModelObj$ModelOutput[[8]] ->ModelPath
  Nfailcutoff<-ModelObj$ModelInput$param[[1]]
  ModelObj$ModelInput$Preprocess -> trained_rec

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

    ###

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

  ###

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

    ## Delete file if it exists

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

  ###  Step4 - Note that accuracy and CM reporting only possible when running a full back test,  i.e where the second period is in the past.

    modelPreProc_update(
      combined.df = SOB_table_results,
      predictors = predictors,
      classification = TRUE,
      Nfailcutoff = Nfailcutoff,
      outlierRemove = FALSE,
      trained_rec = trained_rec

    ) -> DF

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

  h2o::h2o.loadModel(ModelPath) -> GBMModel
  GBMModel@parameters$x -> ModelFeatures
  pred <- h2o::h2o.predict(GBMModel, ModelData)

  as.data.frame(pred) -> pred

  h2o::h2o.confusionMatrix(GBMModel, newdata = ModelData, metric = "f1") -> CM
  print(CM)
  as.data.frame(CM)


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

  (maxf1_Th + maxf0.5_Th) / 2 -> Th

  # Predicted class in predict not equal to the results in the confusion matrix using same ModelData.  #need to manually update predictions using a threshold.
  # using an average of threshold for maximum f1 and f0.5.

  #
  newPosClassLabel <- paste0(Nfailcutoff, " or more")
  newNegClassLabel <- paste0("Less than ", Nfailcutoff)

  as.character(pred[, 1]) -> pred[, 1]
  pred[, 1][grep("Less", pred[, 1])] <- newNegClassLabel # replace with Less
  pred[, 1][grep("Greater", pred[, 1])] <- newPosClassLabel

  SOB_table_results %>% dplyr::select(SOB, ActualPipeFailVal, ActualFailVal) -> temLfJoin

  nrow(predDF)
  cbind(newSOBIDS[, 1], pred) -> predDF
  as.data.frame(predDF) -> predDF
  colnames(predDF) <- c("SOB", "Prediction", "Prob Negative Outcome", "Prob Positive Outcome")

  dplyr::left_join(temLfJoin, predDF, by = "SOB") -> predDF
  nrow(predDF)
  ifelse(predDF$ActualFailVal >= Nfailcutoff, newPosClassLabel, newNegClassLabel) -> TrueLabel

  predDF$TrueOutcome <- TrueLabel

  predDF$TrueOutcome <- as.factor(predDF$TrueOutcome)
  predDF$Prediction <- as.factor(predDF$Prediction)

  print(table(predDF$Prediction))
  print(table(predDF$TrueOutcome))

  levels(predDF$Prediction)
  levels(predDF$TrueOutcome)

  as.numeric(predDF$`Prob Positive Outcome`) -> Predictions
  predDF$TrueOutcome -> Labels

  # Summary of Outcomes for the Test Year..

  # factor(Labels, levels= c(newNegClassLabel,newPosClassLabel)) -> Labels

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

  Th[, ] -> Thr
  final_preds <- ifelse(Predictions > Thr, newPosClassLabel, newNegClassLabel)

  as.factor(final_preds) -> final_preds
  print(table(final_preds))
  print(table(Labels))

  caret::confusionMatrix(final_preds, Labels, positive = levels(Labels)[2]) -> CM
  print(CM)
  CM$byClass[7]

  predDF$Prediction <- final_preds

  return(predDF)
}


#' Title
#'
#' @param Fn
#' @param argF
#'
#' @return
#' @export
#'
#' @examples
RunSOBTrain <- function(Fn = SOBmodelTrain, argF) {
  rlang::exec(Fn, !!!argF)->out
  return(out)
}
