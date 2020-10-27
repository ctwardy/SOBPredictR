#' Model PreProcess
#'
#' PreProcess data for training a new SOB model
#'
#' @param combined.df dataframe
#' @param predictors character
#' @param classification logical
#' @param Nfailcutoff integer
#' @param outlierRemove logical
#'
#' @return
#'
#' @examples \dontrun{
#' modelPreProc<- function(combined.df, predictors, classification, Nfailcutoff,  outlierRemove)
#' }
modelPreProc<- function(combined.df, predictors, classification, Nfailcutoff,  outlierRemove) {

  #This analyses sOB data and NHPP parameters

  data.frame(lapply(combined.df, function(x) unlist(x)))->combined.df
  combined.df %>% dplyr::mutate(Avg.Age=as.numeric(lubridate::year(Sys.Date()) - AvgAge.FUN))->combined.df

  colnames(combined.df)[4]<-"Future_Number_365"
  colnames(combined.df)[16]<-"Avg.Length"
  colnames(combined.df)[17]<-"Tot.Length"
  colnames(combined.df)[18]<-"Avg.diameter"
  colnames(combined.df)[20]<-"N.assets"
  colnames(combined.df)[21]<-"FailCause"
  colnames(combined.df)[22]<-"Pipe.Material"
  colnames(combined.df)[32]<-"Last.Date"
  colnames(combined.df)[37]<-"N_WOs"
  colnames(combined.df)[39]<-"Nrepeats"

  colnames(combined.df)->all_vars

  combined.df$LengthAC[is.na(combined.df$LengthAC)]<-0
  combined.df$LengthCI[is.na(combined.df$LengthCI)]<-0
  combined.df$Nrepeats[is.na(combined.df$Nrepeats)]<-0
  combined.df$NSLIDs[is.infinite(combined.df$NSLIDs)]<-mean(combined.df$NSLIDs, na.rm=TRUE)
  combined.df$NSLIDs[is.na(combined.df$NSLIDs)]<-mean(combined.df$NSLIDs, na.rm=TRUE)

  combined.df$MaxCohortBeta[is.infinite(combined.df$MaxCohortBeta)]<-0
  combined.df$MinCohortEta[is.infinite(combined.df$MinCohortEta )]<-0
  combined.df$maxcohortFNR[is.infinite(combined.df$maxcohortFNR )]<-0
  combined.df$maxcohortInten[is.infinite(combined.df$maxcohortInten  )]<-0

  predictors<-c(predictors, "assetSpread", "WDZ")  #add two new??

  combined.df %>% dplyr::select(predictors)->combined.df2
  #make.names(levels(combined.df2$Pipe.Material))->levels(combined.df2$Pipe.Material)

  #lot of missing SOB eta and Beta,  lets impute the missing values using a random forest model

    do.call(data.frame,lapply(combined.df2, function(x) replace(x, is.infinite(x),NA))) ->combined.df2

    #convert characters to factors
    combined.df2[sapply(combined.df2, is.character)] <- lapply(combined.df2[sapply(combined.df2, is.character)],
                                                               as.factor)

    #Pipe Material is unknown,  make NA
    combined.df2$Pipe.Material[combined.df2$Pipe.Material =="NA"]<-NA

    #impute pipe material  Still required???
    missRanger::missRanger(combined.df2, Pipe.Material ~ ., pmm.k = 5,  num.trees = 100) -> combined.df2

    #skim(combined.df2)
    ImpDFs<-replicate(20, {missRanger::missRanger(combined.df2, pmm.k = 5,  num.trees = 100) ->X
      X %>% dplyr::select(PowerLaw1, PowerLaw2, AvgAge.FUN)}, simplify=FALSE)


    all.matrix <- abind::abind(ImpDFs, along=3)
    apply(all.matrix, c(1,2), mean) -> imputedValues

    combined.df2$AvgAge.FUN <-round(imputedValues[,3],0)
    combined.df2$PowerLaw1 <-imputedValues[,1]
    combined.df2$PowerLaw2 <-imputedValues[,2]

  combined.df2[stats::complete.cases(combined.df2),]->combined.df3  #remove NAs
  droplevels(combined.df3)->combined.df3

  colnames(combined.df3)->parameters

  combined.df3[stats::complete.cases(combined.df3),]->combined.df3

  #remove outliers from SOM  #dont run as was removing actual fails

  OutD <-  function(x){
    qnt <- stats::quantile(x, probs=c(.25, .75), na.rm = T)
    caps <- stats::quantile(x, probs=c(.05, .95), na.rm = T)
    H <- 5 * stats::IQR(x, na.rm = T)
    x[x < (qnt[1] - H)]   <- caps[1]
    x[x>(qnt[2] + H)] <- caps[2]
    return(x)
  }

  combined.df3[ , purrr::map_lgl(combined.df3, is.numeric)]->combined.df3n
  if(outlierRemove){lapply(combined.df3n[,-(1:2)], OutD)->combined.df3n[,-(1:2)]    #care with this as removing all of the >2 SOBs.
  }

  as.data.frame(combined.df3n)->combined.df3nb

  combined.df3[ , purrr::map_lgl(combined.df3, is.factor),drop=FALSE]->H
  combined.df3[ , purrr::map_lgl(combined.df3, is.character),drop=FALSE]->H2
  cbind(H,H2)->H
  ###colnames(H)<-c("Pipe.Material")

  cbind(combined.df3nb, H)->combined.df3

  #Split SOM_train and SOM_test??
  y_name<<-"ActualFailTest"

  trainIndex <- caret::createDataPartition(combined.df3[,y_name], p = .8,
                                    list = F,
                                    times = 1)
  Train <- combined.df3[trainIndex,]
  Test  <- combined.df3[-trainIndex,]

  Test$SOB->TestSOBs
  Train$SOB->TrainSOBs
  Train$SOB<-NULL
  Test$SOB<-NULL
  preproc <- recipes::recipe(ActualFailTest ~ ., data = Train) %>%
    recipes::step_other(recipes::all_nominal(), threshold = .01, other = "other values") %>%
    recipes::step_dummy(recipes::all_nominal(), one_hot=TRUE)  %>%
    recipes::step_center(recipes::all_predictors(), -dplyr::contains("Pipe.Material")) %>%
    recipes::step_scale(recipes::all_predictors(), -dplyr::contains("Pipe.Material"))

  recipes::prep(preproc, training = Train)->trained_rec

  dTrainTreated <- recipes::bake(trained_rec, new_data = Train)
  dTestTreated  <- recipes::bake(trained_rec, new_data = Test)

  nrow(dTrainTreated)->r

  maxna<-0.1
  dTrainTreated[ , colSums(is.na(dTrainTreated)) <= maxna*r]->dTrainTreated
  dTestTreated[ , colSums(is.na(dTestTreated)) <= maxna*r]->dTestTreated

  #convert treated dataframes to data.frame
  as.data.frame(dTrainTreated)->data_frame_train

  as.data.frame(dTestTreated)->data_frame_test

  #Choose Predictors, we drop a and b since these are used to calculate the future number of reoccurence
  colnames(data_frame_train)->predictors
  predictors[!predictors %in% y_name]->predictors

  ##Drop eta and beta?
  #predictors[!predictors %in% "PowerLaw1"]->predictors
  #predictors[!predictors %in% "PowerLaw2"]->predictors

  #training data
  data_frame_train$ActualFailTest ->y
  data_frame_train[,predictors]->x

  #testing data

  data_frame_test$ActualFailTest ->y_test
  data_frame_test[,predictors]->x_test

  #Classification,  Given the same model inputs can we correctly classify a SOB as either having Zero or 1, or Greater than 1 interuptions?

  ### user-specified (with labels)
  data.frame(ActualFail=y,x)->dftrain
  data.frame(ActualFail=y_test,x_test)->dftest

  if(classification){
    arules::discretize(dftrain$ActualFail, method = "fixed", breaks = c(-Inf, Nfailcutoff, Inf),
                       labels = c("Less_Than", "Greater_or_Equal"))->classTarget

    arules::discretize(dftest$ActualFail, method = "fixed", breaks = c(-Inf, Nfailcutoff, Inf),
                       labels = c("Less_Than", "Greater_or_Equal"))->classTargetTest

    table(classTarget) ->bins
    table(classTargetTest)->binTest
    #hist(dftrain$ActualFail, breaks = 20, main = "Fixed")
    #abline(v = arules::discretize(y, method = "fixed", breaks = c(-Inf, Nfailcutoff, Inf),
    #                            onlycuts = TRUE), col = "red")

    # hist(dftest$ActualFail, breaks = 20, main = "Fixed")
    #abline(v = arules::discretize(y, method = "fixed", breaks = c(-Inf, Nfailcutoff, Inf),
    #                          onlycuts = TRUE), col = "red")

    make.names(classTarget)->classTarget
    make.names(classTargetTest)->classTargetTest

    dftrain$ActualFail<- factor(classTarget)
    dftest$ActualFail<-factor(classTargetTest)

    sizes <- factor(dftrain$ActualFail, levels = c("Less_Than", "Greater_or_Equal"))
    sizes <- factor(dftest$ActualFail, levels =c("Less_Than", "Greater_or_Equal"))
  }

  dftrain %>% dplyr::select(ActualFail, predictors) -> dftrain
  dftest %>% dplyr::select(ActualFail, predictors) -> dftest

  list(data=list(dftrain,dftest, TrainSOBs, TestSOBs), param=list(Nfailcutoff, rfe, predictors), Preprocess=trained_rec)->output

  return(output)
}


#' Model PreProcess Update
#'
#' PreProcess new data for input to SOB model
#'
#' @param combined.df dataframe
#' @param predictors list
#' @param classification logical
#' @param Nfailcutoff integer
#' @param trained_rec character
#' @param outlierRemove logical
#'
#' @return list of dataframes which are used as inputs to Machine Learning Train
#'
#' @examples \dontrun{
#' modelPreProc_update<- function(combined.df, predictors, classification, Nfailcutoff, outlierRemove, trained_rec)
#' }
modelPreProc_update<- function(combined.df, predictors, classification, Nfailcutoff, outlierRemove, trained_rec) {

  #This analyses sOB data and NHPP parameters

  data.frame(lapply(combined.df, function(x) unlist(x)))->combined.df
  combined.df %>% dplyr::mutate(Avg.Age=as.numeric(lubridate::year(Sys.Date()) - AvgAge.FUN))->combined.df

  colnames(combined.df)[4]<-"Future_Number_365"
  colnames(combined.df)[16]<-"Avg.Length"
  colnames(combined.df)[17]<-"Tot.Length"
  colnames(combined.df)[18]<-"Avg.diameter"
  colnames(combined.df)[20]<-"N.assets"
  colnames(combined.df)[21]<-"FailCause"
  colnames(combined.df)[22]<-"Pipe.Material"
  colnames(combined.df)[32]<-"Last.Date"
  colnames(combined.df)[37]<-"N_WOs"
  colnames(combined.df)[39]<-"Nrepeats"

  colnames(combined.df)->all_vars

  #Missing Variables
  combined.df$LengthAC[is.na(combined.df$LengthAC)]<-0
  combined.df$LengthCI[is.na(combined.df$LengthCI)]<-0
  combined.df$Nrepeats[is.na(combined.df$Nrepeats)]<-0
  combined.df$NSLIDs[is.infinite(combined.df$NSLIDs)]<-mean(combined.df$NSLIDs, na.rm=TRUE)
  combined.df$NSLIDs[is.na(combined.df$NSLIDs)]<-mean(combined.df$NSLIDs, na.rm=TRUE)

  combined.df$MaxCohortBeta[is.infinite(combined.df$MaxCohortBeta)]<-0
  combined.df$MinCohortEta[is.infinite(combined.df$MinCohortEta )]<-0
  combined.df$maxcohortFNR[is.infinite(combined.df$maxcohortFNR )]<-0
  combined.df$maxcohortInten[is.infinite(combined.df$maxcohortInten  )]<-0

  summary(combined.df)
  colnames(combined.df)

  predictors<-c(predictors, "assetSpread", "WDZ")  #add two new??
  combined.df %>% dplyr::select(predictors)->combined.df2

  #lot of missing SOB eta and Beta,  lets impute the missing values using a random forest model

    do.call(data.frame,lapply(combined.df2, function(x) replace(x, is.infinite(x),NA))) ->combined.df2

    #convert characters to factors
    combined.df2[sapply(combined.df2, is.character)] <- lapply(combined.df2[sapply(combined.df2, is.character)],
                                                               as.factor)
    #Pipe Material is unknown,  make NA
    combined.df2$Pipe.Material[combined.df2$Pipe.Material =="NA"]<-NA

    #impute pipe material
    missRanger::missRanger(combined.df2, Pipe.Material ~ ., pmm.k = 5,  num.trees = 100) -> combined.df2

    ImpDFs<-replicate(20, {missRanger::missRanger(combined.df2, pmm.k = 5,  num.trees = 100) ->X
      X %>% dplyr::select(PowerLaw1, PowerLaw2, AvgAge.FUN)}, simplify=FALSE)

    all.matrix <- abind::abind(ImpDFs, along=3)
    apply(all.matrix, c(1,2), mean) -> imputedValues

    combined.df2$AvgAge.FUN <-round(imputedValues[,3],0)
    combined.df2$PowerLaw1 <-imputedValues[,1]
    combined.df2$PowerLaw2 <-imputedValues[,2]


  combined.df2[stats::complete.cases(combined.df2),]->combined.df3  #remove NAs
  droplevels(combined.df3)->combined.df3

  colnames(combined.df3)->parameters

  combined.df3[stats::complete.cases(combined.df3),]->combined.df3


  #remove outliers from SOM  #dont run as was removing actual fails

  OutD <-  function(x){
    qnt <- stats::quantile(x, probs=c(.25, .75), na.rm = T)
    caps <- stats::quantile(x, probs=c(.05, .95), na.rm = T)
    H <- 5 * stats::IQR(x, na.rm = T)
    x[x < (qnt[1] - H)]   <- caps[1]
    x[x>(qnt[2] + H)] <- caps[2]
    return(x)
  }

  combined.df3[ , purrr::map_lgl(combined.df3, is.numeric)]->combined.df3n
  if(outlierRemove){lapply(combined.df3n[,-(1:2)], OutD)->combined.df3n[,-(1:2)]    #care with this as removing all of the >2 SOBs.
  }

  as.data.frame(combined.df3n)->combined.df3nb

  combined.df3[ , purrr::map_lgl(combined.df3, is.factor),drop=FALSE]->H
  combined.df3[ , purrr::map_lgl(combined.df3, is.character),drop=FALSE]->H2
  cbind(H,H2)->H

  cbind(combined.df3nb, H)->combined.df3

  #Split SOM_train and SOM_test??
  y_name<<-"ActualFailTest"

  #changed this when only using for predictions, without a validation as Nactual fails is always zero in this case
  trainIndex <- caret::createDataPartition(combined.df3[,"PowerLaw1"], p = .8,
                                    list = F,
                                    times = 1)
Train <- combined.df3[trainIndex,]
Test  <- combined.df3[-trainIndex,]

  Test$SOB->TestSOBs
  Train$SOB->TrainSOBs
  Train$SOB<-NULL
  Test$SOB<-NULL

  dTrainTreated <- recipes::bake(trained_rec, new_data = Train)
  dTestTreated  <- recipes::bake(trained_rec, new_data = Test)

  nrow(dTrainTreated)->r

  maxna<-0.1
  dTrainTreated[ , colSums(is.na(dTrainTreated)) <= maxna*r]->dTrainTreated
  dTestTreated[ , colSums(is.na(dTestTreated)) <= maxna*r]->dTestTreated


  #convert treated dataframes to data.frame
  as.data.frame(dTrainTreated)->data_frame_train
  as.data.frame(dTestTreated)->data_frame_test

  #Choose Predictors, we drop a and b since these are used to calculate the future number of reoccurence
  colnames(data_frame_train)->predictors
  predictors[!predictors %in% y_name]->predictors

  ##Drop eta and beta?
  #predictors[!predictors %in% "PowerLaw1"]->predictors
  #predictors[!predictors %in% "PowerLaw2"]->predictors

  #training data
  data_frame_train$ActualFailTest ->y
  data_frame_train[,predictors]->x

  #testing data

  data_frame_test$ActualFailTest ->y_test
  data_frame_test[,predictors]->x_test

  #Classification,  Given the same model inputs can we correctly classify a SOB as either having Zero or 1, or Greater than 1 interuptions?

  ### user-specified (with labels)
  data.frame(ActualFail=y,x)->dftrain
  data.frame(ActualFail=y_test,x_test)->dftest

  if(classification){
    arules::discretize(dftrain$ActualFail, method = "fixed", breaks = c(-Inf, Nfailcutoff, Inf),
                       labels = c("Less_Than", "Greater_or_Equal"))->classTarget
    table(classTarget)
    arules::discretize(dftest$ActualFail, method = "fixed", breaks = c(-Inf, Nfailcutoff, Inf),
                       labels = c("Less_Than", "Greater_or_Equal"))->classTargetTest

    table(classTarget) ->bins
    table(classTargetTest)->binTest

    make.names(classTarget)->classTarget
    make.names(classTargetTest)->classTargetTest

    dftrain$ActualFail<- factor(classTarget)
    dftest$ActualFail<-factor(classTargetTest)

    sizes <- factor(dftrain$ActualFail, levels = c("Less_Than", "Greater_or_Equal"))
    sizes <- factor(dftest$ActualFail, levels =c("Less_Than", "Greater_or_Equal"))
  }

  dftrain %>% dplyr::select(ActualFail, predictors) -> dftrain
  dftest %>% dplyr::select(ActualFail, predictors) -> dftest
  list(data=list(dftrain,dftest, TrainSOBs, TestSOBs), param=list(Nfailcutoff, predictors))->output
  return(output)
}

#' Trains a stacked ensemble model using H20
#'
#' @param classification logical
#' @param dftrain dataframe
#' @param dftest dataframe
#' @param upsample logical
#' @param SMOTE logical
#' @param path string
#'
#' @return list of objects including performance, confusion Matrices, finalModel, Hyperparameter grid, bestModelID and model saved path)
#'
#' @examples \dontrun{
#' GBMstackedensemble<-function(classification, dftrain, dftest, upsample, SMOTE, path)
#' }
GBMstackedensemble<-function(classification, dftrain, dftest, upsample, SMOTE, path){

  #use H20 package to build deep classifier
  ## R installation instructions are at http://h2o.ai/download
  h2o::h2o.init()
  h2o::h2o.removeAll() ## clean slate - just in case the cluster was already running

  if(upsample==TRUE)
  {caret::upSample(dftrain, dftrain$ActualFail, list=FALSE) ->dftrainUP
    dftrainUP$Class<-NULL
  }

  if(SMOTE==TRUE){
    dftrainUP <- UBL::SmoteClassif(ActualFail ~ ., dat  = dftrain, k=10, C.perc = "balance")
  }

  if(SMOTE==TRUE || upsample==TRUE) {dftrain.hex <- h2o::as.h2o(dftrainUP)
  } else {
    dftrain.hex <- h2o::as.h2o(dftrain)}

  splits <- h2o::h2o.splitFrame(dftrain.hex, ratios=0.75, seed=1234)
  train  <- h2o::h2o.assign(splits[[1]], "train.hex") # 60%
  valid  <- h2o::h2o.assign(splits[[2]], "valid.hex") # 20%

  dftest.hex <- h2o::as.h2o(dftest)
  if(classification){
    h2o::h2o.relevel(x = train["ActualFail"],y="Less_Than") ->train["ActualFail"]
    h2o::h2o.relevel(x = valid["ActualFail"],y="Less_Than") ->valid["ActualFail"]
  }

  response <- "ActualFail"
  predictors <- setdiff(names(dftrain.hex), response)

  nrow(train)
  nrow(valid)

  nfolds<-10  #test with small number,  use n=10 for production

  #Model 1 Grid search

  gbm_params1 <- list(learn_rate = c(0.01, 0.1, 0.5), #
                      max_depth = c(4,5,6,8),  #
                      sample_rate = c(0.8,1.0), #
                      ntrees = c(10,20,30),  #100,150,200,300,500
                      learn_rate_annealing = c(0.96,1), #
                      col_sample_rate = c(0.8,1.0)) #

  gbmmodel <- h2o::h2o.grid(
    algorithm="gbm",
    seed=1234,
    grid_id="gbm_grid",
    x=predictors,
    y=response,
    training_frame=train,
    validation_frame=valid,
    balance_classes =FALSE,
    keep_cross_validation_predictions = TRUE,
    nfolds = nfolds,
    fold_assignment = "Modulo",
    calibrate_model=FALSE,
    stopping_rounds = 3,        ##
    stopping_tolerance = 0.001,  ##
    score_each_iteration = TRUE,
    stopping_metric = "AUC",
    hyper_params=gbm_params1,
    search_criteria = list(strategy = "Cartesian")  #RandomDiscrete
  )

  ## Sort by f1

  if (classification) {grid <- h2o::h2o.getGrid("gbm_grid",sort_by="f1",decreasing=TRUE)
  } else {grid <- h2o::h2o.getGrid("gbm_grid",sort_by="rmse",decreasing=FALSE)}

  model <- h2o::h2o.getModel(grid@model_ids[[1]])

  if (classification){
    h2o::h2o.confusionMatrix(model, newdata = train, metrics="f1")
    h2o::h2o.confusionMatrix(model, newdata = dftest.hex, metrics="f1")
    h2o::h2o.confusionMatrix(model, newdata = valid, metrics="f1")
    perf1 <- h2o::h2o.performance(model, newdata = dftest.hex)
  }

  #Model 2 Grid search
  hyper_params2 <- list(
    max_depth = c(4,5,6,8), #
    sample_rate = c(0.8,1.0),#
    ntrees = c(10,20,50)  #110, 140, 170, 500
  )

  drfmodel <- h2o::h2o.grid(
    algorithm="drf",
    seed=1234,
    grid_id="drf_grid",
    #model_id="dl_model_tuned",
    x=predictors,
    y=response,
    training_frame=train,
    validation_frame=valid,
    keep_cross_validation_predictions = TRUE,
    nfolds=nfolds,
    fold_assignment = "Modulo",
    calibrate_model=FALSE,       ## use 70% of the columns to fit each tree
    stopping_rounds = 3,        ##
    stopping_tolerance = 0.001,  ##
    score_each_iteration = TRUE,   ##
    model_id = "drf_covType3",
    stopping_metric = "AUC",####
    hyper_params=hyper_params2,
    search_criteria = list(strategy = "Cartesian")  #RandomDiscrete
  )

  ## Sort by f1
  grid2 <- h2o::h2o.getGrid("drf_grid",sort_by="f1",decreasing=TRUE)
  model <- h2o::h2o.getModel(grid2@model_ids[[1]])
  perf1 <- h2o::h2o.performance(model, newdata = dftest.hex)


  #Model 3 Grid search
  hyper_params3 <- list(
    hidden=list(c(40),c(45,50),c(15,10,12),c(90,80)), #
    rate=c(0.0001,0.002,0.01),  #
    #rate_annealing=c(1e-8,1e-7,1e-6),
    #l1=c(0,0.001,0.00001),
    epochs =c(10, 50, 200, 1000)  #10, 50, 200, 1000,
  )


  #dlmodel <- h2o.deeplearning(
  dlmodel <- h2o::h2o.grid(
    algorithm="deeplearning",
    seed=1234,
    grid_id="dl_grid",
    x=predictors,
    y=response,
    training_frame=train,
    validation_frame=valid,
    keep_cross_validation_predictions=TRUE,
    activation="TanhWithDropout",  #"Tanh", "TanhWithDropout", "Rectifier", "RectifierWithDropout", "Maxout", "MaxoutWithDropout"
    loss = "Automatic",     #("Automatic", "CrossEntropy","Quadratic", "ModifiedHuber
    #score_validation_samples=1000,      ## sample the validation dataset (faster)
    overwrite_with_best_model=TRUE,
    stopping_rounds=3,
    stopping_metric="AUC", ## "AUTO", "deviance", "logloss", "MSE", "RMSE","MAE", "RMSLE", "AUC", "lift_top_group", "misclassification","mean_per_class_error", "custom", "custom_increasing")
    stopping_tolerance=0.001,
    nfolds=nfolds,
    fold_assignment="Modulo", # can be "AUTO", "Modulo", "Random" or "Stratified"
    balance_classes=FALSE,## enable this for high class imbalance
    adaptive_rate=FALSE,
    #rate=0.001,                    ## manually tuned learning rate
    #rate_annealing=2e-6,
    #momentum_start=0.2,             ## manually tuned momentum
    #momentum_stable=0.4,
    #momentum_ramp=1e7,
    l1=0.01,                        ## add some L1/L2 regularization
    l2=0.01,
    hyper_params=hyper_params3,
    max_w2=10                  ## helps stability for Rectifier
  )

  # Compare to base learner performance on the test set
  .getauc <- function(mm) h2o::h2o.auc( h2o::h2o.performance(h2o::h2o.getModel(mm), newdata = dftest.hex))
  .getF1 <- function(mm) {h2o::h2o.performance(h2o::h2o.getModel(mm), newdata = dftest.hex)->FD
    max(FD@metrics$thresholds_and_metric_scores$f1, na.rm = TRUE)
  }

  ## Sort by f1
  grid3 <- h2o::h2o.getGrid("dl_grid",sort_by="f1",decreasing=TRUE)
  model <- h2o::h2o.getModel(grid3@model_ids[[1]])
  perf3 <- h2o::h2o.performance(model, newdata = dftest.hex)

  list()->f1
  for(i in 1: nrow(grid@summary_table)){
    print(i)
    best_model <- h2o::h2o.getModel(grid@model_ids[[i]])
    h2o::h2o.performance(best_model, newdata=dftest.hex) ->fd  ## full validation data
    max(fd@metrics$thresholds_and_metric_scores$f1, na.rm = TRUE)->f1[[i]]
  }

  do.call(rbind, f1)->f1
  which.max(f1)->modid
  best_model <- h2o::h2o.getModel(grid@model_ids[[modid]])

  list()->f2
  for(i in 1: nrow(grid2@summary_table)){
    print(i)
    #grid@summary_table[139,]
    best_model2 <- h2o::h2o.getModel(grid2@model_ids[[i]])
    h2o::h2o.performance(best_model2, newdata=dftest.hex) ->fd  ## full validation data
    max(fd@metrics$thresholds_and_metric_scores$f1, na.rm = TRUE)->f2[[i]]
  }

  do.call(rbind, f2)->f1_2
  which.max(f1_2)->modid2
  best_model2 <- h2o::h2o.getModel(grid2@model_ids[[modid2]])

  list()->f3
  for(i in 1: nrow(grid3@summary_table)){
    print(i)
    #grid@summary_table[139,]
    best_model3 <- h2o::h2o.getModel(grid3@model_ids[[i]])
    h2o::h2o.performance(best_model3, newdata=dftest.hex) ->fd  ## full validation data
    max(fd@metrics$thresholds_and_metric_scores$f1, na.rm = TRUE)->f3[[i]]
  }

  do.call(rbind, f3)->f1_3
  which.max(f1_3)->modid3
  best_model3 <- h2o::h2o.getModel(grid3@model_ids[[modid3]])

  perf1 <- h2o::h2o.performance(best_model, newdata = dftest.hex)
  perf2 <- h2o::h2o.performance(best_model2, newdata = dftest.hex)
  perf3 <- h2o::h2o.performance(best_model3, newdata = dftest.hex)

  # Train a stacked ensemble using the GBM grid

  length(grid@model_ids)->L1
  length(grid2@model_ids)->L2
  length(grid3@model_ids)->L3


  ensemble2 <- h2o::h2o.stackedEnsemble(
    x=predictors,
    y=response,
    training_frame = train,
    model_id = "ensemble",
    metalearner_algorithm="glm",
    base_models = c(sample(grid3@model_ids,round(0.8*L3,0)), sample(grid2@model_ids,round(0.8*L2,0)),sample(grid@model_ids,round(0.8*L1,0)))
    #base_models = list(best_model, best_model2, best_model3)
  )

  # Eval ensemble performance on a test set
  perf <- h2o::h2o.performance(ensemble2, newdata = dftest.hex)

  baselearner_aucs <- sapply(grid@model_ids, .getauc)
  baselearner_best_auc_test <- max(baselearner_aucs)

  baselearner_aucs2 <- sapply(grid2@model_ids, .getauc)
  baselearner_best_auc_test2 <- max(baselearner_aucs2)

  baselearner_aucs3 <- sapply(grid3@model_ids, .getauc)
  baselearner_best_auc_test3 <- max(baselearner_aucs3)

  ensemble_auc_test <- h2o::h2o.auc(perf)

  best_F1 <- sapply(grid@model_ids[[modid]], .getF1)
  best_F2 <- sapply(grid2@model_ids[[modid2]], .getF1)
  best_F3 <- sapply(grid3@model_ids[[modid3]], .getF1)

  print(sprintf("Best Base-learner Test AUC:  %s", baselearner_best_auc_test))
  print(sprintf("Best Base-learner2 Test AUC:  %s", baselearner_best_auc_test2))
  print(sprintf("Best Base-learner3 Test AUC:  %s", baselearner_best_auc_test3))

  print(sprintf("Ensemble Test AUC:  %s", ensemble_auc_test))

  ensemble_F1_test <- sapply(ensemble2@model_id, .getF1)
  ensemble_best_F1<- max(ensemble_F1_test)

  print(sprintf("Best Base-learner Test F1:  %s", best_F1))
  print(sprintf("Best Base-learner2 Test F1:  %s", best_F2))
  print(sprintf("Best Base-learner3 Test F1:  %s", best_F3))
  print(sprintf("Ensemble Test F1:  %s", ensemble_best_F1))

  which.max(c(best_F1, best_F2, best_F3, ensemble_best_F1))->minModelID
  c(best_model, best_model2, best_model3, ensemble2)->all_models

  finalModel<-all_models[[minModelID]]

  ##retrain model using all data and calibrate model
  #weights <- c(0, rep(1, nrow(train) - 1))
  #weights2 <- c(0, rep(1, nrow(valid) - 1))

  #train$weight <- as.h2o(weights)
  #valid$weight <- as.h2o(weights2)

  # Download the MOJO and the resulting h2o-genmodel.jar file
  # to a new **experiment** folder. Note that the ``h2o-genmodel.jar`` file
  # is a library that supports scoring and contains the required readers
  # and interpreters. This file is required when MOJO models are deployed
  # to production. Be sure to specify the entire path, not just the relative path.


  #modelfile <-  h2o.download_mojo(finalModel, path=getwd(), get_genmodel_jar=TRUE)

  h2o::h2o.saveModel(finalModel, path = path, force = TRUE)->stackedGBMPath

  h2o::h2o.confusionMatrix(finalModel, newdata=dftest.hex, metric="f1")->confMat
  h2o::h2o.confusionMatrix(finalModel, newdata=dftrain.hex, metric="f1")->confMatTrain

  list(perf=perf, conf=confMat, confTrain=confMatTrain, finalModel=finalModel, GBMgrid=grid, RFgrid=grid2, bestMod=minModelID, stackedGBMPath)->result

  return(result)

}
