
#' CohortAnalysis
#' this function updates the cohorts and runs analysis and metrics on the data
# per Cohort and returns pipe metrics by cohort

#' @param update
#' @param outages
#'
#' @return
#' @export
#'
#' @examples
preCohort <- function(update, outages) {
  if (update %in% "Yes") {
    print("Running Pipe Analysis")

    # includes additional NSLIDS>X columns
    readRDS("Outputs/workorder_asset_data_Repeats.RDS") -> WO_Repeats

    as.data.frame(WO_Repeats) -> WO_Repeats

    # runs pipe analysis
    pipe_analysis(
      outages = outages, asset_workorder_data = WO_Repeats,
      update = update
    ) -> result

    print("Finished Analysis")
  } else {
    print("Not updating")
    readRDS("Outputs/WorkOrderPipeMetrics.RDS") -> result
  }
  return(result)
}



#' Pipe Analysis
#' # This function groups and runs metrics on the pipes and work orders by cohort
# imputes pipe material with random forest before metrics are run
# and returns these results in a list
#' @param outages
#' @param asset_workorder_data
#' @param update
#'
#' @return
#' @export
#'
#' @examples
pipe_analysis <- function(outages, asset_workorder_data, update) {

  # example of asset number in SOB i =2 that can't be found, no cohort ID 1158712
  # Asset Number with no Cohort,  17965 DUCTILE IRON CEMENT LINED, 1989
  readRDS("Outputs/AssetData.RDS") -> asset_data

  # New filter, remove decommisioned and not operating
  asset_data %>%
    dplyr::filter(Status != "NOTOPERATING") %>%
    dplyr::filter(Status != "DECOMMISSIONED") -> asset_data
  asset_data %>% dplyr::filter(Class.Structure.Level.03.Description ==
    "Drinking Water") -> asset_data
  asset_data %>% dplyr::filter(Class.Structure ==
    "Drinking Water Pipes") -> asset_data

  # RF impute missing pipe material
  c(
    "Install.Date", "X.Coordinates", "Y.Coordinates",
    "Shutoff.Block", "Nominal.Pipe.Size..mm.", "Pipe.Material"
  ) -> MatPred

  # data wrangling/cleaning
  grep("Pipe.Material", colnames(asset_data)) -> pos2
  grep("Install.Date", colnames(asset_data)) -> pos1
  asset_data[asset_data$Pipe.Material == "", pos2] <- "NA"
  asset_data[asset_data$Pipe.Material == "UNKNOWN", pos2] <- "NA"
  factor(asset_data$Pipe.Material) -> asset_data$Pipe.Material
  asset_data %>% dplyr::select(MatPred) -> X1
  X1[complete.cases(X1), ] -> X2
  factor(X2$Pipe.Material) -> X2$Pipe.Material

  # use Random Forest to impute missing info in pipe material and date
  if (update %in% "Yes") {
    rf <- randomForest(Pipe.Material ~ ., data = X2, ntree = 6, mtry = 4)
    rf2 <- randomForest(Install.Date ~ ., data = X2, ntree = 6, mtry = 4)
    saveRDS(rf, "Outputs/RF_pipe.RDS")
    saveRDS(rf2, "Outputs/RF_installyear.RDS")
  } else {
    readRDS("Outputs/RF_pipe.RDS") -> rf
    readRDS("Outputs/RF_installyear.RDS") -> rf2
  }

  # impute predicted materials in asset data
  X1[asset_data$Install.Date %in% NA, ] -> Xnew2
  asset_data[asset_data$Install.Date %in% NA, pos1] <-
    round(predict(rf2, Xnew2), 0)
  asset_data[asset_data$Pipe.Material %in% "NA", MatPred] -> Xnew1
  grep("Pipe.Material", colnames(asset_data)) -> pos2
  asset_data[asset_data$Pipe.Material %in% "NA", pos2] <- predict(rf, Xnew1)
  paste0(asset_data$Pipe.Material, ", ", asset_data$Install.Date) -> asset_data$I.D
  as.factor(asset_data$I.D) -> asset_data$I.D

  saveRDS(asset_data, "Outputs/AssetData_Imputed.RDS")

  # Work Order Summary Plots
  colnames(asset_workorder_data)

  # remove main filter 4/10  trying to understand why N>2 drops from 800 to 70.
  if (outages == TRUE) {
    asset_workorder_data %>% filter(Number.of.Work.Order.Water.Outages > 0) ->
    asset_workorder_data
  }

  # reconfigure dates
  as.data.frame(asset_workorder_data) -> asset_workorder_data
  as.Date(asset_workorder_data$Install.Date) -> asset_workorder_data$Install.Date
  lubridate::year(asset_workorder_data$Install.Date) ->
  asset_workorder_data$Install.Date

  # make missing data consistent
  grep("Pipe.Material", colnames(asset_workorder_data)) -> loc1
  asset_workorder_data[asset_workorder_data$Pipe.Material == "", loc1] <- "NA"
  asset_workorder_data[asset_workorder_data$Pipe.Material == "UNKNOWN", loc1] <-
    "NA"

  factor(asset_workorder_data$Pipe.Material) ->
  asset_workorder_data$Pipe.Material

  asset_workorder_data %>% dplyr::filter(I.D %in%
    "MEDIUM DENSITY POLYETHYLENE, 2007")

  #### Analysis of complete data ####
  asset_data %>%
    dplyr::select(I.D, Length, Install.Date) %>%
    dplyr::filter(Length > 0) -> subset_data
  subset_data[complete.cases(subset_data), ] -> subset_data

  subset_data %>%
    group_by(I.D) %>%
    tally(Length) %>%
    as.data.frame() -> PipeLength
  subset_data %>%
    group_by(I.D) %>%
    dplyr::summarize(
      Nassets = n(),
      Date = mean(Install.Date)
    ) %>%
    as.data.frame() -> PipeNumber

  left_join(PipeNumber, PipeLength, by = "I.D") -> temp
  colnames(temp)[4] <- "Length"
  temp$Length <- as.numeric(temp$Length)
  temp %>% mutate(MeanLength_meters = (Length / Nassets)) -> PipeNumber

  asset_workorder_data %>%
    dplyr::select(I.D, Length, Install.Date) %>%
    dplyr::filter(Length > 0) -> subset_data2
  subset_data2[complete.cases(subset_data2), ] -> subset_data2

  subset_data2 %>%
    group_by(I.D) %>%
    dplyr::summarize(NFails = n()) %>%
    as.data.frame() -> PipeNumber2
  as.character(PipeNumber2$I.D) -> PipeNumber2$I.D
  as.character(PipeNumber$I.D) -> PipeNumber$I.D

  left_join(PipeNumber, PipeNumber2, by = "I.D") -> table.result
  as.data.frame(table.result) -> table.result

  # Fails by age and length
  table.result$NFails[is.na(table.result$NFails)] <- 0
  table.result %>% mutate(Nfailspertotallength = NFails / Length) -> table.result
  table.result %>% arrange(desc(Nfailspertotallength)) -> table.result
  table.result %>% filter(MeanLength_meters > 0.001, Nassets > 1) -> table.result
  table.result[complete.cases(table.result), ] -> table.result
  table.result$Age <- lubridate::year(Sys.Date()) - table.result$Date
  plot(table.result$Age, table.result$Nfailspertotallength)

  table.result %>% mutate(NfailperNasset = NFails / Nassets) -> table.result

  # joining on ID
  as.character(asset_workorder_data$I.D) -> asset_workorder_data$I.D
  table.result2 <- left_join(asset_workorder_data, table.result, by = "I.D")

  table.result2[is.na(table.result2$I.D), ]
  table.result2[!is.na(table.result2$I.D), ]
  summary(table.result2)

  saveRDS(table.result2, "Outputs/WorkOrderPipeMetrics.RDS")

  results <- list(combined = table.result2)

  return(results)
}
