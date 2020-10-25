
#' Pipe Analysis
#' # This function groups and runs metrics on the pipes and work orders by cohort
# imputes pipe material with random forest before metrics are run
# and returns these results in a list
#' @param workorder_data
#'
#' @param asset_data
#' @param outages
#'
#' @return
#' @export
#'
#' @examples
pipe_cohort_analysis <- function(outages, workorder_data, asset_data) {

  # New filter, remove decommisioned and not operating Pipes
  asset_data %>%
    dplyr::filter(Status != "NOTOPERATING") %>%
    dplyr::filter(Status != "DECOMMISSIONED") -> asset_data


  asset_data$Static_Pressure_Final<-as.numeric(asset_data$Static_Pressure_Final)
  asset_data %>% dplyr::filter(Class.Structure.Level.03.Description ==
    "Drinking Water") -> asset_data

  asset_data %>% dplyr::filter(Class.Structure ==
    "Drinking Water Pipes") -> pipe_data

  # RF impute missing pipe material
  c(
    "Install.Date", "X.Coordinates", "Y.Coordinates",
     "Nominal.Pipe.Size..mm.", "Pipe.Material", "Length","Static_Pressure_Final"
  ) -> MatPred
  # extra data wrangling/cleaning
  grep("Pipe.Material", colnames(pipe_data)) -> pos2
  grep("Install.Date", colnames(pipe_data)) -> pos1

  pipe_data[pipe_data$Pipe.Material == "", pos2] <- "NA"
  pipe_data[pipe_data$Pipe.Material == "UNKNOWN", pos2] <- "NA"
  pipe_data$Static_Pressure_Final[pipe_data$Static_Pressure_Final >300] <- NA
  pipe_data$Static_Pressure_Final[pipe_data$Static_Pressure_Final <0] <- NA

  factor(pipe_data$Pipe.Material) -> pipe_data$Pipe.Material

  pipe_data %>% dplyr::select(dplyr::all_of(MatPred)) -> X1

  # use Random Forest to impute missing info in pipe material and date

  ImpDFs<-replicate(20, {missRanger::missRanger(X1, pmm.k = 5,  num.trees = 100) ->X
    X %>% dplyr::select(Pipe.Material, Install.Date)}, simplify=FALSE)

  all.matrix <- abind::abind(ImpDFs, along=3)

  getmode <- function(v) {
    apply(v,1,function(x){uniqv <- unique(x)
    uniqv[which.max(tabulate(match(x, uniqv)))]})
  }

  apply(all.matrix, c(2), getmode) -> imputedValues
  pipe_data$Install.Date <-imputedValues[,2]
  pipe_data$Pipe.Material <-imputedValues[,1]

  paste0(pipe_data$Pipe.Material, ", ", pipe_data$Install.Date) -> pipe_data$I.D
  as.factor(pipe_data$I.D) -> pipe_data$I.D

  pipe_data %>% dplyr::select(Asset.Number, Pipe.Material, Install.Date, I.D) -> tmp
  dplyr::left_join(asset_data, tmp, by="Asset.Number") -> asset_data

  coalesce2(asset_data$Pipe.Material.x, asset_data$Pipe.Material.y) ->
    asset_data$Pipe.Material

  coalesce2(asset_data$Install.Date.x, asset_data$Install.Date.y) ->
    asset_data$Install.Date

  asset_data$Install.Date.x <-NULL
  asset_data$Pipe.Material.x<-NULL
  asset_data$Install.Date.y<-NULL
  asset_data$Pipe.Material.y<-NULL

  ImpDFs<-replicate(20, {missRanger::missRanger(X1, pmm.k = 5,  num.trees = 100) ->X
    X %>% dplyr::select(Static_Pressure_Final)}, simplify=FALSE)

  all.matrix <- abind::abind(ImpDFs, along=3)
  apply(all.matrix, c(1), mean) -> imputedValues
  pipe_data$Static_Pressure_Final <-imputedValues
  pipe_data %>% dplyr::select(Asset.Number, Static_Pressure_Final) -> tmp
  dplyr::left_join(asset_data, tmp, by="Asset.Number") -> asset_data

  coalesce2(asset_data$Static_Pressure_Final.x, asset_data$Static_Pressure_Final.y) ->
    asset_data$Static_Pressure_Final

  asset_data$Static_Pressure_Final.x <-NULL
  asset_data$Static_Pressure_Final.y<-NULL

  #Add Static Pressure to WorkOrders
  workorder_data$Asset.Number<-as.numeric(workorder_data$Asset.Number)
  dplyr::left_join(workorder_data, tmp, by="Asset.Number") -> workorder_data

  # Work Order Summary Plots,  why here?

  # remove main filter 4/10  trying to understand why N>2 drops from 800 to 70.
  if (outages == TRUE) {
    workorder_data %>% filter(Number.of.Work.Order.Water.Outages > 0) ->
    workorder_data
  }

  # # reconfigure dates
  as.data.frame(workorder_data) -> workorder_data
  as.Date(workorder_data$Install.Date) -> workorder_data$Install.Date
  lubridate::year(workorder_data$Install.Date) ->
  workorder_data$Install.Date


  # # make missing data consistent
  # grep("Pipe.Material", colnames(workorder_data)) -> loc1
  # workorder_data[workorder_data$Pipe.Material == "", loc1] <- "NA"
  # workorder_data[workorder_data$Pipe.Material == "UNKNOWN", loc1] <-
  #   "NA"

  factor(workorder_data$Pipe.Material) ->
  workorder_data$Pipe.Material


  #### Analysis of complete data ####

  asset_data %>%
    dplyr::select(I.D, Length, Install.Date) %>%
    dplyr::filter(Length > 0) -> subset_data
  subset_data[complete.cases(subset_data), ] -> subset_data

  subset_data %>%
    dplyr::group_by(I.D) %>%
    dplyr::tally(Length) %>%
    as.data.frame() -> PipeLength
  subset_data %>%
    dplyr::group_by(I.D, Install.Date) %>%
    dplyr::summarize(
      Nassets = dplyr::n()
    ) %>%
    as.data.frame() -> PipeNumber

  dplyr::left_join(PipeNumber, PipeLength, by = "I.D") -> temp
  colnames(temp)[4] <- "Length"
  temp$Length <- as.numeric(temp$Length)
  temp %>% dplyr::mutate(MeanLength_meters = (Length / Nassets)) -> PipeNumber

  as.numeric(workorder_data$Asset.Number) -> workorder_data$Asset.Number

  asset_data %>% dplyr::select(Asset.Number, Pipe.Material, Install.Date)->tmp2
  dplyr::left_join(workorder_data, tmp2, by="Asset.Number") -> workorder_data

  coalesce2(workorder_data$Pipe.Material.x, workorder_data$Pipe.Material.y) ->
    workorder_data$Pipe.Material

  coalesce2(workorder_data$Install.Date.x, workorder_data$Install.Date.y) ->
    workorder_data$Install.Date

  workorder_data$Install.Date.x <-NULL
  workorder_data$Pipe.Material.x<-NULL
  workorder_data$Install.Date.y<-NULL
  workorder_data$Pipe.Material.y<-NULL

  paste0(workorder_data$Pipe.Material, ", ", workorder_data$Install.Date) -> workorder_data$I.D
  as.factor(workorder_data$I.D) -> workorder_data$I.D

  workorder_data %>%
    dplyr::select(I.D, Length, Install.Date) %>%
    dplyr::filter(Length > 0) -> subset_data2
  subset_data2[complete.cases(subset_data2), ] -> subset_data2

  subset_data2 %>%
    dplyr::group_by(I.D) %>%
    dplyr::summarize(NFails = dplyr::n()) %>%
    as.data.frame() -> PipeNumber2


  dplyr::left_join(PipeNumber, PipeNumber2, by = "I.D") -> table.result
  as.data.frame(table.result) -> table.result

  # Fails by age and length
  table.result$NFails[is.na(table.result$NFails)] <- 0
  table.result %>% dplyr::mutate(Nfailspertotallength = NFails / Length) -> table.result
  table.result %>% dplyr::arrange(dplyr::desc(Nfailspertotallength)) -> table.result
  table.result %>% dplyr::filter(MeanLength_meters > 0.001, Nassets > 1) -> table.result
  table.result[stats::complete.cases(table.result), ] -> table.result
  table.result$Age <- lubridate::year(Sys.Date()) - as.numeric(table.result$Install.Date)
  plot(table.result$Age, table.result$Nfailspertotallength)

  table.result %>% dplyr::mutate(NfailperNasset = NFails / Nassets) -> table.result

  # joining on ID  this doesn't work  NA, NA, not unique!
  as.character(workorder_data$I.D) -> workorder_data$I.D

  grep("NA", table.result$I.D)->dropRow
  table.result[-dropRow, ] ->table.result
  table.result$Install.Date<-NULL

  colnames(table.result)[3]<-"CohortLength"
  table.result2 <- dplyr::left_join(workorder_data, table.result, by = "I.D")

  results <- list(WOMetrics = table.result2, AssetImputed=asset_data, CohortSummary=table.result)

  return(results)
}
