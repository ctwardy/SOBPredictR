

###
ZoneAnalysis <- function(ZoneResult, WOD, ZoneNr, PrCutOff, maxLength) {

  # Step 1 Filter all pipes below a threshold.  How many pipes and SOBS

  WOD %>% dplyr::filter(Distribution.Zone.Name %in% Zones[ZoneNr]) -> WOD
  WOD %>%
    dplyr::group_by(lubridate::year(Reported.Date)) %>%
    dplyr::summarize("Number" = dplyr::n()) -> yeartotals
  colnames(yeartotals) <- c("Year", "Number")
  ggplot2::ggplot(data = yeartotals, ggplot2::aes(x = Year, y = Number, group = 1)) +
    ggplot2::geom_line() +
    ggplot2::geom_point() -> P1

  mean(utils::tail(yeartotals$Number, 6), na.rm = TRUE) -> AvBurstsinZone

  WOD %>%
    dplyr::group_by(lubridate::year(Reported.Date)) %>%
    dplyr::summarise(NoG2 = sum(NSLIDS_1 > 0, na.rm = TRUE) + sum(NSLIDS_2 > 0, na.rm = TRUE), G2 = sum(NSLIDS_3 > 0, na.rm = TRUE) + sum(NSLIDS_4 > 0, na.rm = TRUE) + sum(NSLIDS_5 > 0, na.rm = TRUE) + sum(NSLIDS_6 > 0, na.rm = TRUE) + sum(NSLIDS_7 > 0, na.rm = TRUE)) -> G2

  colnames(G2) <- c("Year", "NumberNonG2events", "NumberG2events")

  ggplot2::ggplot(data = G2, ggplot2::aes(x = Year)) +
    ggplot2::geom_line(ggplot2::aes(y = NumberNonG2events, colour = "Near G2")) +
    ggplot2::geom_line(ggplot2::aes(y = NumberG2events, colour = "G2")) -> P2
  mean(utils::tail(G2$NumberG2events, 7), na.rm = TRUE) -> AvG2eventsinZone

  WOD %>%
    dplyr::group_by(lubridate::year(Reported.Date)) %>%
    dplyr::summarise(NoG2 = sum(NSLIDS_1 [NSLIDS_1 > 0], na.rm = TRUE) + sum(NSLIDS_2 [NSLIDS_2 > 0], na.rm = TRUE), G2 = sum(NSLIDS_3 [NSLIDS_3 > 0], na.rm = TRUE) + sum(NSLIDS_4 [NSLIDS_4 > 0], na.rm = TRUE) + sum(NSLIDS_5 [NSLIDS_5 > 0], na.rm = TRUE) + sum(NSLIDS_6 [NSLIDS_6 > 0], na.rm = TRUE) + sum(NSLIDS_7 [NSLIDS_7 > 0], na.rm = TRUE)) -> G3

  colnames(G3) <- c("Year", "NumberNonG2SLIDS", "NumberG2SLIDS")

  ggplot2::ggplot(data = G3, ggplot2::aes(x = Year)) +
    ggplot2::geom_line(ggplot2::aes(y = NumberNonG2SLIDS, colour = "Non G2 SLIDS")) +
    ggplot2::geom_line(ggplot2::aes(y = NumberG2SLIDS, colour = "G2 SLIDS")) -> P3

  ZoneResult %>%
    dplyr::filter(DZ %in% Zones[ZoneNr]) -> ZoneResult

  unique(ZoneResult$Shutoff.Block)
  sum(ZoneResult$NFail, na.rm = TRUE) -> totalNFailsinDZ

  length(unique(ZoneResult$Asset.Number)) -> nAssetsinZone
  sum(ZoneResult$Length, na.rm = TRUE) -> totalLengthinZone

  ZoneResult %>%
    dplyr::distinct(CohortID, .keep_all = TRUE) %>%
    dplyr::mutate(temp = (NassetsinCohortinZone / N_asset_inCohort) * Cohort_FNR) %>%
    dplyr::summarize(sum(temp)) %>%
    as.numeric() -> approxFNbursts # aportioning the Cohort FNR to the fraction of Cohort in Zone.  Gives number of bursts in that zone for that cohort.

  ZoneResult$Prediction
  # filter out SOBs not predicted to fail
  ZoneResult %>%
    dplyr::filter(grepl("more", ZoneResult$Prediction))  -> ZoneResult

  # filter out bottom 95% percentile
  ZoneResult %>% dplyr::filter(PrNmorethan0in1 > PrCutOff) -> ZoneResult
  nrow(ZoneResult)
  # need to check that SOB IDs are correct
  ZoneResult %>%
    dplyr::group_by(Shutoff.Block) %>%
    dplyr::summarize(Intensity = mean(Median_Intensity, na.rm = TRUE)) -> A
  ZoneResult %>%
    dplyr::group_by(Shutoff.Block) %>%
    dplyr::summarize(Npipes = dplyr::n()) -> B
  ZoneResult %>%
    dplyr::group_by(Shutoff.Block) %>%
    dplyr::summarize(Length = sum(Length)) -> C

  cbind(A, B[, 2], C[, 2]) -> SOBTable

  SOBTable %>%
    dplyr::mutate(NpipeIntensity = Intensity * Npipes) %>%
    dplyr::arrange(dplyr::desc(NpipeIntensity)) -> SOBTable

  SOBTable %>%
    dplyr::filter(!Npipes < 1) -> SOBTable

  SOBTable %>%
    dplyr::mutate(cumLength = SOBTable$Length %>% cumsum()) %>%
    dplyr::filter(cumLength < maxLength) %>%
    dplyr::select(-cumLength) -> SOBTable

  FinalSOBs <- SOBTable$Shutoff.Block
  ZoneResult %>% dplyr::filter(Shutoff.Block %in% FinalSOBs) -> result2
  sum(result2$Length) -> totalLength
  unique(result2$Shutoff.Block) -> totalNSOB
  sum(result2$SOBData..Shutoff.Number.of.slids., na.rm = TRUE) -> totalSLID
  sum(result2$NFail, na.rm = TRUE) -> totalNFailinselected

  list(DF = result2, DF2 = SOBTable, totalSLIDS = totalSLID, Lengthtoreplace = totalLength, SOBs = FinalSOBs, approxNbursts = approxFNbursts, totalNFailinselected = totalNFailinselected, totalNFailsinDZ = totalNFailsinDZ, totL = totalLengthinZone, AvgBursts = AvBurstsinZone, AvgG2 = AvG2eventsinZone, NassetsinZone = nAssetsinZone, P1, P2, P3) -> out

  return(out)
}


#' Zone Results
#'
#' Pipe Modelling for Zones
#'
#' @param PipeResults dataframe
#' @param PrCutOff numeric
#' @param workorder_data dataframe
#' @param maxLength numeric
#'
#' @return list of dataframes
#' @export
#' @examples \dontrun{
#' ZoneResults<-function(PipeResults, PrCutOff, workorder_data, maxLength)
#' }
ZoneResults<-function(PipeResults, PrCutOff, workorder_data, maxLength){

  unique(PipeResults$DZ) -> Zones

  zoneresults <- list()


  for (k in 1:length(Zones)) {
    print(k)
    ZoneAnalysis(ZoneNr = k,
                 ZoneResult=PipeResults,
                 PrCutOff=PrCutOff,
                 maxLength =maxLength,
                 WOD=workorder_data) -> zoneresults[[k]]
  }

  lapply(zoneresults, function(x) x[[1]]) -> table1
  do.call(rbind, table1) -> table1

  table1 <- table1[,colSums(is.na(table1))<nrow(table1)]

  lapply(zoneresults, function(x) x[[2]]) -> table2
  do.call(rbind, table2) -> table2

  out <- list()
  for (l in 1:length(Zones)) {
    newDF <- data.frame(
    Zone=Zones[[l]],
    totalSLIDSinTargetedSOBS = zoneresults[[l]]$totalSLIDS,
    Lengthtoreplace=zoneresults[[l]]$Lengthtoreplace,
    SOBsTargeted=length(zoneresults[[l]]$SOBs),
    nAssetsReplace=nrow(zoneresults[[l]]$DF),
    AvgBursts=zoneresults[[l]]$AvgBursts,
    AvgG2=zoneresults[[l]]$AvgG2,
    NassetsinZone=zoneresults[[l]]$NassetsinZone,
    totalLengthinZone=zoneresults[[l]]$totL,
    totalNFailinselected=zoneresults[[l]]$totalNFailinselected,
    totalNFailsinDZ=zoneresults[[l]]$totalNFailsinDZ
)
    newDF -> out[[l]]
  }

  do.call(rbind, out) -> r

  r %>% dplyr::mutate(SLIDSperLength = totalSLIDSinTargetedSOBS / (Lengthtoreplace)) -> r
  r %>% dplyr::mutate(SOBsperLength = SOBsTargeted / (Lengthtoreplace)) -> r
  r %>% dplyr::mutate(burstsper100kmpredicted = (totalNFailsinDZ - totalNFailinselected) / 24 / (totalLengthinZone / 1000) * 100) -> r
  r %>% dplyr::mutate(burstsper100km = (totalNFailsinDZ) / 24 / (totalLengthinZone / 1000) * 100) -> r

 out<-list(table1, table2, r)
 return(out)

}


#' rankWDZ
#'
#' ranks WDZ in order of decreasing proportion of SOBs
#'
#' @param WDZPath character
#' @param SOBResult dataframe
#' @param N integer
#'
#' @return a list of the top N Zones
#' @export
#'
#' @examples \dontrun{
#' rankWDZ <- function(WDZPath, SOBResult, N=10)
#' }
rankWDZ <- function(WDZPath, SOBResult, N, newPosClassLabel) {
  utils::read.csv(WDZPath) -> WDZ

  # Previously Distribution Zone Name Errors needed to be fixed in Asset and WOD tables
  WDZ$Water_Distribution_Zone <- gsub("P.R.", "PR", WDZ$Water_Distribution_Zone)
  WDZ$Water_Distribution_Zone <- gsub("P.R", "PR", WDZ$Water_Distribution_Zone)
  WDZ$Water_Distribution_Zone <- gsub("Res.", "Res", WDZ$Water_Distribution_Zone)
  WDZ$Water_Distribution_Zone <- gsub("Res..", "Res", WDZ$Water_Distribution_Zone)
  WDZ$Water_Distribution_Zone <- gsub("P/B", "PB", WDZ$Water_Distribution_Zone)
  WDZ$Water_Distribution_Zone <- gsub("Creek", "Ck", WDZ$Water_Distribution_Zone)
  WDZ$Water_Distribution_Zone <- gsub("P.B", "PB", WDZ$Water_Distribution_Zone)
  WDZ$Water_Distribution_Zone <- gsub("Olinda - Mitcham", "Olinda Mitcham", WDZ$Water_Distribution_Zone)
  WDZ$Water_Distribution_Zone <- gsub("Resoir", "Reservoir", WDZ$Water_Distribution_Zone)
  WDZ$Water_Distribution_Zone <- gsub("Resch", "Research", WDZ$Water_Distribution_Zone)
  WDZ$Water_Distribution_Zone <- gsub("Sub Zone ", "Res", WDZ$Water_Distribution_Zone)
  WDZ$Water_Distribution_Zone <- gsub(" No ", "Res ", WDZ$Water_Distribution_Zone)

  WDZ$Water_Distribution_Zone[WDZ$Water_Distribution_Zone == "UNKN"] <- NA
  WDZ$Water_Distribution_Zone[WDZ$Water_Distribution_Zone == "<NA>"] <- NA
  WDZ$Status[WDZ$Status == "DECOMMISSIONED"] <- NA

  WDZ %>% dplyr::group_by(Water_Distribution_Zone) %>%
    dplyr::tally() %>%
    as.data.frame() -> TotSOBperZone
  dplyr::left_join(SOBResult[[1]], WDZ, by = c("SOB" = "Shutoff_Block")) -> SOB_WDZ

  SOB_WDZ$Water_Distribution_Zone[SOB_WDZ$Water_Distribution_Zone == "UNKN"] <- NA
  SOB_WDZ$Water_Distribution_Zone[SOB_WDZ$Water_Distribution_Zone == "<NA>"] <- NA
  SOB_WDZ$Status[SOB_WDZ$Status == "DECOMMISSIONED"] <- NA

  SOB_WDZ[stats::complete.cases(SOB_WDZ), ] -> SOB_WDZ

  SOB_WDZ %>%
    dplyr::filter(Prediction == newPosClassLabel) %>%
    dplyr::group_by(Water_Distribution_Zone) %>%
    dplyr::tally() %>%
    as.data.frame() -> SOBPredperWDZ
  dplyr::left_join(TotSOBperZone, SOBPredperWDZ, by = "Water_Distribution_Zone") -> SOBPredperWDZ

  SOBPredperWDZ$n.y[is.na(SOBPredperWDZ$n.y)] <- 0
  SOBPredperWDZ %>%
    dplyr::mutate(propSOBinWDZ = n.y / n.x) %>%
    dplyr::arrange(dplyr::desc(propSOBinWDZ)) %>%
    dplyr::filter(n.x > 50) -> SOBPredperWDZ

  Zones <- utils::head(SOBPredperWDZ$Water_Distribution_Zone, N)

  SOBPredperWDZ %>%
    dplyr::filter(Water_Distribution_Zone %in% Zones) %>%
    as.data.frame() -> Final
    list(Final, Zones) ->out
  return(out)
}

#' PipeModel
#' Individual pipe intensity modelling
#' @param workorder_data
#' @param asset_data
#' @param cohort_data
#' @param SOBData
#' @param WDZAssetPath
#' @param Zones
#' @param SOBResult
#' @param NHPPResult
#' @param ResultPath
#' @param TestStart
#' @param TestEnd
#'
#' @return
#' @export

pipeModel <- function(workorder_data, asset_data, cohort_data, SOBData, WDZAssetPath, Zones, SOBResult, NHPPResult, ResultPath, TestStart, TestEnd) {

  dir.create(ResultPath)
  # ####  Pipe Modeling Start

  workorder_data %>% dplyr::filter(Class.Structure %in% "Repair Burst Water Main" |
    Class.Structure %in% "Repair Leaking Water Main") -> workorder_data

  asset_data %>% dplyr::filter(Class.Structure %in% "Drinking Water Pipes") -> asset_data
  asset_data %>% dplyr::mutate(Pipe.Material = replace(Pipe.Material, Pipe.Material == "", "NA")) -> asset_data
  asset_data$ID <- paste0(asset_data$Pipe.Material, ", ", asset_data$Install.Date)
  #

  # #
  # # #Fix Distribution Zone Name Errors in WO table
  workorder_data$Distribution.Zone.Name <- gsub('P.R.', 'PR', workorder_data$Distribution.Zone.Name)
  workorder_data$Distribution.Zone.Name <- gsub('P.R', 'PR', workorder_data$Distribution.Zone.Name)
  workorder_data$Distribution.Zone.Name <- gsub('Res.', 'Res', workorder_data$Distribution.Zone.Name)
  workorder_data$Distribution.Zone.Name <- gsub('Res..', 'Res', workorder_data$Distribution.Zone.Name)
  workorder_data$Distribution.Zone.Name <- gsub('P/B', 'PB', workorder_data$Distribution.Zone.Name)
  workorder_data$Distribution.Zone.Name <- gsub('Creek', 'Ck', workorder_data$Distribution.Zone.Name)
  workorder_data$Distribution.Zone.Name <- gsub('P.B', 'PB', workorder_data$Distribution.Zone.Name)
  workorder_data$Distribution.Zone.Name <- gsub('Olinda - Mitcham', 'Olinda Mitcham', workorder_data$Distribution.Zone.Name)
  workorder_data$Distribution.Zone.Name <- gsub('Resoir', 'Reservoir', workorder_data$Distribution.Zone.Name)
  workorder_data$Distribution.Zone.Name <- gsub('Resch', 'Research', workorder_data$Distribution.Zone.Name)
  workorder_data$Distribution.Zone.Name <- gsub('PRV', 'PR', workorder_data$Distribution.Zone.Name)
  workorder_data$Distribution.Zone.Name <- gsub(' No ', 'Res ', workorder_data$Distribution.Zone.Name)
  # #

  # # Fix Distribution Zone Name Errors in Asset table
  asset_data$Distribution.Zone.Name <- gsub("P.R.", "PR", asset_data$Distribution.Zone.Name)
  asset_data$Distribution.Zone.Name <- gsub("P.R", "PR", asset_data$Distribution.Zone.Name)
  asset_data$Distribution.Zone.Name <- gsub("Res.", "Res", asset_data$Distribution.Zone.Name)
  asset_data$Distribution.Zone.Name <- gsub("Res..", "Res", asset_data$Distribution.Zone.Name)
  asset_data$Distribution.Zone.Name <- gsub("P/B", "PB", asset_data$Distribution.Zone.Name)
  asset_data$Distribution.Zone.Name <- gsub("Creek", "Ck", asset_data$Distribution.Zone.Name)
  asset_data$Distribution.Zone.Name <- gsub("P.B", "PB", asset_data$Distribution.Zone.Name)
  asset_data$Distribution.Zone.Name <- gsub("Olinda - Mitcham", "Olinda Mitcham", asset_data$Distribution.Zone.Name)
  asset_data$Distribution.Zone.Name <- gsub("Resoir", "Reservoir", asset_data$Distribution.Zone.Name)
  asset_data$Distribution.Zone.Name <- gsub("Resch", "Research", asset_data$Distribution.Zone.Name)
  asset_data$Distribution.Zone.Name <- gsub("PRV", "PR", asset_data$Distribution.Zone.Name)
  asset_data$Distribution.Zone.Name <- gsub(" No ", "Res ", asset_data$Distribution.Zone.Name)


  utils::read.csv(WDZAssetPath) -> WDZID
   # # #Fix Distribution Zone Name Errors in Asset table
  WDZID$Water.Distribution.Zone <- gsub('P.R.', 'PR', WDZID$Water.Distribution.Zone)
  WDZID$Water.Distribution.Zone <- gsub('P.R', 'PR', WDZID$Water.Distribution.Zone)
  WDZID$Water.Distribution.Zone <- gsub('Res.', 'Res', WDZID$Water.Distribution.Zone)
  WDZID$Water.Distribution.Zone <- gsub('Res..', 'Res', WDZID$Water.Distribution.Zone)
  WDZID$Water.Distribution.Zone <- gsub('P/B', 'PB', WDZID$Water.Distribution.Zone)
  WDZID$Water.Distribution.Zone <- gsub('Creek', 'Ck', WDZID$Water.Distribution.Zone)
  WDZID$Water.Distribution.Zone <- gsub('P.B', 'PB', WDZID$Water.Distribution.Zone)
  WDZID$Water.Distribution.Zone <- gsub('Olinda - Mitcham', 'Olinda Mitcham', WDZID$Water.Distribution.Zone)
  WDZID$Water.Distribution.Zone <- gsub('Resoir', 'Reservoir', WDZID$Water.Distribution.Zone)
  WDZID$Water.Distribution.Zone <- gsub('Resch', 'Research', WDZID$Water.Distribution.Zone)
  WDZID$Water.Distribution.Zone <- gsub('Sub Zone ', 'Res', WDZID$Water.Distribution.Zone)
  WDZID$Water.Distribution.Zone <- gsub(' No ', 'Res ', WDZID$Water.Distribution.Zone)

  Zones -> DZs
  cohort_IDs <- unique(cohort_data$Cohort)

  for (j in 1:length(DZs)) {
    print(paste0("Running Analysis on DZ ", DZs[[j]]))

    list(data.frame()) -> result
    DZs[[j]] ->DZ
    asset_data %>%
      dplyr::filter(Distribution.Zone.Name %in% DZ) %>%
      dplyr::distinct(I.D) -> CohortZoneIDs

    CohortZoneIDs[,] ->CohortZoneIDs
    grep("NA,", CohortZoneIDs) -> unknownIDs
    if(length(unknownIDs)>0)
    {CohortZoneIDs[-unknownIDs] -> CohortZoneIDs}

    # Pipe failure Intensity for all Cohorts i in DZ j
    # how much is the prior distribution influencing the posterior intensity???  how much does the gamma sampling contribute to the variance between assets versus the individual asset failure
    for (i in 1:length(CohortZoneIDs)) {

      # Use table2$SOBs to calculate intensity of all assets in trial zone(s), not just thos in SOBs predited to fail by model.

      print(paste0("Running Cohort ", i))

      tryCatch(
        {
          suppressWarnings(PipeIntensity(
            DZ = DZs[[j]],
            SOB = SOBResult$predDF$SOB,
            cohortFailure = cohort_data,
            asset_data = asset_data,
            work_Order_Data = workorder_data,
            CohortID=CohortZoneIDs[i],
            KnownCohorts=cohort_IDs,
            test_start = TestStart,
            test_end = TestEnd
          )) -> temp
        },
        error = function(e) {
          cat("ERROR :", conditionMessage(e), "\n")
        }
      )

      if (!inherits(temp, "error")) {
        temp -> result[[i]]
      } else {
        result[[i]] <- NULL
      }
    }

    do.call(rbind, result) -> ZoneResult

    ZoneResult %>% dplyr::filter(NassetsinCohortinZone > 0) -> ZoneResult
    ZoneResult$DZ <- DZs[[j]]
    colnames(ZoneResult)
    # ZoneResult[!is.na(ZoneResult$Asset.Number),]->ZoneResult
    lapply(ZoneResult[, -c(1, 8, 12, 13, 15)], as.numeric) -> ZoneResult[, -c(1, 8, 12, 13, 15)]


    forecastonly <- FALSE
    if (forecastonly) {
      ZoneResult %>% dplyr::select(-c("NFail_Test", "TestEnd", "TestStart")) -> ZoneResult
    }

    gsub(" ", "", (DZs[[j]]), fixed = TRUE) -> DZs[[j]]
    saveRDS(ZoneResult, paste0(ResultPath, DZs[[j]], ".RDS"))

  }

  # Consolidate all WDZ results...  Note at moment need to manually copy RDS files from /Outputs

  multiplezones <- (length(DZs) > 1)
  if (multiplezones) {
    filenames <- list.files(path = ResultPath, pattern = "\\.RDS$", full.names = TRUE)
    file.info(filenames) -> details
    details <- details[with(details, order(as.POSIXct(mtime))), ]
    filenames <- rownames(details)

    r <- lapply(filenames, readRDS)

    do.call(rbind, r) -> table_results
  } else {
    (ZoneResult -> table_results)
  }

  as.numeric(table_results$Median_Intensity) -> table_results$Median_Intensity

  table_results %>% dplyr::arrange(dplyr::desc(Median_Intensity)) -> table_results

  # check for duplicated,  remove duplicates need to work out why they are appearing in the results, mostly NA

  if (any(is.na(table_results$Asset.Number))) {
    table_results[-which(is.na(table_results$Asset.Number)), ] -> table_results
  }
  # Add various Probabilities of N in T to results table,  Note the intensity values T are in Years, N here refers to combined burst and leak

  PrPoisson <- function(Lambda, N, T) {
    exp(-Lambda * T) * (Lambda * T)^N / factorial(N) -> x
    return(x)
  }

  table_results %>% dplyr::mutate(PrNmorethan2in1 = round(
    1 - (PrPoisson(Lambda = table_results$Median_Intensity, T = 1, N = 0) +
      PrPoisson(Lambda = table_results$Median_Intensity, T = 1, N = 1) +
      PrPoisson(Lambda = table_results$Median_Intensity, T = 1, N = 2)), 2
  )) -> table_results

  table_results %>% dplyr::mutate(PrNmorethan2in3 = round(
    1 - (PrPoisson(Lambda = table_results$Median_Intensity, T = 3, N = 0) +
      PrPoisson(Lambda = table_results$Median_Intensity, T = 3, N = 1) +
      PrPoisson(Lambda = table_results$Median_Intensity, T = 3, N = 2)), 2
  )) -> table_results

  table_results %>% dplyr::mutate(PrNmorethan1in1 = round(
    1 - (PrPoisson(Lambda = table_results$Median_Intensity, T = 1, N = 0) +
      PrPoisson(Lambda = table_results$Median_Intensity, T = 1, N = 1)), 2
  )) -> table_results

  table_results %>% dplyr::mutate(PrNmorethan0in1 = round(
    1 - (PrPoisson(Lambda = table_results$Median_Intensity, T = 1, N = 0)), 2
  )) -> table_results

  # Add other data, SOB intensity!
  colnames(SOBResult$predDF)[1] <- "Shutoff.Block"

  NHPPResult %>% dplyr::select(SOB, PowerLaw1) -> NHPPResult
  colnames(NHPPResult)[1] <- "Shutoff.Block"

  asset_data <- asset_data[,colSums(is.na(asset_data))<nrow(asset_data)]

  dplyr::left_join(table_results, asset_data, by = "Asset.Number") -> table_results2


  dplyr::left_join(SOBResult$predDF, NHPPResult, by="Shutoff.Block") ->SOB_result
  SOB_result$PowerLaw1[is.na(SOB_result$PowerLaw1)]<-0

  # check for duplicates in combined,  we have.  why?
  #SOBResult[-which(duplicated(SOBResult$Shutoff.Block)), ] -> SOBResult
  dplyr::left_join(table_results2, SOBResult$predDF, by = "Shutoff.Block") -> table_results2

  data.frame(SOBData$`Shutoff Block`, SOBData$`Shutoff Number of slids`) ->SOBSLID
  colnames(SOBSLID)[1] <- "Shutoff.Block"
  colnames(SOBSLID)[2] <- "NSLIDS_in_Shutoff.Block"
  as.numeric(table_results2$Shutoff.Block) -> table_results2$Shutoff.Block
  as.numeric(SOBSLID$Shutoff.Block) -> SOBSLID$Shutoff.Block

  SOBSLID %>% dplyr::distinct(Shutoff.Block, .keep_all=TRUE) -> SOBSLID
  dplyr::left_join(table_results2, SOBSLID, by = "Shutoff.Block") -> table_results2

  table_results2[!is.na(table_results2$Median_Intensity), ] -> table_results2

  return(table_results2)
}


#' PipeIntensity
#'
#'This determines the value of alpha or intensity of failure, lambda(t) for pipes.  It assumes that alpha is constant with time and is updated based on new evidence, i.e  N and T
# here T is fixed as the years of observation, and N is number of failures for Pipe in interval T.
# Need to look at Support and modify this for a non homogeneous process, i.e alpha is replaced by a and b,  or eta and Beta.
# Need to look at non parameteric methods, i.e Beta and Dirichlet to replace the Gamma and Poisson.

#' @param DZ character
#' @param SOB integer
#' @param asset_data dataframe
#' @param work_Order_Data dataframe
#' @param cohortFailure dataframe
#' @param test_start date
#' @param test_end date
#' @param CohortID integer
#'
#' @importFrom lubridate "%within%"
#' @return
#'
#' @export
#' @examples \dontrun{
#' PipeIntensity <- function(DZ, SOB, asset_data, work_Order_Data, CohortID, cohortFailure,
#'  test_start,test_end)
#' }
PipeIntensity <- function(DZ, SOB, asset_data, work_Order_Data, CohortID, KnownCohorts, cohortFailure,  test_start,
                          test_end) {
  print(CohortID)

  CohortID %in% KnownCohorts -> knownCohort
  if(knownCohort){
  which(KnownCohorts %in%  CohortID) ->CohortNr
  cohortFailure[CohortNr, ] -> cohortFailure
  } else

  {
    cohortFailure[1,]->cohortFailure
     cohortFailure$intensity <-0.00001
     cohortFailure$FNR_power <-sample(1:10, 1)
     cohortFailure$PowerLaw1.beta <-stats::runif(1, 0.5,1)
     cohortFailure$PowerLaw1.eta <- 1000
     cohortFailure$Ti1<-8000
  }

  # Note intensity cohorts are in days,  and we've used years.  let's change this to N days but keeping variable names same
  cohortFailure$Ti1 / 366 -> Years
  # cohortFailure$Ti1-> Years

  result <- list(c())

  # check WDZ IDs from asset data are in CohortIDs from cohort analysis
  asset_data %>%
    dplyr::filter(Distribution.Zone.Name %in% DZ) %>%
    dplyr::select(I.D) -> IDsinDZ

  #
  # missingassets <- asset_data %>%
  #   dplyr::filter(Distribution.Zone.Name %in% DZ) %>%
  #   dplyr::filter(I.D %in% missingCohortIDS) %>%
  #   nrow()
  #
  CohortID %in% IDsinDZ[, ]
  int <- lubridate::interval(start = test_start, end = test_end)

  work_Order_Data %>%
    dplyr::filter(I.D %in% CohortID) %>%
    dplyr::select(Reported.Date) %>%
    dplyr::distinct(as.Date(Reported.Date)) -> Fail_Dates
  nrow(Fail_Dates) -> Nfails
  work_Order_Data %>%
    dplyr::filter(I.D %in% CohortID) -> workOrder_cohort_data

  workOrder_cohort_data %>%
    dplyr::filter(Reported.Date %within% int) %>%
    dplyr::group_by(Asset.Number) %>%
    dplyr::summarise(NTestFail = dplyr::n()) %>%
    as.data.frame() -> D2

  workOrder_cohort_data %>%
    dplyr::group_by(Asset.Number) %>%
    dplyr::summarise(NFail = dplyr::n()) %>%
    as.data.frame() -> D1

  asset_data %>%
    dplyr::filter(Distribution.Zone.Name %in% DZ) %>%
    dplyr::summarise(NAssetsInZone = dplyr::n()) -> NassetsinDZ
  asset_data %>%
    dplyr::filter(Distribution.Zone.Name %in% DZ & Shutoff.Block %in% SOB) %>%
    dplyr::summarise(NAssetsInZone = dplyr::n()) -> NassetsinDZandinfailSOB # note check if SOB is all SOBs or predicted to fail SOBs???
  # asset_data %>% dplyr::filter(Shutoff.Block %in% SOB) %>% dplyr::summarise(NAssetsInZone=n()) ->NassetsinSOB_DZ

  # Temp Data Check
  asset_data %>% dplyr::filter(Shutoff.Block %in% SOB) -> l
  asset_data %>% dplyr::filter(Distribution.Zone.Name %in% DZ & Shutoff.Block %in% SOB) -> m # this is where I am losing a large number.  SOB from AssetData not matching SOBs.  what happens if I delete SOB filter?
  asset_data %>% dplyr::filter(Distribution.Zone.Name %in% DZ) -> mm

  # mismatch between SOBs and WDZ
  setdiff(l$Asset.Number, m$Asset.Number) -> missAss
  l %>% dplyr::filter(Asset.Number %in% missAss)
  # End

  print(paste0("Total Assets in all SOBs in DZ ", DZ, " is ", as.numeric(NassetsinDZ)))

  asset_data %>%
    dplyr::filter(I.D %in% CohortID) %>%
    dplyr::summarise(Nassets = dplyr::n_distinct(Asset.Number)) -> assetspercohort
  print(paste0("Total Assets in ", CohortID, " is ", as.numeric(assetspercohort)))

  asset_data %>% dplyr::filter(I.D %in% CohortID) -> asset_cohort_data
  # why filter Shutoff Blocks in SOB,  this is causing whole cohort to go to zero.  Not sure why SOB IDs in asset_data are not being found in SOB?
  as.numeric(asset_cohort_data$Shutoff.Block) -> asset_cohort_data$Shutoff.Block

  # replace line with out SOB filter
  # asset_cohort_data %>% dplyr::filter(Distribution.Zone.Name %in% DZ) %>% filter(SHUTOFF_BLOCK1 %in% SOB) ->asset_cohort_data
  asset_cohort_data %>% dplyr::filter(Distribution.Zone.Name %in% DZ) -> asset_cohort_data

  asset_cohort_data <- asset_cohort_data[, colSums(is.na(asset_cohort_data)) < nrow(asset_cohort_data)]
  print(paste0("Total Assets in ", CohortID, " and in DZ is ", nrow(asset_cohort_data)))

  if (nrow(asset_cohort_data) > 0) {
    # check these joins
    dplyr::left_join(asset_cohort_data, D1, by = "Asset.Number") -> asset_cohort_data
    dplyr::left_join(asset_cohort_data, D2, by = "Asset.Number") -> asset_cohort_data

    asset_cohort_data$NTestFail[is.na(asset_cohort_data$NTestFail)] <- 0
    asset_cohort_data$NFail[is.na(asset_cohort_data$NFail)] <- 0

    # follow example in Watson 2004 et.al
    # Bayesian hierarchical model

    # Let h =  number of pipe fails for cohort
    # let p = prob of failure in a year for cohort)
    # Set the numer of observations,  N pipes in cohort X N years  #Updated to N days, so p much smaller
    n <- as.numeric(round(assetspercohort * Years, 0)) # need to confirm that this total assets per cohort is correct!
    # Set the number of bursts
    h <- Nfails
    # Define our likelihood function.
    # Since our model is a binomial model, we can use:
    likelihood <- function(h, n, p) {
      lh <- stats::dbinom(h, n, p)
      lh
    }
    # Set the starting value of p    #probability of single pipe in cohort failing on any given time interval
    p <- stats::runif(1, 0, 1)
    p <- h / n
    # Create an empty data.frame to store the accepted p values for each iteration.
    # Remember: "the posterior probability is just an updated version of the prior"
    posterior <- data.frame()
    # Set the lenght of the loop (Marcov Chain, number of iterations).
    nrep <- 1000

    # Start the loop (MCMC)  sample from the posterior
    for (i in 1:nrep) {
      # Obtain a new proposal value for p
      p_prime <- p + stats::runif(1, -0.05, 0.05)
      # Avoid values out of the range 0 - 1
      if (p_prime < 0) {
        p_prime <- abs(p_prime)
      }
      if (p_prime > 1) {
        p_prime <- 2 - p_prime
      }
      # Compute the acceptance proability using our likelihood function and the
      # beta(1,1) distribution as our prior probability.
      R <- likelihood(h, n, p_prime) / likelihood(h, n, p) * (stats::dbeta(p_prime, 1, 1) / stats::dbeta(p, 1, 1))
      # Accept or reject the new value of p
      if (is.na(R)) {
        0 -> R
      }
      if (R > 1) {
        R <- 1
      }
      random <- stats::runif(1, 0, 1)
      if (random < R) {
        p <- p_prime
      }
      # Store the likelihood of the accepted p and its value
      posterior[i, 1] <- log(likelihood(h, n, p))
      posterior[i, 2] <- p
      log(likelihood(h, n, p))
    }

    #hist(posterior[, 2])
    # Plots,  suppress
    plot <- FALSE

    if (plot) {
      graphics::par(mfrow = c(1, 2))
      prior <- stats::rbeta(nrep, 1, 1)
      plot(1:nrep, posterior$V2,
        cex = 0, xlab = "generations", ylab = "p",
        main = "trace of MCMC\n accepted values of parameter p\n prior = beta(1,1) generations = 5000"
      )
      graphics::lines(1:nrep, posterior$V2, cex = 0)
      graphics::abline(h = mean(posterior$V2), col = "red")
      plot(stats::density(posterior$V2),
        xlim = c(min(min(prior), min((posterior$V2))), min(max(prior), max((posterior$V2)))),
        ylim = c(0, max(max(stats::density(prior)$y), max((stats::density(posterior$V2)$y)))), main = "prior VS posterior\n prior= beta(1,1)",
        lwd = 3, col = "red"
      )
      graphics::lines(stats::density(prior), lwd = 3, lty = 2, col = "blue")
      graphics::legend("topleft",
        legend = c("prior density", "posterior density"),
        col = c("blue", "red"), lty = c(3, 1), lwd = c(3, 3), cex = 1
      )
    }

    # Gamma distributed Intensity
    # mean intensity, #Prior Intensity for cohort X
    meanIntensity <- mean(posterior[, 2])
    sdIntensity <- stats::sd(posterior[, 2])
    beta <- sdIntensity / meanIntensity
    alpha <- meanIntensity^2 / sdIntensity

    # possible issue with above is we use all available data from cohort over 0 to ~20 years and then each pipe is updated based on N failures in the same period T{0,20}
    # instead modify above to use say 10 years for cohort pipe average/sd, and use 10 years to update individual pipes
    # Alternatively, can we use previously fitted values for beta and eta for the cohort and calculate the intensity at T=1 years
    # choice for SD to work out alpha and beta in that case?

    cohortFailure$PowerLaw1.beta -> be
    cohortFailure$PowerLaw1.eta -> a2
    T0 <- 1 # or T<-365,  thought eta and beta were determined for T=days???
    T1 <- cohortFailure$Ti1
    Starting_intensity <- ((T0 / a2)^(be - 1)) * (be / a2) * 365 / as.numeric(assetspercohort) # cohort intensity normalised by N pipes,  probably should change this to length of cohort
    End_intensity2 <- ((T1 / a2)^(be - 1)) * (be / a2) * 365 / as.numeric(assetspercohort) # cohort intensity normalised by N pipes,  probably should change this to length of cohort

    avgIntensity <- (Starting_intensity + End_intensity2) / 2
    # alpha<-...
    # beta<-....

    likelihood_poisson <- function(a, N, T) {
      lh <- exp(-a * T) * ((a * T)^N / factorial(N))
      lh
    }

    # a is the intensity, alpha and beta are parameters describing the gamma distribution
    prob_gamma <- function(beta, alpha, a) {
      1 / beta -> b
      b^a * alpha^(a - 1) * exp(-alpha * b) -> p
      p
    }

    # Do For each asset...
    T <- round(Years, 0) # Note in Days
    data.frame() -> A

    for (k in 1:nrow(asset_cohort_data))

    {
      print(paste0("Asset Number ", k, " out of ", nrow(asset_cohort_data), " assets"))
      N <- asset_cohort_data$NFail[k]
      a <- stats::rgamma(1, shape = alpha, rate = 1 / beta)
      print(a)
      posterior2 <- data.frame()
      # Set the lenght of the loop (Marcov Chain, number of iterations).
      nrep <- 10000

      # Start the loop (MCMC)

      for (j in 1:nrep) {
        # sample from gamma distribution intensity a

        # Obtain a new proposal value for p
        # a_prime <- a + stats::runif(1, -0.03,0.03)
        a_prime <- a + stats::runif(1, -0.2 * a, 0.2 * a)

        # Avoid negative values 0 - 1
        if (a_prime < 0) {
          a_prime <- abs(a_prime)
        }
        # if (a_prime > 1) {a_prime <- 2 - a_prime}
        # Compute the acceptance proability using our likelihood function and the
        # beta(1,1) distribution as our prior probability.

        # Support
        R <- likelihood_poisson(a_prime, N, T) / likelihood_poisson(a, N, T) * (prob_gamma(beta, alpha, a_prime) / prob_gamma(beta, alpha, a))

        if (is.na(R)) {
          0 -> R
        }
        # Accept or reject the new value of p
        if (R > 1) {
          R <- 1
        }
        random <- stats::runif(1, 0, 1)
        if (random < R) {
          a <- a_prime
        }
        # Store the likelihood of the accepted p and its value
        posterior2[j, 1] <- log(likelihood_poisson(a, N, T))
        posterior2[j, 2] <- a
      }

      # burn first 500
      posterior2[500:nrep, 2, drop = FALSE] -> x
      colnames(x) <- "foo"


      rcompanion::groupwiseMedian(foo ~ 1,
        data = x,
        conf = 0.95,
        R = 1000,
        percentile = TRUE,
        bca = FALSE,
        basic = FALSE,
        normal = FALSE,
        wilcox = FALSE,
        digits = 3
      ) -> medians

      asset_cohort_data$Asset.Number[[k]] -> A[k, 1]
      medians$Median -> A[k, 2]
      medians$Percentile.lower -> A[k, 3]
      medians$Percentile.upper -> A[k, 4]
      N -> A[k, 5]
      asset_cohort_data$NTestFail[[k]] -> A[k, 6]
      meanIntensity -> A[k, 7]
      asset_cohort_data$I.D[[k]] -> A[k, 8]
      cohortFailure$intensity -> A[k, 9]
      cohortFailure$FNR_power -> A[k, 10]
      assetspercohort -> A[k, 11]
      test_start -> A[k, 12]
      test_end -> A[k, 13]
      nrow(asset_cohort_data) -> A[k, 14]
      colnames(A) <- c(
        "Asset.Number", "Median_Intensity", "Median_95_Lower",
        "Median_95_Upper", "NFail", "NFail_Test", "Mean_Prior_Intensity", "CohortID",
        "Cohort_Intensity", "Cohort_FNR", "N_asset_inCohort", "TestStart", "TestEnd", "NassetsinCohortinZone"
      )
      posterior2 <- NULL
    }
  } else {
    as.data.frame(matrix(NA, ncol = 14, nrow = 1)) -> A
    colnames(A) <- c(
      "Asset.Number", "Median_Intensity", "Median_95_Lower",
      "Median_95_Upper", "NFail", "NFail_Test", "Mean_Prior_Intensity", "CohortID",
      "Cohort_Intensity", "Cohort_FNR", "N_asset_inCohort", "TestStart", "TestEnd", "NassetsinCohortinZone"
    )
    data.frame(lapply(A, as.character), stringsAsFactors = FALSE) -> A
    nrow(asset_cohort_data) -> A[1, 14]
  }

  return(A)
}
