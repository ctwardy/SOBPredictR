
pipeModel <- function() {
  ###############

  # Steps 5  This approach using the following WDZ is more direct but yields a lot fewer SOBs per zone...
  read.csv("Data/SOB_WDZ_2020Update.csv") -> WDZ

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

  WDZ %>%
    group_by(Water_Distribution_Zone) %>%
    tally() %>%
    as.data.frame() -> TotSOBperZone
  left_join(predDF, WDZ, by = c("SOB" = "Shutoff_Block")) -> SOB_WDZ

  SOB_WDZ$Water_Distribution_Zone[SOB_WDZ$Water_Distribution_Zone == "UNKN"] <- NA
  SOB_WDZ$Water_Distribution_Zone[SOB_WDZ$Water_Distribution_Zone == "<NA>"] <- NA
  SOB_WDZ$Status[SOB_WDZ$Status == "DECOMMISSIONED"] <- NA
  skimr::skim(SOB_WDZ)

  SOB_WDZ[complete.cases(SOB_WDZ), ] -> SOB_WDZ

  SOB_WDZ %>%
    filter(Prediction == newPosClassLabel) %>%
    group_by(Water_Distribution_Zone) %>%
    tally() %>%
    as.data.frame() -> SOBPredperWDZ
  left_join(TotSOBperZone, SOBPredperWDZ, by = "Water_Distribution_Zone") -> SOBPredperWDZ

  SOBPredperWDZ$n.y[is.na(SOBPredperWDZ$n.y)] <- 0
  SOBPredperWDZ %>%
    mutate(propSOBinWDZ = n.y / n.x) %>%
    arrange(desc(propSOBinWDZ)) %>%
    filter(n.x > 50) -> SOBPredperWDZ

  Zones <- head(SOBPredperWDZ$Water_Distribution_Zone, 10)

  SOB_WDZ %>%
    filter(Water_Distribution_Zone %in% Zones) %>%
    as.data.frame() -> Final

  write.csv(Final, paste0("Outputs/", subDir, "/WDZ_table_Nfail=", Nfailcutoff, "_ModelledYearEnd_", subDir, "_", substr(subD, 6, 24), ".csv"))

  ####  Pipe Modeling Start

  # #Step 6
  # #Beginning of a modified 3 chunks from the main code. Repeat the pipe analysis as part of backtesting workflow.
  #
  # #Nfailcutoff<-2
  # #subDir <-2018
  #
  # #idea is to predict pipe probability using data up until the start of the prediction year
  #
  # #from earlier
  # read.csv(paste0("Outputs/", subDir, "/Full_table_Nfail=", Nfailcutoff,"_ModelledYearEnd_", subDir, "_",substr(subD,6,24),".csv"))->predDF
  #
  #
  # # Note inclusion of DZ zone, to reduce number of assets.
  file.path(newDir, subDir, paste0("NHPPInputWO_Cohort_", subDir, ".RDS")) -> path1
  file.path(newDir, subDir, paste0("NHPPInputAssetCohort_", subDir, ".RDS")) -> path2
  file.path("Outputs", subDir, paste0("Table1NHPP_cohorts_", subDir, ".RDS")) -> path4

  readRDS(path1) -> WOD
  readRDS(path2) -> AD
  readRDS(path4) -> CF

  # temp check this only picks up 73 assets in AD in Woori Yallock,  #SOBs is ~303!
  table(AD$Distribution.Zone.Name %in% "Woori Yallock PR")

  #
  # WOD %>% filter(Class.Structure %in% "Repair Burst Water Main" |
  #                  Class.Structure %in% "Repair Leaking Water Main") %>% filter(Reported.Date<val_start) -> WOD
  #
  AD %>% filter(Class.Structure %in% "Drinking Water Pipes") -> AD
  AD %>% mutate(Pipe.Material = replace(Pipe.Material, Pipe.Material == "", "NA")) -> AD
  AD$ID <- paste0(AD$Pipe.Material, ", ", AD$Install.Date)
  #
  #  unique(WOD$Distribution.Zone.Name)->DZs
  #  length(unique(WOD$Distribution.Zone.Name))->NDZs
  # #
  # # #Fix Distribution Zone Name Errors in WO table
  # # WOD$Distribution.Zone.Name <- gsub('P.R.', 'PR', WOD$Distribution.Zone.Name)
  # # WOD$Distribution.Zone.Name <- gsub('P.R', 'PR', WOD$Distribution.Zone.Name)
  # # WOD$Distribution.Zone.Name <- gsub('Res.', 'Res', WOD$Distribution.Zone.Name)
  # # WOD$Distribution.Zone.Name <- gsub('Res..', 'Res', WOD$Distribution.Zone.Name)
  # # WOD$Distribution.Zone.Name <- gsub('P/B', 'PB', WOD$Distribution.Zone.Name)
  # # WOD$Distribution.Zone.Name <- gsub('Creek', 'Ck', WOD$Distribution.Zone.Name)
  # # WOD$Distribution.Zone.Name <- gsub('P.B', 'PB', WOD$Distribution.Zone.Name)
  # # WOD$Distribution.Zone.Name <- gsub('Olinda - Mitcham', 'Olinda Mitcham', WOD$Distribution.Zone.Name)
  # # WOD$Distribution.Zone.Name <- gsub('Resoir', 'Reservoir', WOD$Distribution.Zone.Name)
  # # WOD$Distribution.Zone.Name <- gsub('Resch', 'Research', WOD$Distribution.Zone.Name)
  # # WOD$Distribution.Zone.Name <- gsub('PRV', 'PR', WOD$Distribution.Zone.Name)
  # # WOD$Distribution.Zone.Name <- gsub(' No ', 'Res ', WOD$Distribution.Zone.Name)
  # #
  # # saveRDS(WOD, paste0("C:/Users/bgladman/OneDrive/DataProjects/YVW/waterAssetAnal/Outputs/WOD_WDZ_",subDir,".RDS"))
  # #
  # Fix Distribution Zone Name Errors in Asset table
  AD$Distribution.Zone.Name <- gsub("P.R.", "PR", AD$Distribution.Zone.Name)
  AD$Distribution.Zone.Name <- gsub("P.R", "PR", AD$Distribution.Zone.Name)
  AD$Distribution.Zone.Name <- gsub("Res.", "Res", AD$Distribution.Zone.Name)
  AD$Distribution.Zone.Name <- gsub("Res..", "Res", AD$Distribution.Zone.Name)
  AD$Distribution.Zone.Name <- gsub("P/B", "PB", AD$Distribution.Zone.Name)
  AD$Distribution.Zone.Name <- gsub("Creek", "Ck", AD$Distribution.Zone.Name)
  AD$Distribution.Zone.Name <- gsub("P.B", "PB", AD$Distribution.Zone.Name)
  AD$Distribution.Zone.Name <- gsub("Olinda - Mitcham", "Olinda Mitcham", AD$Distribution.Zone.Name)
  AD$Distribution.Zone.Name <- gsub("Resoir", "Reservoir", AD$Distribution.Zone.Name)
  AD$Distribution.Zone.Name <- gsub("Resch", "Research", AD$Distribution.Zone.Name)
  AD$Distribution.Zone.Name <- gsub("PRV", "PR", AD$Distribution.Zone.Name)
  AD$Distribution.Zone.Name <- gsub(" No ", "Res ", AD$Distribution.Zone.Name)

  saveRDS(AD, paste0("Outputs/AD_WDZ_", subDir, ".RDS"))
  # # #
  # # # #Top 5 WDZs as ratio of SOBs predicted to fail to total Number of SOB in WDZ
  # #
  # # #Join NHPP output and SOB predictions
  # # file.path("Outputs", subDir,  paste0("Table1NHPP_",subDir,".RDS"))->path6
  # # readRDS(path6)->combined.df
  # #
  # # left_join(predDF, combined.df, by="SOB")->table2   #Note this is a left join...
  # #
  # # #Fix Distribution Zone Name Errors in Asset table
  # # table2$WDZ <- gsub('P.R.', 'PR', table2$WDZ)
  # # table2$WDZ <- gsub('P.R', 'PR', table2$WDZ)
  # # table2$WDZ <- gsub('Res.', 'Res', table2$WDZ)
  # # table2$WDZ <- gsub('Res..', 'Res', table2$WDZ)
  # # table2$WDZ <- gsub('P/B', 'PB', table2$WDZ)
  # # table2$WDZ <- gsub('Creek', 'Ck', table2$WDZ)
  # # table2$WDZ <- gsub('P.B', 'PB', table2$WDZ)
  # # table2$WDZ <- gsub('Olinda - Mitcham', 'Olinda Mitcham', table2$WDZ)
  # # table2$WDZ <- gsub('Resoir', 'Reservoir', table2$WDZ )
  # # table2$WDZ <- gsub('Resch', 'Research', table2$WDZ )
  # # table2$WDZ <- gsub('Sub Zone ', 'Res', table2$WDZ )
  # # table2$WDZ <- gsub(' No ', 'Res ', table2$WDZ )
  # #
  # # table2 %>% filter(Prediction %in% "2 or more")->table4  #any SOB predicted to fail repeatedly
  # # #table2 %>% filter(Prob.Positive.Outcome> 0.80)->table4  #all SOBs predicted to fail with probability > 0.95
  # #
  # # table4 %>% group_by(WDZ) %>% summarize(numberofSOBs=n()) ->NpredictedSOBS
  # #
  # # colnames(NpredictedSOBS)[1] <-"Distribution.Zone.Name"
  # # as.character(NpredictedSOBS$Distribution.Zone.Name)->NpredictedSOBS$Distribution.Zone.Name
  # #
  # # #How many SOBs in WDZ???,  use Asset Data/Static Data  (Asset Data missing some assets)
  # # #use static pressure table instead/
  # #
  # # #using Asset Data AD to provide WDZ from SOB mapping.
  # # AD %>% dplyr::select(Asset.Number,Distribution.Zone.Name, ID)->AD2  #, Shutoff.Block
  # # colnames(AD2)[1] <-"ASSETID"
  # #
  # # fread("Data/YVW_WATER_PIPE_Static_Pressure.csv", header=TRUE, sep=",")->AD3
  # #
  # # #read in Asset Number/WDZ data
  read.csv("Data/Distribution Zones For Pipe Asset.csv") -> WDZID

  length(unique(WDZID$Asset.Number))

  # further check from above...
  table(WDZID$Water.Distribution.Zone == "Woori Yallock PR") # 804 assets in this table
  WDZID$Asset.Number[WDZID$Water.Distribution.Zone %in% "Woori Yallock PR"] -> AN
  data.frame(Asset.Number = AN) -> AN
  as.character(AN$Asset.Number) -> AN$Asset.Number
  left_join(AN, AD, by = "Asset.Number") # shows that the WDZ in AD is wrong/not in agreement with the WDZ/AssetID table, additionally only 133000 records in AD versus 266000 in WDZID

  as.character(WDZID$Asset.Number) -> WDZID$Asset.Number
  left_join(WDZID, AD, by = "Asset.Number") -> tempss # the WDZ in AD is wrong/not in agreement with the WDZ/AssetID table, additionally only 133000 records in AD versus 266000 in WDZID
  tempss[is.na(tempss$Shutoff.Block), ] # missing from AD why??

  AD %>% filter(Asset.Number == 10008) # missing
  AD3 %>% filter(ASSETID %in% 628322)
  WDZID %>% filter(Asset.Number %in% 10008)
  WDZID %>% filter(Asset.Number %in% 10023)
  AD %>% filter(Asset.Number == 10023) # missing
  class(AD$Asset.Number)
  # #
  # # WDZID[,c(2,4)] ->WDZID
  # # colnames(WDZID)[1]<-"ASSETID"
  # # colnames(WDZID)[2]<-"Distribution.Zone.Name"
  # #
  # # #Added this, not sure if this DF has problems with naming as per Assset/WOs???
  # #
  # # as.factor(WDZID$Distribution.Zone.Name) ->WDZID$Distribution.Zone.Name
  # # as.factor(table2$WDZ)->table2$WDZ
  # #
  # # #Fix Distribution Zone Name Errors in Asset table
  # # WDZID$Distribution.Zone.Name <- gsub('P.R.', 'PR', WDZID$Distribution.Zone.Name)
  # # WDZID$Distribution.Zone.Name <- gsub('P.R', 'PR', WDZID$Distribution.Zone.Name)
  # # WDZID$Distribution.Zone.Name <- gsub('Res.', 'Res', WDZID$Distribution.Zone.Name)
  # # WDZID$Distribution.Zone.Name <- gsub('Res..', 'Res', WDZID$Distribution.Zone.Name)
  # # WDZID$Distribution.Zone.Name <- gsub('P/B', 'PB', WDZID$Distribution.Zone.Name)
  # # WDZID$Distribution.Zone.Name <- gsub('Creek', 'Ck', WDZID$Distribution.Zone.Name)
  # # WDZID$Distribution.Zone.Name <- gsub('P.B', 'PB', WDZID$Distribution.Zone.Name)
  # # WDZID$Distribution.Zone.Name <- gsub('Olinda - Mitcham', 'Olinda Mitcham', WDZID$Distribution.Zone.Name)
  # # WDZID$Distribution.Zone.Name <- gsub('Resoir', 'Reservoir', WDZID$Distribution.Zone.Name)
  # # WDZID$Distribution.Zone.Name <- gsub('Resch', 'Research', WDZID$Distribution.Zone.Name)
  # # WDZID$Distribution.Zone.Name <- gsub('Sub Zone ', 'Res', WDZID$Distribution.Zone.Name)
  # # WDZID$Distribution.Zone.Name <- gsub(' No ', 'Res ', WDZID$Distribution.Zone.Name)
  # #
  # #
  # # #as.character(WDZID$ASSETID)->WDZID$ASSETID
  # # #left_join(AD3, WDZID, by="ASSETID") -> AD_final  # revert to this if other reason for missing assets
  # # left_join(WDZID, AD3,  by="ASSETID") -> AD_final  #more records in WDZID but Hadfield crashed
  # #
  # # #left_join(AD3, AD2, by="ASSETID") -> AD_final
  # # #left_join(AD3, AD2, by="SHUTOFF_BLOCK") -> AD_final
  # # nrow(WDZID)
  # # nrow(AD_final)
  # #
  # # AD_final[is.na(AD_final$SHUTOFF_BLOCK),]
  # # AD_final %>% group_by(Distribution.Zone.Name) %>% summarize(TotalnumberofSOBs=n()) ->totalNSOBS
  # #
  # # left_join(NpredictedSOBS, totalNSOBS, by="Distribution.Zone.Name")->WDZTable
  # # WDZTable %>% mutate(prop=numberofSOBs/TotalnumberofSOBs) %>% arrange(desc(prop,TotalnumberofSOBs)) ->WDZTable
  # # sum(WDZTable$TotalnumberofSOBs)
  # # write.csv(WDZTable, paste0("Outputs/WDZSOBtable_",subDir,".csv"))
  #
  #
  #
  # #Select one or more zones for possible trial
  #
  # #Zones<-c("Broadmeadows Res", "Mitcham Morang", "Glenroy PR","Hadfield PR","Warburton PR")
  # #Hand pick zones with a high prop and reasonable number of SOBS
  # #include 2020 trial zones for comparison   Broad, Doncast Nth, GladSt Pk, Hdfield,
  # #Monbulk Res, Mor Res, Warbr PR, Warranwood PB,
  # #Woori Yallock PR YY Epp  YarraJunction PR
  #
  #
  # #Zones<-WDZTable$Distribution.Zone.Name[c(6,7,10,13,15,16,17,18,19,20,26,28,31,32,44,45)]
  # #list.files("C:/Users/bgladman/OneDrive/DataProjects/YVW/waterAssetAnal/Outputs/Trial_FINAL_2017") ->Zones
  # #substr(Zones,1,nchar(Zones)-4)->Zones
  #
  # c("Broadmeadows Res","Croydon North PR","Doncaster North PR","Doncaster South PR","Gladstone Park PR","Glenroy PR","Hadfield PR","Mitcham Morang","Morang Res","North Balwyn PR","Reservoir PR","Surrey Hills (LornePde) PB",
  #   "Warburton PR","Warranwood PB","Woori Yallock PR","Yan Yean Epping (CooperSt)")->Zones
  #
  # table4 %>% filter(WDZ %in% Zones) ->FinalShortList
  #
  # FinalShortList$WDZ
  # as.data.frame(table(FinalShortList$WDZ))->tempDF
  # colnames(tempDF)[1]<-"WDZ"
  # filter(tempDF, Freq>0)->tempDF
  #
  # paste0(AD_final$MATERIAL, ", ", substr(AD_final$CONST_DATE, 1, 4))->AD_final$I.D
  #
  # #Quality Check... eg. Hadfield PR has more predicted SOBs than total SOBs based on Asset Data???, got total using Static Pressure ???
  #
  # #FinalShortList %>% filter(WDZ %in% "Hadfield PR") %>% distinct(SOB)  %>% dplyr::select(SOB)  #->SOBinmodel
  # #AD %>% filter(Distribution.Zone.Name %in% "Hadfield PR")  %>% dplyr::select(Shutoff.Block) -> SOBinAsset
  #
  # #class(SOBinmodel[,])
  # #as.numeric(SOBinAsset[,])->SOBinAsset[,]

  #####

  # Single Pipe Model for each WDZ included as part of a Trial targetting susceptible SOBs.

  # Top Six - Need relationship between WDZ name and DZID in Static Pressure DF,  have this table now.   Using Static Pressure DF instead of Asset DF due to gaps in former.  Need to check the Static Pressure table for accuracy and completeness,

  # From the results of SOB prediction, the 6 WDZs were chosen with the highest proportion of predicted to fail SOB.  Predicted to fail SOBS were filtered again to only include those with a class Pr>0.96

  # This is relatively slow, takes a few hours to do 5-6 WDZs depending on the size of the zone

  Zones -> DZs

  # only for testing
  # DZ = DZs[[1]]
  # cohortFailure=CF
  # asset_data=AD
  # asset_data2=AD_final
  # work_Order_Data=WOD
  # i=1
  # test_start=test_start  #defined in CohortNHPP chunk
  # test_end=test_end  #defined in CohortNHPP chunk
  # cohortNr=i
  # SOB=FinalShortList$SOB  #This has only SOB IDS in classified problem SOB
  # SOB=table2$SOB  # This has all SOBS IDS (in chosen zones?)

  # start loop,  cycles through selected DZs and calculates the individual asset intensities in the selected SOBS for each cohort type.


  paste0("Outputs/Trial_FINAL_", subDir) -> TrialDir
  paste0("Trial_FINAL_", subDir) -> TrialName

  cohort_IDs <- unique(CF$Cohort)

  # table2 select all SOBS in zone(s), filter out predicted to fail later
  for (j in 1:length(DZs)) {
    print(paste0("Running Analysis on DZ ", DZs[[j]]))

    list(data.frame()) -> result

    # Pipe failure Intensity for all Cohorts i in DZ j
    # how much is the prior distribution influencing the posterior intensity???  how much does the gamma sampling contribute to the variance between assets versus the individual asset failure
    for (i in 1:length(cohort_IDs)) {

      # Use table2$SOBs to calculate intensity of all assets in trial zone(s), not just thos in SOBs predited to fail by model.

      print(paste0("Running Cohort ", i))

      DZ <- DZs[[j]]
      SOB <- predDF$SOB
      cohortFailure <- CF
      asset_data2 <- AD_final
      asset_data2 <- AD
      work_Order_Data <- WOD
      cohortNr <- i
      test_start <- test_start
      test_end <- test_end


      tryCatch(
        {
          suppressWarnings(PipeIntensitycohortV3(
            DZ = DZs[[j]],
            SOB = table2$SOB,
            cohortFailure = CF,
            asset_data2 = AD,
            work_Order_Data = WOD,
            cohortNr = i,
            test_start = test_start,
            test_end = test_end
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

    ZoneResult %>% filter(NassetsinCohortinZone > 0) -> ZoneResult
    ZoneResult$DZ <- DZs[[j]]
    colnames(ZoneResult)
    # ZoneResult[!is.na(ZoneResult$Asset.Number),]->ZoneResult
    lapply(ZoneResult[, -c(1, 8, 12, 13, 15)], as.numeric) -> ZoneResult[, -c(1, 8, 12, 13, 15)]


    filename <- paste0(getwd(), "/Outputs/BackTestPipes/", gsub(" ", "", DZs[[j]], fixed = TRUE), ".RDS")

    summary(ZoneResult)
    saveRDS(ZoneResult, filename)

    ZoneResult %>% arrange(desc(Median_Intensity), desc(NFail_Test)) -> ZoneResult

    # tempdelete
    # unique(ZoneResult$CohortID)->modelIDS
    # unique(IDsinDZ[,]) ->WDZIDS
    # WDZIDS %in% modelIDS
    # eg   "MEDIUM DENSITY POLYETHYLENE, 1994" is a WDZ cohort but zero assets
    # grep("MEDIUM DENSITY POLYETHYLENE, 1994",cohort_IDs )
  }

  # Consolidate all WDZ results...  Note at moment need to manually copy RDS files from /Outputs

  multiplezones <- (length(DZs) > 1)
  if (multiplezones) {
    filenames <- list.files(path = paste0("Outputs/Trial_FINAL_", subDir), pattern = "\\.RDS$", full.names = TRUE)
    file.info(filenames) -> details
    details <- details[with(details, order(as.POSIXct(mtime))), ]
    filenames <- rownames(details)

    r <- lapply(filenames, readRDS)

    dropCols <- function(x) {
      x %>% dplyr::select(-c("NFail_Test", "TestEnd", "TestStart")) -> x
    }

    forecastonly <- TRUE
    if (forecastonly) {
      r <- lapply(r, dropCols)
    }
    do.call(rbind, r) -> table_results
  } else {
    (ZoneResult -> table_results)
  }

  as.numeric(table_results$Median_Intensity) -> table_results$Median_Intensity

  table_results %>% arrange(desc(Median_Intensity)) -> table_results

  # check for duplicated,  remove duplicates need to work out why they are appearing in the results, mostly NA

  if (any(is.na(table_results$Asset.Number))) {
    table_results[-which(is.na(table_results$Asset.Number)), ] -> table_results
  }
  # Add various Probabilities of N in T to results table,  Note the intensity values T are in Years, N here refers to combined burst and leak

  PrPoisson <- function(Lambda, N, T) {
    exp(-Lambda * T) * (Lambda * T)^N / factorial(N) -> x
    return(x)
  }

  table_results %>% mutate(PrNmorethan2in1 = round(
    1 - (PrPoisson(Lambda = table_results$Median_Intensity, T = 1, N = 0) +
      PrPoisson(Lambda = table_results$Median_Intensity, T = 1, N = 1) +
      PrPoisson(Lambda = table_results$Median_Intensity, T = 1, N = 2)), 2
  )) -> table_results

  table_results %>% mutate(PrNmorethan2in3 = round(
    1 - (PrPoisson(Lambda = table_results$Median_Intensity, T = 3, N = 0) +
      PrPoisson(Lambda = table_results$Median_Intensity, T = 3, N = 1) +
      PrPoisson(Lambda = table_results$Median_Intensity, T = 3, N = 2)), 2
  )) -> table_results

  table_results %>% mutate(PrNmorethan1in1 = round(
    1 - (PrPoisson(Lambda = table_results$Median_Intensity, T = 1, N = 0) +
      PrPoisson(Lambda = table_results$Median_Intensity, T = 1, N = 1)), 2
  )) -> table_results

  table_results %>% mutate(PrNmorethan0in1 = round(
    1 - (PrPoisson(Lambda = table_results$Median_Intensity, T = 1, N = 0)), 2
  )) -> table_results

  colnames(table_results)[1] <- "ASSETID"
  as.numeric(table_results$ASSETID) -> table_results$ASSETID

  as.numeric(AD_final$ASSETID) -> AD_final$ASSETID

  # Add SOB intensity!
  readRDS("Outputs/Table1NHPP_updatetoPresent.RDS") -> combined.df
  combined.df %>% dplyr::select(SOB, PowerLaw1) -> combined.df
  colnames(combined.df)[1] <- "SHUTOFF_BLOCK"

  left_join(table_results, AD_final, by = "ASSETID") -> table_results2
  nrow(table_results2)

  # check for duplicates in combined,  we have.  why?
  combined.df[-which(duplicated(combined.df$SHUTOFF_BLOCK)), ] -> combined.df

  left_join(table_results2, combined.df, by = "SHUTOFF_BLOCK") -> table_results2
  nrow(table_results2) # this adds rows,  why?

  readRDS("Outputs/SLIDSOB.RDS") -> SOBSLID
  colnames(SOBSLID)[1] <- "SHUTOFF_BLOCK"

  as.numeric(table_results2$SHUTOFF_BLOCK) -> table_results2$SHUTOFF_BLOCK
  as.numeric(SOBSLID$SHUTOFF_BLOCK) -> SOBSLID$SHUTOFF_BLOCK

  left_join(table_results2, SOBSLID) -> table_results2

  table_results2 %>% mutate(SOBpredictedtoFail = ifelse(SHUTOFF_BLOCK %in% FinalShortList$SOB, TRUE, FALSE)) -> table_results2

  nrow(table_results2)

  # Is there a difference in the median intensity of assets in the SOBs predicted to fail repeatedly.
  # Yes for the example of Broadmeadows
  table_results2 %>%
    group_by(SOBpredictedtoFail) %>%
    summarize(mean(Median_Intensity, na.rm = TRUE))
  table_results2 %>%
    group_by(DZ) %>%
    summarize(mean(Median_Intensity, na.rm = TRUE))
  table_results2 %>%
    group_by(CohortID) %>%
    summarize(Mean = mean(Median_Intensity, na.rm = TRUE)) %>%
    arrange(desc(Mean))

  table_results2[!is.na(table_results2$Median_Intensity), ] -> table_results2
  colnames(table_results2)[c(1:16, 22, 23, 24, 27, 28, 32, 36, 98:103, 117, 122, 123)] -> sel
  table_results2[, sel] -> table_results23

  write.csv(table_results23, paste0("Outputs/pipepredictedresult_", subDir, ".csv"))
  saveRDS(table_results23, paste0("Outputs/ZonePipePredictions_", subDir, ".RDS"))
  hist(table_results23$Median_Intensity)

  ###
  ZoneAnalysis <- function(ZoneNr) {

    # Step 1 Filter all pipes below a threshold.  How many pipes and SOBS

    readRDS(paste0("Outputs/ZonePipePredictions_", subDir, ".RDS")) -> result

    fread("Data/SOB_Asset.csv", header = TRUE, sep = ",", fill = TRUE) -> SOBAsset
    setDF(SOBAsset) -> SOBAsset
    as.numeric(SOBAsset$`Asset Number`) -> SOBAsset$`Asset Number`

    colnames(SOBAsset)[1] <- "Asset.Number"
    colnames(SOBAsset)[4] <- "Shutoff.Block"
    colnames(SOBAsset)[3] <- "Service.Location.Id"

    SOBAsset[!grepl(",", SOBAsset$Shutoff.Block), ] -> SOBs_2 # Assets/SLIDS belonging to one SOB
    SOBAsset[grepl(",", SOBAsset$Shutoff.Block), ] -> SOBs_Sub # Assets/SLIDS belonging to multiple SOBS

    # tidyr function to separate rows
    separate_rows(SOBs_Sub, Shutoff.Block, sep = ",") -> SOBs_3
    SOBs_3 %>%
      dplyr::select(Shutoff.Block) %>%
      distinct() %>%
      as.data.frame() -> duplicatedSOBS # this is the list of the SOBS which share assets
    SOBs_3 %>%
      dplyr::select(Asset.Number) %>%
      distinct() %>%
      as.data.frame() -> duplicatedAssets # these are the assets belonging to more than one SOB
    as.numeric(SOBs_3$Shutoff.Block) -> SOBs_3$Shutoff.Block

    SOBs_3 %>%
      filter(Shutoff.Block %in% as.numeric(duplicatedSOBS[, ])) %>%
      count(Shutoff.Block)

    # Add together
    rbind(SOBs_2, SOBs_3)[, c(1, 4)] -> SOBs # introduces duplicated SLIDS/Assets
    colnames(SOBs)[1] <- "ASSETID"

    left_join(result, SOBs, by = "ASSETID") -> result

    result$SHUTOFF_BLOCK <- ifelse(is.na(result$Shutoff.Block), result$SHUTOFF_BLOCK, result$Shutoff.Block)
    result$Shutoff.Block <- NULL
    # Choose DZ
    readRDS(paste0("C:/Users/bgladman/OneDrive/DataProjects/YVW/waterAssetAnal/Outputs/NHPPResults/NHPPInputWO_Cohort_", subDir, ".RDS")) -> WOD

    WOD %>% filter(Distribution.Zone.Name %in% Zones[ZoneNr]) -> WOD
    WOD %>%
      group_by(lubridate::year(Reported.Date)) %>%
      summarize("Number" = n()) -> yeartotals
    colnames(yeartotals) <- c("Year", "Number")
    ggplot(data = yeartotals, aes(x = Year, y = Number, group = 1)) +
      geom_line() +
      geom_point() -> P1

    mean(tail(yeartotals$Number, 6), na.rm = TRUE) -> AvBurstsinZone

    WOD %>%
      group_by(lubridate::year(Reported.Date)) %>%
      summarise(NoG2 = sum(NSLIDS_1 > 0, na.rm = TRUE) + sum(NSLIDS_2 > 0, na.rm = TRUE), G2 = sum(NSLIDS_3 > 0, na.rm = TRUE) + sum(NSLIDS_4 > 0, na.rm = TRUE) + sum(NSLIDS_5 > 0, na.rm = TRUE) + sum(NSLIDS_6 > 0, na.rm = TRUE) + sum(NSLIDS_7 > 0, na.rm = TRUE)) -> G2

    colnames(G2) <- c("Year", "NumberNonG2events", "NumberG2events")

    ggplot(data = G2, aes(x = Year)) +
      geom_line(aes(y = NumberNonG2events, colour = "Near G2")) +
      geom_line(aes(y = NumberG2events, colour = "G2")) -> P2
    mean(tail(G2$NumberG2events, 7), na.rm = TRUE) -> AvG2eventsinZone

    WOD %>%
      group_by(lubridate::year(Reported.Date)) %>%
      summarise(NoG2 = sum(NSLIDS_1 [NSLIDS_1 > 0], na.rm = TRUE) + sum(NSLIDS_2 [NSLIDS_2 > 0], na.rm = TRUE), G2 = sum(NSLIDS_3 [NSLIDS_3 > 0], na.rm = TRUE) + sum(NSLIDS_4 [NSLIDS_4 > 0], na.rm = TRUE) + sum(NSLIDS_5 [NSLIDS_5 > 0], na.rm = TRUE) + sum(NSLIDS_6 [NSLIDS_6 > 0], na.rm = TRUE) + sum(NSLIDS_7 [NSLIDS_7 > 0], na.rm = TRUE)) -> G3

    colnames(G3) <- c("Year", "NumberNonG2SLIDS", "NumberG2SLIDS")

    ggplot(data = G3, aes(x = Year)) +
      geom_line(aes(y = NumberNonG2SLIDS, colour = "Non G2 SLIDS")) +
      geom_line(aes(y = NumberG2SLIDS, colour = "G2 SLIDS")) -> P3

    result %>% filter(DZ %in% Zones[ZoneNr]) -> result

    unique(result$SHUTOFF_BLOCK)
    sum(result$NFail, na.rm = TRUE) -> totalNFailsinDZ

    length(unique(result$ASSETID)) -> nAssetsinZone
    sum(result$PIPE_LENGTH, na.rm = TRUE) -> totalLengthinZone

    result %>%
      distinct(CohortID, .keep_all = TRUE) %>%
      mutate(temp = (NassetsinCohortinZone / N_asset_inCohort) * Cohort_FNR) %>%
      summarize(sum(temp)) %>%
      as.numeric() -> approxFNbursts # aportioning the Cohort FNR to the fraction of Cohort in Zone.  Gives number of bursts in that zone for that cohort.

    # filter out SOBs not predicted to fail
    result %>% filter(SOBpredictedtoFail %in% TRUE) -> result

    # filter out bottom 95% percentile
    result %>% filter(PrNmorethan0in1 > .10) -> result
    nrow(result)
    # need to check that SOB IDs are correct
    result %>%
      group_by(SHUTOFF_BLOCK) %>%
      summarize(Intensity = mean(Median_Intensity, na.rm = TRUE)) -> A
    result %>%
      group_by(SHUTOFF_BLOCK) %>%
      summarize(Npipes = n()) -> B
    result %>%
      group_by(SHUTOFF_BLOCK) %>%
      summarize(Length = sum(PIPE_LENGTH)) -> C

    cbind(A, B[, 2], C[, 2]) -> SOBTable
    nrow(SOBTable)

    SOBTable %>%
      mutate(NpipeIntensity = Intensity * Npipes) %>%
      arrange(desc(NpipeIntensity)) -> SOBTable

    SOBTable %>% filter(!Npipes < 1) -> SOBTable

    maxLength <- 650000 # we cut at 5000 m
    SOBTable %>%
      mutate(cumLength = SOBTable$Length %>% cumsum()) %>%
      filter(cumLength < maxLength) %>%
      dplyr::select(-cumLength) -> SOBTable

    FinalSOBs <- SOBTable$SHUTOFF_BLOCK
    result %>% filter(SHUTOFF_BLOCK %in% FinalSOBs) -> result2
    sum(result2$PIPE_LENGTH) -> totalLength
    unique(result2$SHUTOFF_BLOCK) -> totalNSOB
    sum(result2$totalsLID, na.rm = TRUE) -> totalSLID
    sum(result2$NFail, na.rm = TRUE) -> totalNFailinselected

    list(DF = result2, DF2 = SOBTable, totalSLIDS = totalSLID, Lengthtoreplace = totalLength, SOBs = FinalSOBs, approxNbursts = approxFNbursts, totalNFailinselected = totalNFailinselected, totalNFailsinDZ = totalNFailsinDZ, totL = totalLengthinZone, AvgBursts = AvBurstsinZone, AvgG2 = AvG2eventsinZone, NassetsinZone = nAssetsinZone, P1, P2, P3) -> out

    return(out)
  }

  zoneresults <- list()
  for (k in 1:length(Zones)) {
    print(k)
    ZoneAnalysis(ZoneNr = k) -> zoneresults[[k]]
  }

  saveRDS(zoneresults, paste0("Outputs/ZoneResults_", subDir, ".RDS"))

  out <- list()
  for (l in 1:length(Zones)) {
    newDF <- matrix()
    zoneresults[[l]]$totalSLIDS -> newDF$totalSLIDSinTargetedSOBS
    zoneresults[[l]]$Lengthtoreplace -> newDF$Lengthtoreplace
    length(zoneresults[[l]]$SOBs) -> newDF$SOBsTargeted
    nrow(zoneresults[[l]]$DF) -> newDF$nAssetsReplace
    zoneresults[[l]]$AvgBursts -> newDF$AvgBursts
    zoneresults[[l]]$AvgG2 -> newDF$AvgG2
    zoneresults[[l]]$NassetsinZone -> newDF$NassetsinZone
    zoneresults[[l]]$totL -> newDF$totalLengthinZone
    zoneresults[[l]]$totalNFailinselected -> newDF$totalNFailinselected
    zoneresults[[l]]$totalNFailsinDZ -> newDF$totalNFailsinDZ

    as.data.frame(newDF) -> out[[l]]
  }

  zoneresults[[4]]$DF
  do.call(rbind, out) -> r
  r[, 1] <- Zones
  # r %>% mutate(burstsper100km=AvgBursts/(totalLengthinZone/1000)*100) -> r
  r %>% mutate(SLIDSperLength = totalSLIDSinTargetedSOBS / (Lengthtoreplace)) -> r
  r %>% mutate(SOBsperLength = SOBsTargeted / (Lengthtoreplace)) -> r
  r %>% mutate(burstsper100kmpredicted = (totalNFailsinDZ - totalNFailinselected) / 24 / (totalLengthinZone / 1000) * 100) -> r
  r %>% mutate(burstsper100km = (totalNFailsinDZ) / 24 / (totalLengthinZone / 1000) * 100) -> r

  write.csv(r, paste0("Outputs/summaryTRialDMZ_", subDir, ".csv"))
}
