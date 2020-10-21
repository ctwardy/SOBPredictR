#internal function,  note that this produces a smaller DF containing only workorders with a known outage.
calcrepeat <- function(SOBs, DF, listofYears) {
  SOBs %>% dplyr::filter(Shutoff.Block != "") -> SOBss
#
# Calculate the total number of SLIDS for each SOB (appearing in Workorder DF?)
  SOBss %>%
    dplyr::group_by(Shutoff.Block) %>%
    dplyr::summarize(Count = dplyr::n()) %>%
    dplyr::arrange(dplyr::desc(Count)) %>%
    as.data.frame() -> SOBlist


  year_repeats <- list()

  # set true here so only report water off for G2 calc
  DF %>%
    dplyr::filter(Number.of.Work.Order.Water.Outages > 0) %>%
    as.data.frame() -> DFtemp

  # Following code chunk gets number of repeats 2,3..7 in the following list of periods.

    for (k in length(listofYears):1) {
      print(paste0("Year", k))
      Date1 <- as.Date(listofYears[[k]][[1]])
      Date2 <- as.Date(listofYears[[k]][[2]])

      DFtemp %>% dplyr::filter(as.Date(Reported.Date) > Date1 & as.Date(Reported.Date) < Date2) -> dateFilteredDF
      SOBs[!is.na(SOBs$Shutoff.Block), ] -> SOBss

      # see note 27/09, kept as numeric as slow otherwise
      as.numeric(SOBss$Shutoff.Block) -> SOBss$Shutoff.Block

      SOBss[SOBss$Shutoff.Block %in% unique(dateFilteredDF$Shutoff.Block), ] -> SOBss

      # add new lines here to point to new SOBS prior to joining with SOB
      dateFilteredDF %>%
        dplyr::arrange(Reported.Date) %>%
        dplyr::group_by(Work.Order.Number) %>%
        dplyr::select(Work.Order.Number, Service.Location.Id, Asset.Number) %>%
        dplyr::mutate(NdistinctSLIDS = dplyr::n()) %>%
        as.data.frame() -> dateFilteredDF2

      ## end new code

      as.numeric(SOBss$Shutoff.Block) -> SOBss$Shutoff.Block
      as.numeric(dateFilteredDF$Shutoff.Block) -> dateFilteredDF$Shutoff.Block


      # end
      SOBss[!is.na(SOBss$Shutoff.Block), ] -> SOBss
      dplyr::full_join(dateFilteredDF, SOBss, by = "Shutoff.Block") -> dateFilteredDF2
      coalesce2(dateFilteredDF2$Service.Location.Id.y, dateFilteredDF2$Service.Location.Id.x) -> dateFilteredDF2$Service.Location.Id
      dateFilteredDF2$Service.Location.Id.x <- NULL
      dateFilteredDF2$Service.Location.Id.y <- NULL

      dateFilteredDF2 %>%
        dplyr::arrange(Reported.Date) %>%
        dplyr::group_by(Work.Order.Number) %>%
        dplyr::select(Work.Order.Number, Service.Location.Id) %>%
        dplyr::mutate(NdistinctSLIDS = dplyr::n()) %>%
        as.data.frame() -> dateFilteredDF2

      # as.factor(dateFilteredDF2$Service.Location.Id)->dateFilteredDF2$Service.Location.Id
      # levels(dateFilteredDF2$Service.Location.Id)->allSLIDS

      N1 <- list()
      N2 <- list()
      N3 <- list()
      N4 <- list()
      N5 <- list()
      N6 <- list()
      N7 <- list()

      as.numeric(dateFilteredDF2$Work.Order.Number) -> dateFilteredDF2$Work.Order.Number
      dateFilteredDF2[!(is.na(dateFilteredDF2$Work.Order.Number)), ] -> dateFilteredDF2
      unique(dateFilteredDF2$Work.Order.Number) -> WONs

      # Get first record out of loop

      dateFilteredDF2 %>% dplyr::filter(Work.Order.Number %in% WONs[1]) -> InitWO #sff

      InitWO$Service.Location.Id -> listSLID


      for (i in 1:length(WONs)) {
        print(paste0("running", i, " of ", length(WONs), " in Year ", k))
        dateFilteredDF2 %>% dplyr::filter(Work.Order.Number %in% WONs[i]) -> InitWO
        # dateFilteredDF2 %>% filter(Work.Order.Number %in% WONs[1:(i-1)])->sfff

        unique(InitWO$Service.Location.Id) -> SLIDS
        list() -> X
        for (j in 1:length(SLIDS)) {
          if (is.null(sum(listSLID %in% SLIDS[[j]]))) {
            1 -> X[[j]]
          }
          else {
            1 + sum(listSLID %in% SLIDS[[j]]) -> X[[j]]
          }
        }

        as.data.frame(table(unlist(X))) -> SingleWOResult
        # colnames(tabDF)<-c("SLIDs", "Total")
        # tabDF %>% group_by (Total) %>% dplyr::summarize(dplyr::n()) %>% as.data.frame->hhh
        colnames(SingleWOResult) <- c("Total", "SLIDs")
        as.numeric(SingleWOResult[SingleWOResult$Total %in% 1, ][2]) -> N1[[i]]
        as.numeric(SingleWOResult[SingleWOResult$Total %in% 2, ][2]) -> N2[[i]]
        as.numeric(SingleWOResult[SingleWOResult$Total %in% 3, ][2]) -> N3[[i]]
        as.numeric(SingleWOResult[SingleWOResult$Total %in% 4, ][2]) -> N4[[i]]
        as.numeric(SingleWOResult[SingleWOResult$Total %in% 5, ][2]) -> N5[[i]]
        as.numeric(SingleWOResult[SingleWOResult$Total %in% 6, ][2]) -> N6[[i]]
        as.numeric(SingleWOResult[SingleWOResult$Total %in% 7, ][2]) -> N7[[i]]


        c(SLIDS, listSLID) -> listSLID
      } # end i

      do.call(rbind, N1) -> temp1
      do.call(rbind, N2) -> temp2
      do.call(rbind, N3) -> temp3
      do.call(rbind, N4) -> temp4
      do.call(rbind, N5) -> temp5
      do.call(rbind, N6) -> temp6
      do.call(rbind, N7) -> temp7

      cbind(temp1, temp2, temp3, temp4, temp5, temp6, temp7) -> temp
      temp[is.na(temp)] <- 0

      cbind(as.matrix(WONs), temp) -> year_repeats[[k]]


      gc()
      rm(temp)
      rm(SingleWOResult)
    } # end k

    do.call(rbind, year_repeats)->Nrepeats

  as.data.frame(Nrepeats) -> Nrepeats

  c("NSLIDS_1", "NSLIDS_2", "NSLIDS_3", "NSLIDS_4", "NSLIDS_5", "NSLIDS_6", "NSLIDS_7")->newNames
  colnames(Nrepeats) <- c("WONs", newNames)

  # as.numeric(as.character(combined.dftemp$Work.Order.Number))->combined.dftemp$Work.Order.Number

  Nrepeats[!duplicated(Nrepeats[, 1]), ] -> Nrepeats

  merge(as.data.frame(DFtemp), Nrepeats, by.x = "Work.Order.Number", by.y = "WONs", all.x = TRUE) -> DF

  DF[,newNames][is.na(DF[,newNames])] <- 0

  # DF$NSLIDS_1[is.na(DF$NSLIDS_1)] <- 0
  # DF$NSLIDS_2[is.na(DF$NSLIDS_2)] <- 0
  # DF$NSLIDS_3[is.na(DF$NSLIDS_3)] <- 0
  # DF$NSLIDS_4[is.na(DF$NSLIDS_4)] <- 0
  # DF$NSLIDS_5[is.na(DF$NSLIDS_5)] <- 0
  # DF$NSLIDS_6[is.na(DF$NSLIDS_6)] <- 0
  # DF$NSLIDS_7[is.na(DF$NSLIDS_7)] <- 0

  # Add Number of slids from SLID SOB lookup to combined.df

  make.names(colnames(DF)) -> colnames(DF)
  DF %>%
    dplyr::select(Shutoff.Block) %>%
    as.data.frame() -> S

  dplyr::left_join(S, SOBlist) -> temp
  temp[, 2] -> Z
  DF %>% dplyr::mutate(N.slids.affected = Z) -> DF

  return(DF)
}


#' Title
#'
#' @param updateRepeat
#' @param data.f
#'
#' @return
#' @export
#'
#' @examples
CalcNSLIDS <- function(imported_data, df) {
  list(
    c("1996/04/01", "1997/03/31"), c("1997/04/01", "1998/03/31"), c("1998/04/01", "1999/03/31"), c("1999/04/01", "2000/03/31"),
    c("2000/04/01", "2001/03/31"), c("2001/04/01", "2002/03/31"), c("2002/04/01", "2003/03/31"), c("2003/04/01", "2004/03/31"),
    c("2004/04/01", "2005/03/31"), c("2005/04/01", "2006/03/31"), c("2006/04/01", "2007/03/31"), c("2007/04/01", "2008/03/31"),
    c("2008/04/01", "2009/03/31"), c("2009/04/01", "2010/03/31"), c("2010/04/01", "2011/03/31"), c("2011/04/01", "2012/03/31"),
    c("2012/04/01", "2013/03/31"), c("2013/04/01", "2014/03/31"), c("2014/04/01", "2015/03/31"), c("2015/04/01", "2016/03/31"),
    c("2016/04/01", "2017/03/31"), c("2017/04/01", "2018/03/31"), c("2018/04/01", "2019/03/31"), c("2019/04/01", "2020/03/31")
  ) -> listofYears

  #Current (relatively) Asset and SOB lookup - not historic I don't believe..
  imported_data[[10]] -> SOBAsset

  data.table::setDF(SOBAsset) -> SOBAsset

  as.numeric(SOBAsset$`Asset Number`) -> SOBAsset$`Asset Number`

  colnames(SOBAsset)[c(1,3,10)] <-c("Service.Location.Id", "Shutoff.Block", "Asset.Number")
  SOBAsset %>% dplyr::select(Asset.Number, Shutoff.Block, Service.Location.Id) ->SOBAsset

    calcrepeat(
      DF = df,
      listofYears = listofYears,
      SOB = SOBAsset
    ) -> DF_NSlids  #Note that this whole function is based on outages and G2.

    # merging analysis of workorders
    setdiff(colnames(DF_NSlids), colnames(df)) -> newcols # new columns created by NSLID calc
    DF_NSlids %>% dplyr::select(Work.Order.Number, newcols) %>% as.data.frame() -> temp2 # merge old and new DF, new DF should have fewer rows as it only includes outages.

    dplyr::left_join(df, temp2, by = "Work.Order.Number") %>% as.data.frame() -> merged_DF_NSlids

    lapply(merged_DF_NSlids[,newcols], function(x) as.numeric(as.character(x))) ->merged_DF_NSlids[,newcols]
    merged_DF_NSlids[,newcols][is.na(merged_DF_NSlids[,newcols])] <- 0

    # time between failure in SOBs
    merged_DF_NSlids %>%
      dplyr::arrange(
        Shutoff.Block, Reported.Date, dplyr::desc(NSLIDS_1), dplyr::desc(NSLIDS_2), dplyr::desc(NSLIDS_3), dplyr::desc(NSLIDS_4),
        dplyr::desc(NSLIDS_5), dplyr::desc(NSLIDS_6), dplyr::desc(NSLIDS_7)
      ) %>%
      dplyr::group_by(Shutoff.Block) %>%
      dplyr::mutate(time_since_SOB_failure = as.numeric(difftime(Reported.Date, dplyr::lag(Reported.Date), units = "days"))) -> merged_DF_NSlids

    as.factor(merged_DF_NSlids$Class.Structure.1) -> merged_DF_NSlids$Class.Structure.1
    merged_DF_NSlids %>% dplyr::mutate(time_since_SOB_failure = replace(time_since_SOB_failure, time_since_SOB_failure < 0, NA)) -> merged_DF_NSlids

    # time between outage in SOBs
    merged_DF_NSlids %>%
      dplyr::filter(Number.of.Work.Order.Water.Outages>0) %>%
      dplyr::arrange(Shutoff.Block, Reported.Date, dplyr::desc(NSLIDS_1), dplyr::desc(NSLIDS_2), dplyr::desc(NSLIDS_3), dplyr::desc(NSLIDS_4), dplyr::desc(NSLIDS_5), dplyr::desc(NSLIDS_6), dplyr::desc(NSLIDS_7)) %>%
      dplyr::group_by(Shutoff.Block) %>%
      dplyr::mutate(time_since_SOB_outage = as.numeric(difftime(Reported.Date, dplyr::lag(Reported.Date), units = "days"))) -> merged_DF_NSlids_outage
    merged_DF_NSlids_outage %>% dplyr::mutate(time_since_SOB_outage = replace(time_since_SOB_outage, time_since_SOB_outage < 0, NA)) %>%
      dplyr::select(Work.Order.Number, time_since_SOB_outage) -> merged_DF_NSlids_outage

    merged_DF_NSlids_outage %>% dplyr::select(Work.Order.Number, time_since_SOB_outage) -> tmp

    dplyr::left_join(merged_DF_NSlids, tmp, by="Work.Order.Number") -> merged_DF_NSlids

    paste0(merged_DF_NSlids$Pipe.Material, ", ", lubridate::year(lubridate::ymd(merged_DF_NSlids$Install.Date))) -> merged_DF_NSlids$I.D
    as.factor(merged_DF_NSlids$I.D) -> merged_DF_NSlids$I.D

    return(merged_DF_NSlids)

}

#' Title
#'
#' @param N
#' @param spatYear
#' @param df
#'
#' @return
#' @export
#'
#' @examples
G2analysis <- function(N, spatYear, df) {

  ###
  combined.df[combined.df$Class.Structure %in% "Repair Burst Water Main", ] -> examin
  head(as.data.frame(examin))

  table(examin$NSLIDS_1)

  as.data.frame(table(combined.df$Class.Structure)) -> foo # use this to divide interuptions percent resulting in interuption
  colnames(foo) <- c("Class.Structure", "Totalperclass")

  # Repeat interuption plots

  grep("NSLIDS", colnames(combined.df)) -> P
  colnames(combined.df)[P] -> B
  colnames(combined.df)[P[N]] -> A
  combined.df %>% dplyr::filter(get(A) > 0) -> int

  int %>% dplyr::filter(Class.Structure == "Water Hydrant Repair Replace")
  combined.df %>% dplyr::select(c(B, Install.Date)) -> e

  names(which(table(int$Class.Structure) > 200 / (N^2))) -> freqClass
  names(which(table(int$Pipe.Material) > 200 / (N^2))) -> freqPipeMat
  freqPipeMat <- c(freqPipeMat, NA)
  int %>% dplyr::filter(Class.Structure %in% freqClass) -> int
  nrow(int)
  int %>% dplyr::filter(Pipe.Material %in% freqPipeMat) -> int
  nrow(int)

  # Year on Year repeat plots
  colnames(combined.df)
  combined.df %>%
    dplyr::mutate(Year = lubridate::year(as.Date(Reported.Date))) %>%
    dplyr::group_by(Year) %>%
    dplyr::select(c(B)) %>%
    summarize_all(sum, na.rm = TRUE) %>%
    as.data.frame() ->> yearRepeats

  tidyr::gather(yearRepeats, SLIDS, value, -Year) -> long_tab

  p <- ggplot(data = long_tab[long_tab$Year >= 2000, ], aes(x = Year, y = log10(value), color = SLIDS)) +
    geom_line() +
    theme(axis.text.x = element_text(angle = 0, hjust = 1))
  p

  # Fraction of total outages resulting in N interuptions, modify to include all work orders,
  int %>%
    dplyr::group_by(Class.Structure) %>%
    dplyr::summarize(total = dplyr::n()) %>%
    as.data.frame() -> foo2
  merge(foo2, foo, all.x = TRUE) -> foo3
  foo3 %>% dplyr::mutate(propClass = total / Totalperclass) -> foo4

  # Histogram PLots
  hp1 <- ggplot2::ggplot(data = int, aes(int$Class.Structure)) +
    geom_histogram(stat = "count") +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    ggtitle(paste0("Number of Interuptions greater than ", N)) +
    labs(y = "", x = "Work Order Type") +
    scale_x_discrete(drop = TRUE)
  hp2 <- ggplot2::ggplot(data = int, aes(int$Pipe.Material)) +
    geom_histogram(stat = "count") +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    ggtitle(paste0("Number of Interuptions greater than ", N)) +
    labs(y = "", x = "Pipe Material")
  # ggplot(data= int, aes(int$I.D)) + geom_histogram(stat="count")+ theme(axis.text.x = element_text(angle = 90, hjust =1)) + ggtitle(paste0("Number of Interuptions greater than ",N))
  hp3 <- ggplot2::ggplot(data = int, aes(lubridate::year(as.Date(int$Install.Date)))) +
    geom_histogram(stat = "count") +
    theme(axis.text.x = element_text(angle = 0, hjust = 1)) +
    ggtitle(paste0("Number of Interuptions greater than ", N)) +
    labs(y = "", x = "Year")
  hp4 <- ggplot2::ggplot(data = int, aes(int$Class.Structure.1)) +
    geom_histogram(stat = "count") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    ggtitle(paste0("Number of Interuptions greater than ", N)) +
    labs(y = "", x = "Year")

  figure <- ggarrange(hp1, hp2, hp3, hp4,
    labels = c("A", "B", "C", "D"),
    ncol = 2, nrow = 2
  )

  # Bar Plots, Nslids affected
  p <- ggplot(data = int, aes(x = Class.Structure, y = get(A))) +
    geom_bar(stat = "identity") +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    labs(y = "Total N SLIDS", x = "") +
    ggtitle(paste0("Total N SLIDS interuption greater than ", N))
  p

  p <- ggplot(data = int, aes(x = Pipe.Material, y = get(A))) +
    geom_bar(stat = "identity") +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    labs(y = "Total N SLIDS", x = "") +
    ggtitle(paste0("Total N SLIDS interuption greater than ", N))
  p

  p <- ggplot(data = int, aes(x = lubridate::year(as.Date(int$Install.Date)), y = get(A))) +
    geom_bar(stat = "identity") +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    labs(y = "Total N SLIDS", x = "") +
    ggtitle(paste0("Total N SLIDS interuption greater than ", N))
  p

  # New table for Luke  Just Main bursts and main leaks,
  readRDS("Outputs/AssetTableClusterID.RDS") -> table.result2

  combined.df %>%
    dplyr:::select(c(P, I.D)) %>%
    dplyr::group_by(I.D) %>%
    dplyr::summarize_at(vars(NSLIDS_1:NSLIDS_7), sum) %>%
    as.data.frame() %>%
    dplyr::arrange(dplyr::desc(NSLIDS_1)) -> h
  h # add this to clustered table

  colnames(table.result2)
  nrow(table.result2)
  levels(table.result2$Class.Structure)

  summary(table.result2$Class.Structure)
  table.result2[!is.na(table.result2$NFails), ] -> table.result2
  table.result2 %>%
    dplyr::select(Asset.Number, Reported.Date, Class.Structure) %>%
    dplyr::filter(Reported.Date > "2017/08/01") %>%
    dplyr::group_by(Asset.Number, Class.Structure) %>%
    dplyr::summarise(Nfails12month = dplyr::n()) %>%
    dplyr::select(Asset.Number, Nfails12month, Class.Structure) -> L
  sum(L$Nfails12month)

  colnames(table.result2)
  table.result2 %>% dplyr::select(Asset.Number, Pipe.Material, Install.Date, Nfailspertotallength, totalNpershutoffblock, Noutagesperasset, Class.Structure) -> M
  nrow(M) / nrow(L)

  summary(L$Class.Structure)

  dplyr::left_join(M, L, by = "Asset.Number") -> summary.table
  as.data.frame(summary.table) -> summary.table

  summary.table[!duplicated(summary.table$Asset.Number), ] -> summary.table
  summary(table.result2$Class.Structure)
  paste0(summary.table$Pipe.Material, ", ", summary.table$Install.Date) -> summary.table$I.D

  merge(summary.table, h, by = "I.D", all.x = TRUE) -> summary.table
  summary.table %>% dplyr::arrange(dplyr::desc(Nfailspertotallength)) -> summary.table

  summary.table %>%
    dplyr::group_by(I.D) %>%
    dplyr::summarise(MeanNFailsperL = mean(Nfailspertotallength, na.rm = TRUE)) %>%
    dplyr::arrange(dplyr::desc(MeanNFailsperL)) %>%
    as.data.frame() -> tab5

  summary.table %>% dplyr::left_join(Total = sum(Noutagesperasset, na.rm = TRUE)) -> DWPs
  summary.table %>% dplyr::left_join(Total = sum(Nfails12month, na.rm = TRUE)) -> DWPs12

  # Bar Plots, Nslids affected
  p5 <- ggplot(data = tab5[1:20, ], aes(x = I.D, y = MeanNFailsperL)) +
    geom_bar(stat = "identity") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    labs(x = "")

  write.csv(summary.table, "Outputs/pipe_table.csv")

  # N<-3
  grep("NSLIDS", colnames(combined.df)) -> P
  colnames(combined.df)[P] -> B
  colnames(combined.df)[P[N]] -> A

  # Start by summing NSLIDS 1 and 2 and summing 3,4,5,6
  combined.df$NSLIDS_1 + combined.df$NSLIDS_2 -> noG22
  combined.df %>%
    ungroup() %>%
    dplyr::select(B) %>%
    rowSums() -> G222
  as.vector(G222) -> G222
  G222 - noG22 -> G222

  combined.df$NumG2SLIDs <- NULL
  as.data.frame(combined.df) -> combined.df
  combined.df %>% dplyr::mutate(NumberG2SLIDs = G222) -> newcombined.df
  newcombined.df %>% dplyr::mutate(Number_almostG2SLIDs = noG22) -> newcombined.df

  head(newcombined.df)

  int <- list()
  NdistinctSOBs <- list()
  NdistinctSOBs_Nearmiss <- list()

  for (i in 1:length(listofYears)) {
    int[[i]] <- lubridate::interval(listofYears[[i]][1], listofYears[[i]][2])
    newcombined.df %>%
      dplyr::filter(Reported.Date %within% int[[i]]) %>%
      dplyr::filter(Number_almostG2SLIDs > 0) %>%
      dplyr::distinct(Shutoff.Block) %>%
      dplyr::tally() -> NdistinctSOBs_Nearmiss[[i]]
    newcombined.df %>%
      dplyr::filter(Reported.Date %within% int[[i]]) %>%
      dplyr::filter(NumberG2SLIDs > 0) %>%
      dplyr::distinct(Shutoff.Block) %>%
      dplyr::tally() -> NdistinctSOBs[[i]]
  }

  do.call(rbind, NdistinctSOBs) -> NdistinctSOBs
  do.call(rbind, NdistinctSOBs_Nearmiss) -> NdistinctSOBs_Nearmiss

  do.call(rbind, listofYears) -> tempp
  cbind(tempp[, 1], NdistinctSOBs) -> Ndistinct
  cbind(Ndistinct, NdistinctSOBs_Nearmiss) -> Ndistinct

  as.data.frame(Ndistinct)
  colnames(Ndistinct) <- c("Date", "NumberSOBs", "NumberSOBnearmiss")
  as.Date(Ndistinct$Date) -> Ndistinct$Date

  Ndistinct %>% dplyr::mutate(ratio = NumberSOBnearmiss / NumberSOBs) -> Ndistinct

  p <- ggplot(data = Ndistinct, aes(x = Date, y = NumberSOBs)) +
    geom_line() +
    geom_point() +
    theme(axis.text.x = element_text(angle = 0, hjust = 1))
  p

  p <- ggplot(data = Ndistinct, aes(x = Date, y = NumberSOBnearmiss)) +
    geom_line() +
    geom_point() +
    theme(axis.text.x = element_text(angle = 0, hjust = 1))
  p

  p <- ggplot(data = Ndistinct, aes(x = Date, y = ratio)) +
    geom_line() +
    geom_point() +
    theme(axis.text.x = element_text(angle = 0, hjust = 1))
  p

  #
  int <- list()
  totalNdistinctSOBs <- list()
  NdistinctSOBs_Nearmiss_pipe <- list()
  NdistinctSOBs_pipe <- list()
  totalNdistinctSOBs_Nearmiss <- list()

  table(newcombined.df$Class.Structure)
  newcombined.df %>% dplyr::filter(NumberG2SLIDs > 0) -> SS
  table(SS$Class.Structure)

  for (i in 1:length(listofYears)) {
    int[[i]] <- lubridate::interval(listofYears[[i]][1], listofYears[[i]][2])
    newcombined.df %>%
      dplyr::filter(Reported.Date %within% int[[i]]) %>%
      dplyr::filter(Number_almostG2SLIDs > 0) %>%
      dplyr::filter(Class.Structure %in% "Repair Burst Water Main") %>%
      dplyr::left_join(sum(Number_almostG2SLIDs)) -> NdistinctSOBs_Nearmiss_pipe[[i]]
    newcombined.df %>%
      dplyr::filter(Reported.Date %within% int[[i]]) %>%
      dplyr::filter(NumberG2SLIDs > 0) %>%
      dplyr::left_join(sum(NumberG2SLIDs)) -> totalNdistinctSOBs[[i]]
    newcombined.df %>%
      dplyr::filter(Reported.Date %within% int[[i]]) %>%
      dplyr::filter(NumberG2SLIDs > 0) %>%
      dplyr::filter(Class.Structure %in% "Repair Burst Water Main") %>%
      dplyr::left_join(sum(NumberG2SLIDs)) -> NdistinctSOBs_pipe[[i]]
    newcombined.df %>%
      dplyr::filter(Reported.Date %within% int[[i]]) %>%
      dplyr::filter(Number_almostG2SLIDs > 0) %>%
      dplyr::left_join(sum(Number_almostG2SLIDs)) -> totalNdistinctSOBs_Nearmiss[[i]]
  }

  do.call(rbind, NdistinctSOBs_Nearmiss_pipe) -> NdistinctSOBs_Nearmiss_pipe
  do.call(rbind, totalNdistinctSOBs) -> totalNdistinctSOBs
  do.call(rbind, NdistinctSOBs_pipe) -> NdistinctSOBs_pipe
  do.call(rbind, totalNdistinctSOBs_Nearmiss) -> totalNdistinctSOBs_Nearmiss

  do.call(rbind, listofYears) -> tempp
  cbind(tempp[, 1], NdistinctSOBs_Nearmiss_pipe) -> Ndistinct
  cbind(Ndistinct, totalNdistinctSOBs) -> Ndistinct
  cbind(Ndistinct, NdistinctSOBs_pipe) -> Ndistinct
  cbind(Ndistinct, totalNdistinctSOBs_Nearmiss) -> Ndistinct

  as.data.frame(Ndistinct)
  colnames(Ndistinct) <- c("Date", "NumberSOBs_Pipe_1sand2s", "NumberSOBs_Total", "NumberSOBs_Pipe", "NumberSOBsTotal_1sand2s")
  as.Date(Ndistinct$Date) -> Ndistinct$Date

  Ndistinct %>% dplyr::mutate(BurstPiperatio1and2 = NumberSOBs_Pipe_1sand2s / NumberSOBsTotal_1sand2s) -> Ndistinct
  Ndistinct %>% dplyr::mutate(BurstPiperatioG2 = NumberSOBs_Pipe / NumberSOBs_Total) -> Ndistinct

  p <- ggplot(data = Ndistinct, aes(x = Date, y = BurstPiperatioG2)) +
    geom_line() +
    theme(axis.text.x = element_text(angle = 0, hjust = 1))
  p

  p <- ggplot(data = Ndistinct, aes(x = Date, y = BurstPiperatio1and2)) +
    geom_line() +
    theme(axis.text.x = element_text(angle = 0, hjust = 1))
  p

  # newcombined.df %>% dplyr::filter(!is.na(time_since_asset_failure)) %>% dplyr::filter(NumberG2SLIDs>0)->new.combined.df2
  # newcombined.df %>% dplyr::filter(NumberG2SLIDs>0)->new.combined.df2a

  newcombined.df %>%
    dplyr::filter(!is.na(time_since_asset_failure)) %>%
    dplyr::filter(NumberG2SLIDs > 2) -> new.combined.df2
  newcombined.df %>% dplyr::filter(NumberG2SLIDs > 2) -> new.combined.df2a
  nrow(new.combined.df2) / nrow(new.combined.df2a) -> fractionofG2repeatingassets

  p <- ggplot(data = new.combined.df2, aes(x = time_since_asset_failure)) +
    stat_ecdf(geom = "step")
  p

  # same as above but for pipe bursts

  newcombined.df %>%
    dplyr::filter(!is.na(time_since_asset_failure)) %>%
    dplyr::filter(Class.Structure %in% "Repair Burst Water Main") %>%
    dplyr::filter(NumberG2SLIDs > 3) -> new.combined.df2
  newcombined.df %>%
    dplyr::filter(Class.Structure %in% "Repair Burst Water Main") %>%
    dplyr::filter(NumberG2SLIDs > 3) -> new.combined.df2a
  nrow(new.combined.df2) / nrow(new.combined.df2a) -> fractionofG2repeatingassets # higher for pipe bursts than for other failure

  p <- ggplot(data = new.combined.df2, aes(x = time_since_asset_failure)) +
    stat_ecdf(geom = "step")
  # p + scale_x_continuous(limits = c(0, 365))->p
  p

  # Try to understand why with G2 Cases there are time since SOBs > 365
  # SOB WOs out of order.  Have added arange by Reported.Date prior to Calculating N repeats.  Not sure how or why gets out of order.  Group by in calcRepeats??
  newcombined.df %>% dplyr::filter(NumberG2SLIDs > 3) -> blag
  blag$time_since_SOB_outage
  blag[724, ]
  combined.df %>% dplyr::filter(Service.Location.Id == 1222937)
  combined.df %>% dplyr::filter(Shutoff.Block == 154378)


  p <- ggplot(data = new.combined.df2, aes(x = time_since_SOB_outage)) +
    stat_ecdf(geom = "step")
  p + scale_x_continuous(limits = c(0, 365)) -> p7
  p7

  newcombined.df %>%
    dplyr::filter(Shutoff.Block == 218857) %>%
    dplyr::filter(NumberG2SLIDs > 3) %>%
    dplyr::filter(time_since_SOB_outage > 365)

  p <- ggplot(data = new.combined.df2, aes(x = totalNpershutoffblock)) +
    stat_ecdf(geom = "step")
  p + scale_x_continuous(limits = c(0, 100)) -> p
  p

  # Combined plot sum all NSLIDS 3 and more for G2
  yearRepeats$NSLIDS_1 + yearRepeats$NSLIDS_2 -> NoG2
  rowSums(yearRepeats[, -1]) - NoG2 -> G2

  data.frame(Year = yearRepeats[, 1], G2cases = G2, NearMisses = NoG2) -> G2Table

  p <- ggplot(data = G2Table[G2Table$Year >= 2000, ], aes(x = Year, y = G2cases)) +
    geom_point() +
    theme(axis.text.x = element_text(angle = 0, hjust = 1))
  p

  gather(G2Table, key, result, -Year) -> G2TableL

  p4 <- ggplot(data = G2TableL[G2TableL$Year >= 2000, ], aes(x = Year, y = result, color = key)) +
    geom_line() +
    theme(axis.text.x = element_text(angle = 0, hjust = 1))
  p4

  # Plots the number of SLIDS
  # input$year-> X1
  "2016/2/1" -> X1
  as.Date(X1) -> X1

  int2 <- list()
  for (i in 1:length(listofYears)) {
    int2[[i]] <- lubridate::interval(listofYears[[i]][1], listofYears[[i]][2])
  }

  unlist(lapply(int2, function(x) X1 %within% x)) -> tm
  which(tm) -> tm2

  newcombined.df %>%
    dplyr::filter(NumberG2SLIDs > 2) %>%
    dplyr::filter(Reported.Date %within% int[[tm2]]) %>%
    dplyr::arrange(Shutoff.Block, Reported.Date) -> SSS
  SSS %>% dplyr::mutate(prop = (Number.of.SLIDs.Affected - N.slids.affected) / N.slids.affected) -> SSS
  SSS %>% dplyr::mutate(diff = (Number.of.SLIDs.Affected - N.slids.affected)) -> SSS

  # Track Raw customer impacted.  Convert SOB to SLIDS,  Bucket into sum of months.

  # MAke any negative difference zero
  SSS[!is.na(SSS$diff) & SSS$diff < 0, ]$diff <- 0
  SSS %>%
    dplyr::mutate(yearmonth = as.Date(as.yearmon(Reported.Date))) %>%
    dplyr::filter(NumberG2SLIDs > 3) %>%
    dplyr::group_by(yearmonth) %>%
    dplyr::summarize(sum = sum(diff)) -> SSSS

  p <- ggplot() +
    geom_point(data = SSSS, aes(x = yearmonth, y = sum)) +
    #  geom_point(data=SSS, aes(x=Reported.Date, y=N.slids.affected), colour="red") +
    theme(axis.text.x = element_text(angle = 0, hjust = 1))
  p + scale_y_continuous(limits = c(0, 300)) -> p

  p + labs(
    title = "Number of SLIDS impacted by Shut off Block Extensions", subtitle = waiver(), caption = waiver(),
    tag = waiver()
  ) -> p

  p + ylab("Number of additional SLIDS") -> p
  p + xlab("2017") -> padd


  SSS %>%
    dplyr::mutate(yearmonth = as.Date(as.yearmon(Reported.Date))) %>%
    dplyr::filter(NumberG2SLIDs > 0) %>%
    dplyr::group_by(yearmonth) %>%
    dplyr::summarize(sum = sum(N.slids.affected)) -> SSSSS
  SSS %>%
    dplyr::mutate(yearmonth = as.Date(as.yearmon(Reported.Date))) %>%
    dplyr::filter(NumberG2SLIDs > 0) %>%
    dplyr::group_by(yearmonth) %>%
    dplyr::summarize(sum = sum(Number.of.SLIDs.Affected)) -> SSSSSS

  cbind(SSSSS, SSSSSS[, 2]) -> SSSSSS

  write.csv(SSSSSS, paste0("Outputs/extension_year", 23, ".csv"))

  SSS %>%
    dplyr::filter(diff > 100) %>%
    dplyr::select(Shutoff.Block) -> SOBswithExtension_2017

  sort(table(SOBswithExtension_2017))

  p <- ggplot(data = SSS, aes(x = diff)) +
    stat_ecdf(geom = "step")
  p + scale_x_continuous(limits = c(-100, 100)) -> p
  p

  # check Number of G2 work orders as percent of totla
  newcombined.df %>%
    dplyr::filter(NumberG2SLIDs > 0) %>%
    dplyr::tally() -> nG2
  newcombined.df %>%
    dplyr::filter(NumberG2SLIDs > 0) %>%
    dplyr::filter(time_since_asset_failure > 365 | is.na(time_since_asset_failure)) %>%
    dplyr::tally() -> NnonrepeatAssets
  (nG2[, ] - NnonrepeatAssets[, ]) / nG2[, ] -> repeatassetG2

  # Pie chart
  newcombined.df %>%
    dplyr::filter(NumberG2SLIDs > 0) %>%
    dplyr::select(Shutoff.Block) -> TEMPW
  newcombined.df %>%
    dplyr::filter(Shutoff.Block %in% TEMPW[, ]) %>%
    dplyr::select(N.slids.affected, Class.Structure) -> YOYOMA

  YOYOMA %>%
    dplyr::group_by(Class.Structure) %>%
    dplyr::summarize(n = sum(N.slids.affected, na.rm = TRUE)) %>%
    ungroup() %>%
    dplyr::mutate(per = `n` / sum(`n`)) %>%
    dplyr::arrange(dplyr::desc(Class.Structure)) -> YOYOMA
  YOYOMA$label <- scales::percent(YOYOMA$per)

  png("WOs.png", width = 1200, height = 900)
  ggplot(data = YOYOMA) +
    geom_bar(aes(x = "", y = per, fill = Class.Structure), stat = "identity", width = 1) +
    coord_polar("y", start = 0) +
    theme_void() +
    theme(text = element_text(size = 20)) +
    geom_text(aes(x = 1, y = cumsum(per) - per / 2, label = label), size = 7) -> P
  print(P)
  dev.off()

  # Creatw a basic bar
  pie <- ggplot(YOYOMA, aes(x = "", y = n, fill = Class.Structure)) +
    geom_bar(stat = "identity", width = 1)
  # Convert to pie (polar coordinates) and add labels
  pie <- pie + coord_polar("y", start = 0) + geom_text(aes(label = n), position = position_stack(vjust = 0.5))

  # Add color scale (hex colors)
  # pie = pie + scale_fill_manual(values=c("#55DSW0", "#33418A","#55DDE0", "#33658A", "#2F4858", "#F6AE2D", "#F26419", "#999999"))

  # Remove labels and add title
  pie <- pie + labs(x = NULL, y = NULL, fill = NULL, title = "All G2 cases - Work Order Breakdown of SLIDS")

  # Tidy up the theme
  pie <- pie + theme_classic() + theme(
    axis.line = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    plot.title = element_text(hjust = 0.5, color = "#666666")
  )

  newcombined.df %>%
    dplyr::group_by(Class.Structure) %>%
    tally() -> YOYOMA

  YOYOMA %>%
    dplyr::group_by(Class.Structure) %>%
    ungroup() %>%
    dplyr::mutate(per = `n` / sum(`n`)) %>%
    dplyr::arrange(dplyr::desc(Class.Structure)) -> YOYOMA
  YOYOMA$label <- scales::percent(YOYOMA$per)

  png("SLIDS.png", width = 1200, height = 900)
  ggplot(data = YOYOMA) +
    geom_bar(aes(x = "", y = per, fill = Class.Structure), stat = "identity", width = 1) +
    coord_polar("y", start = 0) +
    theme_void() +
    theme(text = element_text(size = 20)) +
    geom_text(aes(x = 1, y = cumsum(per) - per / 2, label = label), size = 7) -> P2
  print(P2)
  dev.off()

  saveRDS(newcombined.df, "Outputs/G2combined.df.RDS")

  newcombined.df %>% dplyr::filter(NumberG2SLIDs > 2) -> newcombined.df2

  G2spatfun(asset_data = newcombined.df2, Year = spatYear) -> spatPlot
  spatPlot[[1]] -> raster_df
  spatPlot[[2]] -> maplat
  spatPlot[[3]] -> maplon

  get_map(location = c(maplon, maplat), maptype = "toner-lite", zoom = 11, scale = 2) -> Melb
  ggmap(Melb, extent = "device", ylab = "Latitude", xlab = "Longitude") +
    geom_point(aes(y = raster_df$x, x = raster_df$y, colour = "red"), data = raster_df)

  # G2visualisation

  # SOB table,  Year, ID, lat, lon, Noutages, Ndistinct assests
  # Heterogeneity<-Noutages^2/Ndistinct_assets^2
  # get SOB ID, Number of outages, averagelat lon, Number of Distinct Assets

  readRDS("Outputs/G2combined.df.RDS") -> newcombined.df

  int <- list()
  NdistinctSOBs <- list()

  for (i in 1:length(listofYears)) {
    int[[i]] <- lubridate::interval(listofYears[[i]][1], listofYears[[i]][2])
    newcombined.df %>%
      dplyr::filter(Reported.Date %within% int[[i]]) %>%
      dplyr::filter(NumberG2SLIDs > 0) %>%
      dplyr::group_by(Shutoff.Block) %>%
      summarise_at(c("Latitude", "Longitude"), mean, na.rm = TRUE) -> temp0

    newcombined.df %>%
      dplyr::filter(Reported.Date %within% int[[i]]) %>%
      dplyr::filter(NumberG2SLIDs > 0) %>%
      dplyr::group_by(Shutoff.Block) %>%
      summarise_at(c("NSLIDS_3", "NSLIDS_4", "NSLIDS_5", "NSLIDS_6", "NSLIDS_7"), sum, na.rm = TRUE) -> temp

    apply(temp[, -1], 1, which.max) + 2 -> Noutages
    cbind(temp[, 1], Noutages) -> temp2

    cbind(temp0, temp2) -> NdistinctSOBs[[i]]
    NdistinctSOBs[[i]] -> df
    df[complete.cases(df), ] -> df
    coordinates(df) <- c("Latitude", "Longitude") # promote to SpatialPointsDataFrame

    png(filename = paste0("Outputs/bubbleplot_", i, ".png"))
    bubble(df, "Noutages",
      maxsize = 1.3, main = "N outages",
      key.entries = c(3, 4, 5, 6, 7)
    ) -> f
    print(f)
    dev.off()
  }

  do.call(rbind, NdistinctSOBs) ->> NdistinctSOBs

  result <- list(pie, P2, padd, p4, figure, p5, spatPlot)
  return(result)
}
