#' Modified Coalesce
#' Coalesce two dataframes
#' @param ...
#'
#' @return dataframe
#' @export
#'
#' @examples \dontrun {coalesce2(x, y)}
coalesce2 <- function(...) {
  Reduce(
    function(x, y) {
      levels(y) -> levy
      levels(x) <- c(levels(x), levy)
      i <- which(is.na(x))
      x[i] <- y[i]
      x
    },
    list(...)
  )
}


#' Data Wrangle
#' This is a custom function which creates two cleaned data frames
#' 1) asset data and 2) a combined work order and asset data
#' The cleaning steps ensures there are no duplicates and that the data is consistent
#'
#' @param imported_data
#'
#' @return list of cleaned data frames;  asset data and work order data
#' @export
#'
#' @examples  \dontrun {data_wrangle(imported_data)}
data_wrangle <- function(imported_data) {
  imported_data[[1]] -> work_orders_pre2015
  imported_data[[2]] -> work_orders
  imported_data[[3]] -> asset_data

  # clean and process asset data
  coalesce2(asset_data$Pipe.Material, asset_data$Pipe.Material.1) ->
    asset_data$Pipe.Material
  coalesce2(asset_data$Length.1, asset_data$Length) -> asset_data$Length
  coalesce2(asset_data$Shutoff.Block, asset_data$Shutoff.Block.1) ->
    asset_data$Shutoff.Block
  as.numeric(as.character(asset_data$Nominal.Pipe.Size..mm.)) ->
    asset_data$Nominal.Pipe.Size..mm
  as.Date(asset_data$Install.Date) -> asset_data$Install.Date
  lubridate::year(asset_data$Install.Date) -> asset_data$Install.Date

  asset_data$Pipe.Material.1 <- NULL
  asset_data$Length.1 <- NULL
  asset_data$Shutoff.Block.1 <- NULL

  colnames(asset_data)[1] <- "Asset.Number"

  # clean and process work order data
  # remove columns where all entries are NA
  work_orders <- work_orders[, colSums(is.na(work_orders)) < nrow(work_orders)]
  work_orders_pre2015 <- work_orders_pre2015[, colSums(is.na(work_orders_pre2015))
                                             < nrow(work_orders_pre2015)]

  # Work order data wrangling
  colnames(work_orders)[[1]] <- "Work.Order.Number"
  colnames(work_orders_pre2015)[[2]] <- "Reported.Date"


  # Remove Work Orders without a Date
  work_orders %>% dplyr::filter(!is.na(Reported.Date)) -> work_orders

  # Maximo pre 2014 is not accurate for water outages, replace with NA and use HAnsen
  work_orders$Number.of.Work.Order.Water.Outages[work_orders$Reported.Date
                                                 < as.Date("2014-07-01")] <- NA

  # Not used but could be useful to calculate duration of Work order for maintenance
  drops <- c("Actual.Off.Date", "Actual.On.Date")
  work_orders[, !(names(work_orders) %in% drops)] -> work_orders

  nrow(work_orders)
  summary(work_orders$Internal.Diameter..mm.)
  summary(work_orders$Nominal.Pipe.Size..mm.)

  # Combine PipeSize and internal Diameter, this gives nominal pipe size,
  # only ID if nominal is missing and ID present. Labelled ID, should have been Nominal
  coalesce2(
    work_orders$Nominal.Pipe.Size..mm.,
    work_orders$Internal.Diameter..mm.
  ) -> work_orders$Internal.Diameter..mm.
  drops <- c("Nominal.Pipe.Size..mm.")
  work_orders[, !(names(work_orders) %in% drops)] -> work_orders

  # Combine X,Y  separate X,Y coordinates for mains and service lines
  coalesce2(work_orders$X.Coordinates, work_orders$X.Coordinates.2) ->
    work_orders$X.Coordinates
  coalesce2(work_orders$Y.Coordinates, work_orders$Y.Coordinates.2) ->
    work_orders$Y.Coordinates
  coalesce2(work_orders$X.Coordinates.1, work_orders$X.Coordinates.1.1) ->
    work_orders$X.Coordinates.1
  coalesce2(work_orders$Y.Coordinates.1, work_orders$Y.Coordinates.1.1) ->
    work_orders$Y.Coordinates.1

  # Combine Pipe Material for Main and SLs
  coalesce2(work_orders$Pipe.Material, work_orders$Pipe.Material.1) ->
    work_orders$Pipe.Material

  # Combine SOBs
  coalesce2(work_orders$Shutoff.Block, work_orders$Shutoff.Block.1) ->
    work_orders$Shutoff.Block

  # Combine Length
  coalesce2(work_orders$Length, work_orders$Length.1) -> work_orders$Length

  # Combine Diameter
  coalesce2(work_orders$Internal.Diameter..mm., work_orders$Diameter) ->
    work_orders$Internal.Diameter..mm.

  drops <- c(
    "X.Coordinates.2", "Y.Coordinates.2", "X.Coordinates.1.1",
    "Y.Coordinates.1.1", "Pipe.Material.1", "Shutoff.Block.1",
    "Length.1", "Diameter"
  )

  work_orders[, !(names(work_orders) %in% drops)] -> work_orders

  # Removes work order rows for infrequent(<500) asset classes
  work_orders %>%
    dplyr::group_by(Class.Structure.1) %>%
    dplyr::filter(n() >= 500) %>%
    droplevels() %>%
    as.data.frame() -> work_orders

  levels(work_orders$Class.Structure.1) -> assetlist
  c(
    "Valve", "Customer Meter", "Drinking Water Hydrant",
    "Drinking Water Isolation Valve", "Drinking Water Pipes",
    "Drinking Water Sampling Tap", "Drinking Water Service Line"
  ) -> DWassets
  work_orders %>% dplyr::filter(Class.Structure.1 %in% DWassets) -> work_orders
  nrow(work_orders)

  # Hansen Tidy Up
  drops <- c("Water.Off.Duration", "ï..Service.Request.Number")
  work_orders_pre2015[, !(names(work_orders_pre2015) %in% drops)] ->
    work_orders_pre2015
  utils::head(work_orders_pre2015)

  # Combine Installation Date for different assets
  coalesce2(
    work_orders_pre2015$Installation.Date,
    work_orders_pre2015$Hydrant.Installation.Date
  ) ->
    work_orders_pre2015$Install.Date
  coalesce2(
    work_orders_pre2015$Install.Date,
    work_orders_pre2015$Water.Valve.Installation.Date
  ) ->
    work_orders_pre2015$Install.Date
  coalesce2(
    work_orders_pre2015$Install.Date,
    work_orders_pre2015$Water.Misc.Installation.Date
  ) ->
    work_orders_pre2015$Install.Date

  drops <- c(
    "Service.Location.Id.1", "Installation.Date",
    "Hydrant.Installation.Date", "Water.Valve.Installation.Date",
    "Water.Misc.Installation.Date", "Activity.Description"
  )
  work_orders_pre2015[, !(names(work_orders_pre2015) %in% drops)] ->
    work_orders_pre2015

  table(work_orders_pre2015$Asset.Type.Description)
  nrow(work_orders_pre2015)

  # Need to check this.. Hansen query was for Water only???.
  # Remove any not Drinking water related
  c(
    "Water Service Line", "Water Main", "Hydrant", "Water Valve",
    "Water Node", "Water Meter"
  ) -> keeps
  work_orders_pre2015 %>% dplyr::filter(Asset.Type.Description %in% keeps) ->
    work_orders_pre2015

  work_orders_pre2015$Asset.Id

  # noted two Asset IDS in Hansen with doiffering numbers in some cases
  summary(work_orders_pre2015$Asset.Id)
  summary(work_orders_pre2015$Asset.Id.2)


  # combine work order data sets and clean new data set
  dplyr::left_join(work_orders, work_orders_pre2015,
            by =
              c("Work.Order.Number" = "Work.Order.Number")
  ) -> work_orders_joined

  # combine number of work order outages
  coalesce2(
    work_orders_joined$Number.of.Work.Order.Water.Outages,
    work_orders_joined$Shut.Off.Instance
  ) ->
    work_orders_joined$Number.of.Work.Order.Water.Outages
  drops <- c("Shut.Off.Instance")
  work_orders_joined[, !(names(work_orders_joined) %in% drops)] ->
    work_orders_joined

  # combine install dates
  coalesce2(
    work_orders_joined$Install.Date.x,
    work_orders_joined$Install.Date.y
  ) -> work_orders_joined$Install.Date
  as.Date(lubridate::ymd_hms(work_orders_joined$Install.Date)) ->
    work_orders_joined$Install.Date
  drops <- c("Install.Date.x", "Install.Date.y")
  work_orders_joined[, !(names(work_orders_joined) %in% drops)] ->
    work_orders_joined

  # combine service location
  coalesce2(
    work_orders_joined$Service.Location.Id.x,
    work_orders_joined$Service.Location.Id.y
  ) ->
    work_orders_joined$Service.Location.Id
  drops <- c("Service.Location.Id.x", "Service.Location.Id.y")
  work_orders_joined[, !(names(work_orders_joined) %in% drops)] ->
    work_orders_joined

  # combine water distribution zone
  coalesce2(
    work_orders_joined$Water.Distribution.Zone.x,
    work_orders_joined$Water.Distribution.Zone.y
  ) ->
    work_orders_joined$Water.Distribution.Zone
  drops <- c("Water.Distribution.Zone.x", "Water.Distribution.Zone.y")
  work_orders_joined[, !(names(work_orders_joined) %in% drops)] ->
    work_orders_joined

  # combine reported date
  coalesce2(
    work_orders_joined$Reported.Date.x,
    work_orders_joined$Reported.Date.y
  ) -> work_orders_joined$Reported.Date
  as.Date(work_orders_joined$Reported.Date) -> work_orders_joined$Reported.Date
  drops <- c("Reported.Date.x", "Reported.Date.y")
  work_orders_joined[, !(names(work_orders_joined) %in% drops)] ->
    work_orders_joined

  # combine internal
  coalesce2(
    work_orders_joined$Internal.Diameter..mm.,
    work_orders_joined$Pipe.Diameter
  ) ->
    work_orders_joined$Internal.Diameter..mm.
  drops <- c("Pipe.Diameter")
  work_orders_joined[, !(names(work_orders_joined) %in% drops)] ->
    work_orders_joined

  # combine length
  coalesce2(work_orders_joined$Length, work_orders_joined$Pipe.Length) ->
    work_orders_joined$Length
  drops <- c("Pipe.Length")
  work_orders_joined[, !(names(work_orders_joined) %in% drops)] ->
    work_orders_joined

  as.data.frame(work_orders_joined) -> combined.df

  drops <- c("Asset.Id", "Asset.Id.2")
  combined.df[, !(names(combined.df) %in% drops)] -> combined.df
  nrow(combined.df)

  # adds additional rows due to duplicated work order numbers, need to remove these...
  # consolidate duplicated work order ID and find the Max No. SLIDS affected
  combined.df %>%
    dplyr::group_by(Work.Order.Number) %>%
    dplyr::filter(n() > 1) %>%
    dplyr::arrange(Work.Order.Number) %>%
    dplyr::summarise(
      Number.of.Work.Order.Water.Outages =
        max(Number.of.Work.Order.Water.Outages)
    ) ->
    Number.of.Work.Order.Water.Outages
  as.data.frame(Number.of.Work.Order.Water.Outages) ->
    Number.of.Work.Order.Water.Outages
  Number.of.Work.Order.Water.Outages$Work.Order.Number ->
    duplicateWOID
  Number.of.Work.Order.Water.Outages$Number.of.Work.Order.Water.Outages ->
    duplicatenumberoutages

  combined.df[!duplicated(combined.df$Work.Order.Number), ] -> combined.df
  combined.df[combined.df$Work.Order.Number %in% duplicateWOID, ]$
    Number.of.SLIDs.Affected <- duplicatenumberoutages

  combined.df[is.na(combined.df$Number.of.Work.Order.Water.Outages), ] -> NAoutages
  combined.df[combined.df$Number.of.Work.Order.Water.Outages == 0, ] -> Notoutages

  as.Date(last(work_orders_pre2015$Reported.Date)) -> transitionDate

  nrow(combined.df)

  # Replace NA with 0 for
  is.na(combined.df$Water.Off.Date.Time) & combined.df$
    Reported.Date < transitionDate -> testss
  combined.df$Number.of.Work.Order.Water.Outages[testss] <- 0

  combined.df$Number.of.Work.Order.Water.Outages[is.na(
    combined.df$Number.of.Work.Order.Water.Outages
  )] <- 0

  # total workorders by assetID
  nrow(combined.df)
  combined.df %>%
    dplyr::group_by(Asset.Number) %>%
    dplyr::mutate(Nworkordersperasset = n()) %>%
    as.data.frame() -> work_orders

  # remove previous data files
  rm(work_orders_joined)
  rm(work_orders_pre2015)
  rm(work_orders)
  gc()

  # convert date time
  lubridate::ymd_hms(combined.df$Water.Off.Date.Time) -> combined.df$Water.Off.Date.Time
  lubridate::ymd_hms(combined.df$Water.On.Date.Time) -> combined.df$Water.On.Date.Time

  # reduce list of repair types, shift to before duplicates
  # (this is reducing number of work orders significantly)
  my_list <- c(
    "Repair Burst Water Main", "WQ Street Flush", "Repair Ferrule",
    "Water Hydrant Repair Replace", "Repair Water Service",
    "Repair Valve", "Water Valve Inspection",
    "Repair Pressure Reducing Valve", "Repair Leaking Water Main",
    "Replace/Renew Water Service", "Valve Insertion"
  )
  combined.df[combined.df$Class.Structure %in% my_list, ] -> combined.df


  # Get rid of duplicates (be sure we are keeping the relevant ones??)
  # this is still halving number after filtering why
  combined.df %>%
    dplyr::arrange(Shutoff.Block, Reported.Date) %>%
    dplyr::distinct(Service.Location.Id, Work.Order.Number, .keep_all = TRUE) ->
    combined.df

  # add new column number of workorders by shutoff block,
  # do this after we fill missing SOBs
  combined.df %>%
    dplyr::arrange(Reported.Date) %>%
    dplyr::group_by(Shutoff.Block) %>%
    dplyr::mutate(totalNpershutoffblock = n()) -> combined.df

  # total outages by asset ID
  combined.df %>%
    dplyr::group_by(Asset.Number) %>%
    dplyr::mutate(Noutagesperasset = sum(Number.of.Work.Order.Water.Outages)) ->
    combined.df

  # add a field time between work order dates for each instance and asset
  combined.df %>%
    dplyr::arrange(Asset.Number, Reported.Date) %>%
    dplyr::group_by(Asset.Number) %>%
    dplyr::mutate(time_since_asset_failure = as.numeric
           (difftime(Reported.Date, dplyr::lag(Reported.Date),
                     units = "days"
           ))) -> asset_data

  cleanedData <- list(asset_data, asset_data)
  return(cleanedData)
}
