
#' getBOM
#'This function processes climate data from BOM including temp, rainfall,
# solar, and combines it with any existing climate data that has already been
# processed, returns climate rds with all climate data new and existing, if
# update is needed else returns current processed climate data.
#'
#' @param update logical
#' @param ClimatePath character
#'
#' @return  date frame of climate data
#'
#' @examples \dontrun{
#' getBOM(update=FALSE, ClimatePath="PathtoClimateData")
#' }
getBOM <- function(update, ClimatePath) {

  # pulls in and reformats data to be updated
  if (update) {

    # read in current data
    utils::read.csv(ClimatePath, header = TRUE) -> climate_data_existing
    climate_data_existing$Date <- as.Date(climate_data_existing$Date)
    start <- as.Date(dplyr::last(climate_data_existing$Date))
    start2 <- as.Date("1995/01/01")
    c(start, start2) -> temp
    temp[which.max(temp)] -> start

    # get data from correct stations
    bomrang::sweep_for_stations(c(-37.80, 145.15)) -> stations
    utils::head(stations, 10)
    stations$site[9] -> stationID
    bomrang::get_historical(stationID, type = c("min")) -> min_temp
    bomrang::get_historical(stationID, type = c("max")) -> max_temp
    bomrang::get_historical(stationID, type = c("rain")) -> rain
    bomrang::get_historical(stationID, type = c("solar")) -> solar

    # formatting dates
    min_temp$Date <- as.Date(
      with(min_temp, paste(year, month, day, sep = "-")),
      "%Y-%m-%d"
    )
    max_temp$Date <- as.Date(
      with(max_temp, paste(year, month, day, sep = "-")),
      "%Y-%m-%d"
    )
    rain$Date <- as.Date(
      with(rain, paste(year, month, day, sep = "-")),
      "%Y-%m-%d"
    )
    solar$Date <- as.Date(
      with(solar, paste(year, month, day, sep = "-")),
      "%Y-%m-%d"
    )

    dplyr::select(min_temp, Date, min_temperature) -> min_temp
    dplyr::select(max_temp, Date, max_temperature) -> max_temp
    dplyr::select(rain, Date, rainfall) -> rain
    dplyr::select(solar, Date, solar_exposure) -> solar
    solar[is.na(solar$solar_exposure), "solar_exposure"] <- 0

    # combining different climate data
    merge(min_temp, max_temp, by = "Date", all = TRUE) -> climate
    merge(climate, rain) -> climate
    merge(climate, solar) -> climate
    climate %>% dplyr::filter(Date > start) -> climate
    climate$Date -> Date

    # combine old with new data
    colnames(climate_data_existing)[-1] <- colnames(climate)
    rbind(climate_data_existing[, -1], climate) -> climate
    climate -> my_data
    summary(my_data)
    my_data$Date -> Date2
    nrow(my_data) -> A
    sum(stats::complete.cases(my_data)) -> B

    # checks rows against complete cases if not same smooth data
    if (B < A) {
      stats::as.ts(my_data$Date) -> my_data$Date
      t(my_data) -> tmy.data2

      tmy.data2[2:nrow(tmy.data2), ] -> tmy.data2
      apply(tmy.data2, 2, as.numeric) -> tmy.data2

      # univariate model using MARSS and Kalman Smoothing
      xhat <- list()
      for (j in 1:nrow(tmy.data2)) {
        print(paste0("running ", j, " of ", nrow(tmy.data2)))
        tmy.data2[j, ] -> x

        kemfit2 <- MARSS::MARSS(x)
        kf2 <- MARSS::MARSSkfss(kemfit2)
        kf2$xtT[1, ] -> xhat[[j]]
      }

      do.call("cbind", xhat) -> df_imputed
      ncol(df_imputed)
      as.data.frame(df_imputed) -> df_imputed
      cbind("DATE" = Date2, df_imputed) -> df_imputed
      colnames(df_imputed) <- colnames(climate)
      df_imputed -> climate
      utils::write.csv(climate, ClimatePath, row.names = FALSE) -> climate
    }
  }else
    {utils::read.csv(ClimatePath, header = TRUE) -> climate}

  return(climate)
}


#' GetSoil Data
#'
#' This function gets soil data i.e. clay, sand ect... by longitude and latitude
#  and returns as a table

#' @param lon1 numeric
#' @param soil_data dataframe
#' @param lat1 numeric
#'
#' @return
#'
#' @examples \dontrun{
#' get.soil.data(lat=37.8, lon1=-144)
#' }
get.soil.data <- function(lat1, lon1, soil_data) {

  # Get unique soil data by long and lat
  unique(soil_data$x[which(abs(soil_data$x - lon1) ==
                             min(abs(soil_data$x - lon1)))]) -> lon1
  unique(soil_data$y[which(abs(soil_data$y - lat1) ==
                             min(abs(soil_data$y - lat1)))]) -> lat1
  soil_data %>% dplyr::filter(x == lon1 & y == lat1) -> soil_table

  return(soil_table)
}


#' Import
#'
#'This function is to import all requird data sources for
# ML and modeling purposes, the output is list of 9 data tables used throughout the project

#' @param AssetPath  Path to Asset Data csv
#' @param WorkOrderPath Path to Work Order Maximo Data csv
#' @param HansenPath Path to Hansen Work Order Data csv
#' @param newWorkOrdersPath Path to New Maximo Work order Data csv
#' @param rainfall Path to rainfall grid data RDS
#' @param minTemp Path to min temp grid data RDS
#' @param maxTemp Path to max temp data RDS
#' @param soilData Path to Asset Data tif files
#' @param GISdata Path to GIS Data csv
#' @param ClimatePath character
#' @param SOBSLID dataframe
#'
#' @return  list of dataframes
#'
#' @export
#' @examples \dontrun{
#' import_data(AssetPath="~Data/assetData.csv")
#' }
import_data <- function(AssetPath, WorkOrderPath, HansenPath, newWorkOrdersPath, rainfall, minTemp, maxTemp, soilData, GISdata, ClimatePath, SOBSLID) {

  # Read #Keep this for Total Asset Statistics
  data.table::fread(AssetPath, header = TRUE, sep = ",", fill = TRUE) -> asset_data
  data.table::setDF(asset_data) -> asset_data
  make.names(colnames(asset_data), unique = TRUE) -> colnames(asset_data)

  # Read in climate data
  getBOM(update = FALSE, ClimatePath) -> climate_data

  # Core work order data from BI
  # Combine work orders from multiple files

  data.table::fread(WorkOrderPath, header = T, sep = ",", fill = TRUE) ->
    work_orders

  AddData <- function(path) {
    data.table::fread(path, header = T, sep = ",", fill = TRUE) -> work_orders_2019

    data.table::setDF(work_orders) -> work_orders
    data.table::setDF(work_orders_2019) -> work_orders_2019

    make.names(colnames(work_orders), unique = TRUE) -> colnames(work_orders)
    make.names(colnames(work_orders_2019), unique = TRUE) -> colnames(work_orders_2019)

    #as.Date(lubridate::ymd_hms(work_orders$Reported.Date)) -> work_orders$Reported.Date

    colnames(work_orders) %in% colnames(work_orders_2019)
    colnames(work_orders) -> colnames(work_orders_2019)

    grep("Work.Order.Number", colnames(work_orders_2019)) -> Pos
    grep("Work.Order.Number", colnames(work_orders)) -> Pos2

    work_orders_2019[!work_orders_2019[, Pos] %in% work_orders[, Pos2], ] ->
      work_orders_2019

   # as.Date(lubridate::ymd_hms(work_orders_2019$Reported.Date)) ->
   #   work_orders_2019$Reported.Date

    rbind(work_orders_2019, work_orders) -> work_orders
    dplyr::arrange(work_orders, dplyr::desc(Reported.Date)) -> work_orders

    return(work_orders)
  }

  AddData(path = newWorkOrdersPath) -> work_orders

  # read Grid data
  readRDS(rainfall) -> raindfall.GRID
  readRDS(minTemp) -> mintemp.GRID
  readRDS(maxTemp) -> maxtemp.GRID

  # Read in Hansen Data
  data.table::fread(HansenPath,
                    header = TRUE,
                    na.strings = c("", "NA")
  ) -> work_orders_pre2015

  data.table::setDF(work_orders_pre2015) -> work_orders_pre2015
  make.names(colnames(work_orders_pre2015), unique = TRUE) ->
    colnames(work_orders_pre2015)

  dplyr::arrange(work_orders, Reported.Date) -> work_orders
  dplyr::arrange(work_orders_pre2015, Problem.Date.Time) -> work_orders_pre2015

  # Get Soil Data
  current.list <- list.files(
    path = soilData,
    pattern = ".tif$", full.names = TRUE
  )
  out <- list()
  for (i in 1:length(current.list)) {
    SOIL_dem <- raster::raster(x = current.list[[i]])
    xyz <- raster::rasterToPoints(SOIL_dem)
    xyz[, 3] -> out[[i]]
    xyz[, c(1:2)] -> xycoord
  }
  do.call(cbind, out) -> soil_table

  dim(soil_table)
  as.data.frame(cbind(xycoord, soil_table)) -> soil_table
  colnames(soil_table) <- c(
    "x", "y", "BulkDensity", "Clay", "DepthofSoil",
    "CationExchange", "pH", "totalP", "Silt", "Sand"
  )


  data.table::fread(GISdata, header = TRUE, sep = ",") ->
    static_pressure

  # Read in static pressure data

    dplyr::select(static_pressure,
      SHUTOFF_BLOCK, ASSETID, X_1, Y_1, Z_1, X_2, Z_1, Z_2,
      Static_Pressure_Final
    ) ->temp1


    dplyr::filter(temp1, !Z_1 %in% NA) ->temp2
    as.data.frame(temp2) -> static_pressure_reduced


    #Read in SOB data

    # read in table of Asset Number, SLID and SOBIDs
    data.table::fread(SOBSLID, header = TRUE, sep = ",", fill = TRUE) -> SOBAsset

  # save as list
  data.frame.list <- list(
    work_orders_pre2015,
    work_orders,
    asset_data,
    raindfall.GRID,
    mintemp.GRID,
    maxtemp.GRID,
    climate_data,
    soil_table,
    static_pressure_reduced,
    SOBAsset

  )

  return(data.frame.list)
}




