#' Title
#'
#' @param minNinSOB
#' @param asset_data
#' @param work_Order_Data
#' @param SOBNr
#' @param TI1
#' @param TI2
#' @param FNRupp
#' @param FNRlow
#' @param plot
#' @param minpkh
#' @param rollingwin
#' @param inclsoilmoist
#'
#' @return
#' @export
#'
#' @examples
NHPP_fit <- function(minNinSOB, asset_data, soil_data, work_Order_Data, SOBNr, TI1, TI2, FNRupp, FNRlow, plot, minpkh, rollingwin, inclsoilmoist) {
  result <- list(c())
  SB_IDs[SOBNr] -> SB
  print(SB)

  work_Order_Data %>%
    dplyr::filter(Shutoff.Block %in% SB) %>%
    dplyr::select(Reported.Date) %>%
    dplyr::distinct(as.Date(Reported.Date)) -> Fail_Dates

  work_Order_Data %>%
    dplyr::filter(Shutoff.Block %in% SB) %>%
    dplyr::filter(Class.Structure %in% "Repair Burst Water Main" | Class.Structure %in% "Repair Leaking Water Main") %>%
    dplyr::filter(Reported.Date %in% Val.Dates) %>%
    dplyr::distinct(Work.Order.Number) -> pipeFailval
  nrow(pipeFailval) -> pipeFailval

  work_Order_Data %>%
    dplyr::filter(Shutoff.Block %in% SB) %>%
    dplyr::filter(Class.Structure %in% "Repair Burst Water Main" | Class.Structure %in% "Repair Leaking Water Main") %>%
    dplyr::filter(Reported.Date %in% Test.Dates) %>%
    dplyr::distinct(Work.Order.Number) -> pipeFailtest
  nrow(pipeFailtest) -> pipeFailtest

  nrow(Fail_Dates) -> Nfails
  print(Nfails)
  work_Order_Data %>%
    dplyr::filter(Shutoff.Block %in% SB) %>%
    dplyr::distinct(as.Date(Reported.Date), .keep_all = TRUE) %>%
    dplyr::select(Install.Date) -> Installed.year
  work_Order_Data %>%
    dplyr::filter(Shutoff.Block %in% SB) %>%
    dplyr::distinct(as.Date(Reported.Date), .keep_all = TRUE) %>%
    dplyr::select(Class.Structure) -> Work_Order_Type

  # add some new code here NPipe <-count number of "Pipe burst and Pipe related in Failure
  # add some new code  Nhydrant <- count number of times hydrant in Failure cause
  length(grep("Repair Burst Water Main", Work_Order_Type[, ])) -> Nbursts
  length(grep("Water Hydrant Repair Replace", Work_Order_Type[, ])) -> NHydrant
  length(grep("Repair Water Service", Work_Order_Type[, ])) -> NSLines

  ifelse(is.null(Nbursts),
    {
      Nbursts <- 0
    },
    {
      Nbursts -> Nbursts
    }
  )

  ifelse(is.null(NHydrant),
    {
      NHydrant <- 0
    },
    {
      NHydrant -> NHydrant
    }
  )

  ifelse(is.null(NSLines),
    {
      NSLines <- 0
    },
    {
      NSLines -> NSLines
    }
  )

  work_Order_Data %>%
    dplyr::filter(Shutoff.Block %in% SB) %>%
    dplyr::select(Latitude) %>%
    dplyr::summarize(FUN = mean(Latitude, na.rm = TRUE)) -> mean_Latitude
  work_Order_Data %>%
    dplyr::filter(Shutoff.Block %in% SB) %>%
    dplyr::select(Longitude) %>%
    dplyr::summarize(FUN = mean(Longitude, na.rm = TRUE)) -> mean_Longitude


  work_Order_Data %>%
    dplyr::filter(Shutoff.Block %in% SB) %>%
    dplyr::select(Asset.Number, Length) %>%
    dplyr::distinct(Asset.Number, .keep_all = TRUE) -> Asset_with_WO
  length(unique(Asset_with_WO[, 1])) -> uniqueAssetsinWO
  nrow(Asset_with_WO) -> totalWOs
  uniqueAssetsinWO^2 / totalWOs^2 -> heterogeneity

  work_Order_Data %>%
    dplyr::filter(Shutoff.Block %in% SB) %>%
    dplyr::select(Asset.Number, Length) %>%
    dplyr::distinct(Asset.Number, .keep_all = TRUE) %>%
    dplyr::summarize(FUN = mean(Length[!Length == 0], na.rm = TRUE)) -> mean_Fail_length
  work_Order_Data %>%
    dplyr::filter(Shutoff.Block %in% SB) %>%
    dplyr::select(Asset.Number, Length) %>%
    dplyr::distinct(Asset.Number, .keep_all = TRUE) %>%
    dplyr::summarize(FUN = sum(Length[!Length == 0], na.rm = TRUE)) -> total_Fail_length

  work_Order_Data %>%
    dplyr::filter(Shutoff.Block %in% SB) %>%
    dplyr::select(N.slids.affected) %>%
    dplyr::summarize(mean = mean(N.slids.affected, na.rm = TRUE)) %>%
    as.numeric() -> NSLIDaffected
  if (is.na(NSLIDaffected)) {
    NSLIDaffected <- 0
  }

  asset_data %>%
    dplyr::filter(Shutoff.Block %in% SB) %>%
    dplyr::select(Asset.Number, Length) %>%
    dplyr::distinct(Asset.Number, .keep_all = TRUE) %>%
    dplyr::summarize(FUN = mean(Length[!Length == 0], na.rm = TRUE)) -> mean_length
  asset_data %>%
    dplyr::filter(Shutoff.Block %in% SB) %>%
    dplyr::select(Asset.Number, Length) %>%
    dplyr::distinct(Asset.Number, .keep_all = TRUE) %>%
    dplyr::summarize(FUN = sum(Length[!Length == 0], na.rm = TRUE)) -> total_length

  # note addition of is.finite
  work_Order_Data %>%
    dplyr::filter(Shutoff.Block %in% SB) %>%
    dplyr::select(Length, intensity) %>%
    dplyr::filter_all(dplyr::all_vars(is.finite(.))) %>%
    dplyr::mutate(Weighted = Length / sum(Length, na.rm = TRUE) * intensity) %>%
    dplyr::summarize(sum(Weighted, na.rm = TRUE)) %>%
    as.numeric() -> cohortIntensity
  work_Order_Data %>%
    dplyr::filter(Shutoff.Block %in% SB) %>%
    dplyr::select(Length, FNR_power) %>%
    dplyr::filter_all(dplyr::all_vars(is.finite(.))) %>%
    dplyr::mutate(Weighted = Length / sum(Length, na.rm = TRUE) * FNR_power) %>%
    dplyr::summarize(sum(Weighted, na.rm = TRUE)) %>%
    as.numeric() -> cohortFNR
  work_Order_Data %>%
    dplyr::filter(Shutoff.Block %in% SB) %>%
    dplyr::select(Length,PowerLaw1.eta) %>%
    dplyr::filter_all(dplyr::all_vars(is.finite(.))) %>%
    dplyr::mutate(Weighted = Length / sum(Length, na.rm = TRUE) * PowerLaw1.eta) %>%
    dplyr::summarize(sum(Weighted, na.rm = TRUE)) %>%
    as.numeric() -> cohortEta
  work_Order_Data %>%
    dplyr::filter(Shutoff.Block %in% SB) %>%
    dplyr::select(Length,  PowerLaw1.beta) %>%
    dplyr::filter_all(dplyr::all_vars(is.finite(.))) %>%
    dplyr::mutate(Weighted = Length / sum(Length, na.rm = TRUE) * PowerLaw1.beta) %>%
    dplyr::summarize(sum(Weighted, na.rm = TRUE)) %>%
    as.numeric() -> cohortBeta

  # note addition of is.finite
  work_Order_Data %>%
    dplyr::filter(Shutoff.Block %in% SB) %>%
    dplyr::select(Asset.Number, PowerLaw1.beta, PowerLaw1.eta, FNR_power, intensity) %>%
    dplyr::filter_all(dplyr::all_vars(is.finite(.))) %>%
    dplyr::distinct(Asset.Number, .keep_all = TRUE) %>%
    dplyr::summarize_all(min, na.rm = TRUE) -> MincohortFailure
  ifelse(is.infinite(MincohortFailure[[3]]), 0,
    as.numeric(MincohortFailure[3])
  ) -> MincohortEta

  # note addition of is.finite
  work_Order_Data %>%
    dplyr::filter(Shutoff.Block %in% SB) %>%
    dplyr::select(Asset.Number, PowerLaw1.beta, PowerLaw1.eta, FNR_power, intensity) %>%
    dplyr::filter_all(dplyr::all_vars(is.finite(.))) %>%
    dplyr::distinct(Asset.Number, .keep_all = TRUE) %>%
    dplyr::summarize_all(max, na.rm = TRUE) -> MaxcohortFailure

  ifelse(is.infinite(MaxcohortFailure[[4]]), 0,
    as.numeric(MaxcohortFailure[4])
  ) -> MaxcohortFNR
  ifelse(is.infinite(MaxcohortFailure[[5]]), 0,
    as.numeric(MaxcohortFailure[5])
  ) -> MaxcohortIntensity
  ifelse(is.infinite(MaxcohortFailure[[2]]), 0,
    as.numeric(MaxcohortFailure[2])
  ) -> MaxcohortBeta

  # TO ADD Get Specific AC and CI length
  asset_data %>%
    dplyr::filter(Shutoff.Block %in% SB) %>%
    dplyr::group_by(Pipe.Material) %>%
    dplyr::select(Asset.Number, Pipe.Material, Length) %>%
    dplyr::distinct(Asset.Number, .keep_all = TRUE) %>%
    dplyr::filter(Pipe.Material %in% "CAST IRON CEMENT LINED") %>%
    dplyr::tally(Length) %>%
    as.data.frame() -> LengthCI
  LengthCI[1, 2] -> LengthCI

  asset_data %>%
    dplyr::filter(Shutoff.Block %in% SB) %>%
    dplyr::group_by(Pipe.Material) %>%
    dplyr::select(Asset.Number, Pipe.Material, Length) %>%
    dplyr::distinct(Asset.Number, .keep_all = TRUE) %>%
    dplyr::filter(Pipe.Material %in% "ASBESTOS CEMENT(INCL HARDYFLEX)") %>%
    dplyr::tally(Length) %>%
    as.data.frame() -> LengthACSOB
  LengthACSOB[1, 2] -> LengthACSOB

  if (is.na(LengthACSOB)) LengthACSOB <- 0
  if (is.na(LengthCI)) LengthCI <- 0

  # Added this length weighted mean diameter
  asset_data %>%
    dplyr::filter(Shutoff.Block %in% SB) %>%
    dplyr::select(Asset.Number, Length, Nominal.Pipe.Size..mm.) %>%
    dplyr::distinct(Asset.Number, .keep_all = TRUE) %>%
    dplyr::mutate(LengthweightedDiameter = (Length / sum(Length)) * Nominal.Pipe.Size..mm.) %>%
    dplyr::summarize(sum(LengthweightedDiameter, na.rm = TRUE)) -> diameter

  asset_data$Install.Date<-as.numeric(asset_data$Install.Date)

  asset_data %>%
    dplyr::filter(Shutoff.Block %in% SB) %>%
    dplyr::select(Install.Date) %>%
    dplyr::summarize(FUN = mean(Install.Date[!Install.Date == 0], na.rm = TRUE)) -> Install.year
  max(Install.year) -> max_Install.year

  work_Order_Data %>%
    dplyr::filter(Shutoff.Block %in% SB) %>%
    dplyr::distinct(Asset.Number, .keep_all = TRUE) %>%
    dplyr::select(Length, Static_Pressure_Final) %>%
    dplyr::mutate(LengthweightedPressure = (Length / sum(Length)) * Static_Pressure_Final) %>%
    dplyr::summarize(sum(LengthweightedPressure, na.rm = TRUE)) -> mean.staticPressure

   work_Order_Data %>%
    dplyr::filter(Shutoff.Block %in% SB) %>%
    dplyr::distinct(Asset.Number, .keep_all = TRUE) %>%
    dplyr::select(Static_Pressure_Final) %>%
    dplyr::summarize(max(Static_Pressure_Final, na.rm = TRUE)) -> max.staticPressure

  ifelse(is.infinite(max.staticPressure[[1]]), 0,
    as.numeric(max.staticPressure[[1]])
  ) -> max.staticPressure

  asset_data %>%
    dplyr::filter(Shutoff.Block %in% SB) %>%
    dplyr::summarise(Nassets = dplyr::n_distinct(Asset.Number)) -> assetspershutoff

  work_Order_Data %>%
    dplyr::filter(Shutoff.Block %in% SB) %>%
    dplyr::select(Water.Distribution.Zone) -> WDZ
  if (!is.null(names(which.max(table(WDZ))))) {
    names(which.max(table(WDZ))) -> WDZ
  } else {
    WDZ <- "Unknown"
  }

  # Get Soil Moisture
  if (inclsoilmoist == TRUE) {
    SoilMoisture <- list()
    for (i in 1:nrow(Fail_Dates)) {
      print(paste0("SoilMoisture", i))
      get.soil.moisture2(date = Fail_Dates[i, ], lat1 = mean_Latitude, lon1 = mean_Longitude) -> SoilMoisture[[i]]
    }
    do.call(rbind, SoilMoisture) -> SoilMoisture
    max(SoilMoisture, na.rm = TRUE) -> MaxSM
    min(SoilMoisture, na.rm = TRUE) -> MinSM
    MaxSM - MinSM -> RangeSM
  }

  work_Order_Data %>%
    dplyr::filter(Shutoff.Block %in% SB) %>%
    dplyr::select(time_since_SOB_failure) -> TimesinceSOB

  work_Order_Data %>%
    dplyr::filter(Shutoff.Block %in% SB) %>%
    dplyr::select(N.slids.affected) -> NSLID
  max(NSLID[, 1], na.rm = TRUE) -> NSLID

  # Get Soil Data
  if (is.nan(mean_Latitude[, ])) {
    mean_Longitude[, ] <- 145.126
    mean_Latitude[, ] <- -37.8215
  }
  get.soil.data(lat1 = mean_Latitude[, ], lon1 = mean_Longitude[, ], soil_data = soil_data) -> soil_SOB
  soil_SOB$BulkDensity -> BD
  soil_SOB$Clay -> Clay
  soil_SOB$DepthofSoil -> DOS
  soil_SOB$CationExchange -> CE
  soil_SOB$pH -> pH
  soil_SOB$totalP -> totP
  soil_SOB$Silt -> Silt
  soil_SOB$Sand -> Sand

  work_Order_Data %>%
    dplyr::filter(Shutoff.Block %in% SB) %>%
    dplyr::distinct(as.Date(Reported.Date), .keep_all = TRUE) %>%
    dplyr::select(Pipe.Material) -> Pipe.Material2
  work_Order_Data %>%
    dplyr::filter(Shutoff.Block %in% SB) %>%
    dplyr::distinct(as.Date(Reported.Date), .keep_all = TRUE) %>%
    dplyr::select(Class.Structure) -> Class.Struc

  apply(Pipe.Material2, 2, function(x) gsub("^$|^ $", NA, x)) -> Pipe.Material2
  names(which.max(table(Class.Struc))) -> mode_Failure.Cause
  ifelse(is.null(names(which.max(table(Pipe.Material2)))),
    {
      mode_Pipe.Material <- "NA"
    },
    {
      names(which.max(table(Pipe.Material2))) -> mode_Pipe.Material
    }
  )

  as.numeric(mean(tail(diff(Fail_Dates[, ]), 3), na.rm = TRUE)) -> averagetimesincelast3
  work_Order_Data %>%
    dplyr::filter(Shutoff.Block %in% SB) %>%
    dplyr::distinct(as.Date(Reported.Date), .keep_all = TRUE) %>%
    dplyr::tally() -> totalNpershutoffblock

  # NHPP
  which(as.Date(All.Dates) %in% as.Date(Fail_Dates[, ])) -> Fail_dates_period
  array(0, dim = c(1, length(All.Dates))) -> px
  px[Fail_dates_period] <- 1
  d <- dplyr::first(Fail_dates_period)

  seq.Date(as.Date(start_date), as.Date(end_date), by = "day") -> Dates

  # replaced with new code, adds the rolling cumulative for repeats
  data.frame(times = cumsum(c(1, as.numeric(diff(Dates))))) -> df4
  df4 %>%
    dplyr::mutate(times = cumsum(c(1, as.numeric(diff(Dates))))) %>%
    dplyr::mutate(status = ifelse(Dates %in% as.Date(Fail_Dates[, ]), 1, 0)) %>%
    dplyr::mutate(cumsum = cumsum(status)) -> pipeFail
  pipeFail %>% dplyr::mutate(cum_rolling = zoo::rollapplyr(status, width = rollingwin, FUN = sum, partial = TRUE)) -> pipeFail

  # new code, repeats above for the validation period
  data.frame(times = cumsum(c(1, as.numeric(diff(Val.Dates))))) -> dfr
  dfr %>%
    dplyr::mutate(status = ifelse(Val.Dates %in% as.Date(Fail_Dates[, ]), 1, 0)) %>%
    dplyr::mutate(cumsum = cumsum(status)) -> Failval
  Failval %>% dplyr::mutate(cum_rolling = zoo::rollapplyr(status, width = rollingwin, FUN = sum, partial = TRUE)) -> Failval
  Nfailval <- tail(Failval$cumsum, 1)


  # new code, repeat above for the test period, this is separate to validation for ANN but should follow,  i.e. if want to test 12 months, in 2016, set validation to 3 years
  # if want to test 2017, validation =2, 2018, validation = 1,

  data.frame(times = cumsum(c(1, as.numeric(diff(Test.Dates))))) -> dft
  dft %>%
    dplyr::mutate(status = ifelse(Test.Dates %in% as.Date(Fail_Dates[, ]), 1, 0)) %>%
    dplyr::mutate(cumsum = cumsum(status)) -> Failtest
  Failtest %>% dplyr::mutate(cum_rolling = zoo::rollapplyr(status, width = rollingwin, FUN = sum, partial = TRUE)) -> Failtest
  Nfailtest <- tail(Failtest$cumsum, 1)


  # dont run the NHPP fitting on SOB with limited failures, <maxNinSON
  if (as.numeric(totalNpershutoffblock) > minNinSOB) {

    # Step 1 Plot Cumulative Count,  if slope is not constant implies a NHPP minimal repair/deteriorating or improving system
    if (plot == TRUE) {
      plot(pipeFail$times, pipeFail$cumsum)
      plot(pipeFail$times, pipeFail$status)
      plot(pipeFail$times, pipeFail$cum_rolling, type = "l")
    }

    pracma::findpeaks(pipeFail$cum_rolling, zero = "+", minpeakheight = minpkh, minpeakdistance = 5) -> peakdf
    peakdf[, 2] -> peakDays
    peakDays + as.Date(start_date) -> peakDates
    sort(peakDates) -> peakDates2

    peakDateList <- list()
    ifelse(length(peakDates2) > 0,
      {
        for (k in 1:length(peakDates2)) {
          which(abs(as.numeric((as.Date(Fail_Dates[, ]) - peakDates2[[k]]))) == min(abs(as.numeric(as.Date(Fail_Dates[, ]) - peakDates2[[k]])))) -> f
          as.Date(Fail_Dates[f, ]) -> peakDateList[[k]]
        }
        peakDates3 <- do.call("c", peakDateList)

        work_Order_Data %>%
          dplyr::filter(Shutoff.Block %in% SB) %>%
          dplyr::filter(as.Date(Reported.Date) %in% peakDates) %>%
          dplyr::select(Class.Structure) -> peakClassStructure
      },
      {
        peakClassStructure <- NA
      }
    )

    ifelse(!is.null(peakdf),
      {
        ifelse(nrow(peakdf) > 1,
          {
            nrow(peakdf) - 1 -> h
            kmeans(peakdf[, 2:4], h) -> clusterpeak
            clusterpeak$centers[, 1] -> peak_timespeakdf
          },
          {
            peak_timespeakdf <- "NA"
          }
        )
      },
      {
        peak_timespeakdf <- "NA"
      }
    )

    max(pipeFail$times) -> end_time

    # plot of time versus cumulative recurrences
    ct <- Fail_dates_period
    cf <- 1:length(ct)

    # added code to compute the time to next failure
    # pad cts with 0 and end time, left is truncated right is censored.
    diff(c(0, ct, (end_time + 1))) -> diffct

    # duane(diffct)

    my_seq <- function(x) {
      seq(x, 1, -1)
    }
    lapply(diffct, FUN = my_seq) -> temp
    unlist(temp) -> timetofailure

    cbind(pipeFail, head(timetofailure, -1)) -> pipeFail
    names(pipeFail)[5] <- "timetofailure"

    if (plot == TRUE) {
      plot(ct, cf, type = "p", xlab = "Time", ylab = "Cumulative recurrences")
    }
    if (plot == TRUE) {
      plot(pipeFail$times, pipeFail$timetofailure, type = "l", xlab = "Time", ylab = "Time to Failure")
    }


    pipeFail %>%
      dplyr::filter(status == 1) %>%
      dplyr::mutate(cumN = times / cumsum) -> K
    # duane plot
   # plot(K$times, K$cumN, log = c("x", "y"), ylim = c(0, 400))
   # plot(log10(K$times), log10(K$cumN))


    with(rle(diffct < rollingwin), sum(lengths[values] >= minpkh)) -> n_greaterthan2peaks_lessthan45

    # non-parametric estimate of the intensity
    start_dateT <- 0 # lower bound of the observation window
    endT <- max(ct) # upper bound of the observation window
    sigma <- 90 # bandwidth for Gaussian kernel
    densityEstimate <- density(ct, from = start_dateT, to = endT, n = 200, bw = sigma) # Gaussian kernel
    timesD <- densityEstimate$x # extract times
    nRecurrences <- length(ct) # total number of recurrences
    intensityEstimate <- densityEstimate$y * nRecurrences # estimate of the intensity
    # implement edge correction
    edgeCorrection <- pnorm(endT, timesD, sigma) - pnorm(start_dateT, timesD, sigma)
    # plot log(intensity) against time

    if (plot == TRUE) {
      plot(timesD, log(intensityEstimate / edgeCorrection),
        type = "p",
        xlim = c(start_dateT, endT), xlab = "Time", ylab = "log(intensity)"
      )
    }

    ## fit NHPP-model to temporal data

    # function for fitting a nonhomogeneous Poisson process (NHPP) model   #Tend=max(times)
    fitNHPP <- function(times, Tend, intensity) {
      #-times is a vector with the observed event times
      #-Tend is the upper bound of the observation window
      #-intensity specifies the type of intensity function;
      # a power law ("power"), loglinear ("loglinear"), or a homogeneous
      # ("homogeneous") intensity function

      nobs <- length(times)

      if (intensity == "power") {
        # MLE for beta and eta
        beta <- nobs / sum(log(Tend / times))
        eta <- Tend / (nobs^(1 / beta))
        # log-likelihood of the power-NHPP
        llp <- function(theta, times, Tend) {
          beta <- theta[1]
          eta <- theta[2]
          nobs * log(beta) - nobs * beta * log(eta) + (beta - 1) * sum(log(times)) - (Tend / eta)^beta
        }
        # refit the power-NHPP by optimization of the log-likelihood function
        pow <- optim(c(beta, eta), llp,
          method = "BFGS", control = list(fnscale = -1),
          times = times, Tend = Tend, hessian = TRUE
        )
        parameters <- pow$par
        names(parameters) <- c("beta", "eta")
        loglik <- pow$value
        hessian <- pow$hessian
      }

      else if (intensity == "loglinear") {
        # MLE for gamma0 and gamma1
        rootgamma1 <- function(nobs, times, Tend, gamma1) {
          sum(times) + nobs / gamma1 - (nobs * Tend * exp(gamma1 * Tend)) / (exp(gamma1 * Tend) - 1)
        }
        gamma1 <- fzero(rootgamma1, x = 1e-3, nobs = nobs, times = times, Tend = Tend)$x
        gamma0 <- log((nobs * gamma1) / (exp(Tend * gamma1) - 1))
        # log-likelihood of the loglinear-NHPP
        lll <- function(theta, times, Tend) {
          gamma0 <- theta[1]
          gamma1 <- theta[2]
          nobs * gamma0 + gamma1 * sum(times) - (exp(gamma0) * (exp(gamma1 * Tend) - 1)) / gamma1
        }
        # refit the loglinear-NHPP by optimization of the log-likelihood function
        loglin <- optim(c(gamma0, gamma1), lll,
          method = "BFGS", control = list(fnscale = -1),
          times = times, Tend = Tend, hessian = TRUE
        )
        parameters <- loglin$par
        names(parameters) <- c("gamma0", "gamma1")
        loglik <- loglin$value
        hessian <- loglin$hessian
      }

      else if (intensity == "homogeneous") {
        # MLE for lambda
        lambda <- length(times) / Tend
        # log-likelihood of the homogeneous-NHPP
        lll <- function(theta, times, Tend) {
          lambda <- theta[1]
          ni <- length(times)
          ni * log(lambda) - lambda * Tend
        }
        # refit the homogeneous-NHPP by optimization of the log-likelihood function
        hom <- optim(lambda, lll,
          method = "BFGS", control = list(fnscale = -1),
          times = times, Tend = Tend, hessian = TRUE
        )
        parameters <- hom$par
        names(parameters) <- c("lambda")
        loglik <- hom$value
        hessian <- hom$hessian
      }

      list(
        parameters = parameters, loglik = loglik, AIC = -2 * loglik + 2 * length(parameters),
        intensity = intensity, hessian = hessian, times = times, Tend = Tend
      )
    }

    # fit the power-NHPP (that is, NHPP model with a power law function as intensity)
    try(ppPower <- fitNHPP(
      times = ct, Tend = end_time + 1, # times=ct[-length(ct)], Tend=ct[length(ct)],
      intensity = "power"
    ))[1:4]
    # 95% confidence intervals for beta and eta (normal-approximation)
    SEparms <- sqrt(diag(solve(-1 * ppPower$hessian)))
    as.numeric(ppPower$parameters[1]) * exp(c(1, -1) * qnorm(.05 / 2) * SEparms[1] / ppPower$parameters[1])[1] -> b_lower
    as.numeric(ppPower$parameters[1]) * exp(c(1, -1) * qnorm(.05 / 2) * SEparms[1] / ppPower$parameters[1])[2] -> b_upper

    as.numeric(ppPower$parameters[2]) * exp(c(1, -1) * qnorm(.05 / 2) * SEparms[2] / ppPower$parameters[2])[1] -> eta_lower
    as.numeric(ppPower$parameters[2]) * exp(c(1, -1) * qnorm(.05 / 2) * SEparms[2] / ppPower$parameters[2])[2] -> eta_upper

    ppPower$AIC -> power_AIC
    ppPower$loglik -> power_loglik

    # fit loglinear-NHPP (that is, NHPP model with a loglinear function as intensity)

    # plot the fitted NHPP models
    ct <- Fail_dates_period
    cf <- 1:length(ct)
    timesseq <- seq(1, max(ct), length.out = 100)
    if (plot == TRUE) {
      jpeg(paste0("Outputs/Plot1_", SB, ".jpg"))
      plot(ct, cf, type = "p", xlab = "Time", ylab = "Cumulative recurrences", xlim = c(0, max(ct)))
      lines(timesseq, (timesseq / ppPower$parameters[2])^ppPower$parameters[1], col = "red", lty = 2)
      # lines(timesseq, exp(ppLoglin$parameters[1])*(exp(ppLoglin$parameters[2]*timesseq)-1)/ppLoglin$parameters[2], col="blue", lty=2)
      # lines(timesseq, ppHom$parameters[1]*timesseq, col="orange", lty=2)

      legend("topleft",
        pch = "-", col = c("blue"),
        legend = c("Power-NHPP"), bty = "n", y.intersp = 1.5
      )
      dev.off()
    }
    ## diagnostics

    # function for transforming NHPP-times into HPP-times
    tHPP <- function(object) {
      timesNHPP <- sort(object$times)
      Tstart_date <- 0
      Tend <- min(object$Tend)

      if (object$intensity == "loglinear") {
        gamma0 <- object$parameters[1]
        gamma1 <- object$parameters[2]
        # compute transformed time (homogeneous Poisson process with unit intensity)
        hpp <- (1 / gamma1) * (exp(gamma0 + gamma1 * timesNHPP) - exp(gamma0 + gamma1 * Tstart_date))
      }

      else if (object$intensity == "power") {
        beta <- object$parameters[1]
        eta <- object$parameters[2]
        # compute transformed time (homogeneous Poisson process with unit intensity)
        hpp <- (timesNHPP / eta)^beta - (Tstart_date / eta)^beta
      }

      else if (object$intensity == "homogeneous") {
        lambda <- object$parameters[1]
        # compute transformed time (homogeneous Poisson process with unit intensity)
        hpp <- lambda * timesNHPP - lambda * Tstart_date
      }

      # return values
      list(eventIndex = seq_along(hpp), hppTimes = hpp, nhppTimes = timesNHPP)
    }

    # Use the fitted models to transform the NHPP-times into corresponding
    # Homogeneous Poisson Process (HPP) times,
    # where the HPP is a unit-rate process (i.e., with intensity equal to 1).
    hppPower <- tHPP(ppPower)
    # hppLoglin <- tHPP(ppLoglin)

    # For a unit-rate HPP, the number of cumulative recurrences is expected
    # to be 1 at t=1, the number of cumulative recurrences at t=2 is expected
    # to be 2, et cetera.
    # In other words, when plotting the HPP-times against the cumulative recurrences
    # for a unit-rate HPP, points are expected to lie close to the line y=x.
    if (plot == TRUE) {
      jpeg(paste0("Outputs/Plot2_", SB, ".jpg"))

      plot(hppPower$eventIndex, hppPower$hppTimes,
        type = "l", col = "red",
        xlab = "Cumulative recurrences", ylab = "HPP-time"
      )
      # lines(hppLoglin$eventIndex, hppLoglin$hppTimes, type="l", col="blue")
      abline(a = 0, b = 1, col = "gray", lty = 2) # line y=x
      legend("topleft",
        pch = "-", col = c("red", "blue"),
        legend = c("Power-NHPP", "Loglinear-NHPP"), bty = "n", y.intersp = 1.5
      )

      dev.off()
    }

    # For any NHPP or HPP, the interrecurrence times should be independent.
    if (plot == TRUE) {
      acf(diff(hppPower$hppTimes), main = "Interrecurrence times HPP") # Power-NHPP
      # acf(diff(hppLoglin$hppTimes), main="Interrecurrence times HPP") #Loglinear-NHPP
    }

    ## predicted cumulative recurrences

    # plot again the fitted power-NHPP model, but this time include confidence intervals
    if (plot == TRUE) {
      ts <- seq(1, max(ct), length.out = 100)
      SEs <- sapply(1:length(ts), function(i) {
        car::deltaMethod(ppPower$parameters,
          g = "(ts/eta)^beta",
          vcov. = solve(-1 * ppPower$hessian),
          constants = c(ts = ts[i])
        )
      })
      # 95% confidence intervals for cumulative number of recurrences
      plci <- unlist(SEs[1, ]) * exp((qnorm(.025) * unlist(SEs[2, ])) / unlist(SEs[1, ]))
      puci <- unlist(SEs[1, ]) * exp((qnorm(.975) * unlist(SEs[2, ])) / unlist(SEs[1, ]))

      # plot fit
      plot(ct, cf,
        type = "p", xlab = "Time",
        ylab = "Cumulative recurrences", main = "Power-NHPP"
      )
      lines(timesseq, (timesseq / ppPower$parameters[2])^ppPower$parameters[1],
        col = "red", lty = 2
      )
      lines(x = ts, y = plci, col = "blue", lty = 2)
      lines(x = ts, y = puci, col = "blue", lty = 2)

      # plot the fitted loglinear-NHPP model including confidence intervals
      # ts <- seq(1, max(ct), length.out=100)
      # SEs <- sapply(1:length(ts), function (i) car::deltaMethod(ppLoglin$parameters,
      #                                                   g="(exp(gamma0)*(exp(gamma1*ts)-1))/gamma1",
      #                                                  vcov.=solve(-1*ppLoglin$hessian),
      #                                                 constants=c(ts=ts[i])))

      # 95% confidence intervals for cumulative number of recurrences
      # llci <- unlist(SEs[1,])*exp((qnorm(.025)*unlist(SEs[2,]))/unlist(SEs[1,]))
      # luci <- unlist(SEs[1,])*exp((qnorm(.975)*unlist(SEs[2,]))/unlist(SEs[1,]))

      # plot fit
      # plot(ct, cf, type="p", xlab="Time",
      #    ylab="Cumulative recurrences", main="Loglinear-NHPP")
      #  lines(timesseq, exp(ppLoglin$parameters[1])*(exp(ppLoglin$parameters[2]*timesseq)-1)/ppLoglin$parameters[2],
      #       col="red", lty=2)
      # lines(x=ts, y=llci, col="blue", lty=2)
      # lines(x=ts, y=luci, col="blue", lty=2)
    }

    # compute future number of recurrences (including 95% confidence interval)
    # in the interval [a,b]
    ta <- TI1 # lower limit of interval [a,b]
    tb <- TI2 # upper limit of interval [a,b]

    # power-NHPP
    fnrp <- car::deltaMethod(ppPower$parameters,
      g = "((1/eta)^beta)*(tb^beta-ta^beta)",
      vcov. = solve(-1 * ppPower$hessian), constants = c(ta = ta, tb = tb)
    )
    fnrp$Estimate # point estimate
    fnrp$Estimate * exp(c(1, -1) * ((qnorm(.05 / 2) * fnrp$SE) / fnrp$Estimate)) # confidence interval

    # loglinear-NHPP
    # fnrl <- car::deltaMethod(ppLoglin$parameters,
    #                   g="(exp(gamma0)/gamma1)*(exp(gamma1*tb)-exp(gamma1*ta))",
    #                  vcov.=solve(-1*ppLoglin$hessian), constants=c(ta=ta, tb=tb))
    # fnrl$Estimate #point estimate
    # fnrl$Estimate*exp(c(1,-1)*((qnorm(.05/2)*fnrl$SE)/fnrl$Estimate)) #confidence interval

    # compute likelihood based confidence interval for the future number
    # of recurrences (=FNR) in the interval [a,b]

    # function for computing the log-likelihood
    llikFNR <- function(theta, times, Tend, timeInterval, N, intensity) {
      nobs <- length(times)
      if (intensity == "power") {
        beta <- theta[1]
        eta <- (N / (timeInterval[2]^beta - timeInterval[1]^beta))^-(1 / beta)
        nobs * log(beta) - nobs * beta * log(eta) + (beta - 1) * sum(log(times)) - (Tend / eta)^beta
      }
      else if (intensity == "loglinear") {
        gamma1 <- theta[1]
        gamma0 <- log(gamma1 * (N / (exp(gamma1 * timeInterval[2]) - exp(gamma1 * timeInterval[1]))))
        nobs * gamma0 + gamma1 * sum(times) - (exp(gamma0) * (exp(gamma1 * Tend) - 1)) / gamma1
      }
    }

    # function for profiling FNR (using BFGS)
    profileLogLik <- function(parms, times, Tend, timeInterval, Ns, intensity) {
      plke <- rep(0, length(Ns))

      for (i in 1:length(Ns)) {
        temp <- optim(parms, llikFNR,
          method = "BFGS", control = list(fnscale = -1),
          times = times, Tend = Tend, timeInterval = timeInterval,
          intensity = intensity, N = Ns[i]
        )
        plke[i] <- temp$value
      }
      plke
    }

    # function for computing the confidence interval for FNR
    profileFNR <- function(object, rangeFNR, nvalues = 50, alpha = .95, timeInterval) {
      times <- object$times
      Tend <- object$Tend
      intensity <- object$intensity
      Ns <- seq(rangeFNR[1], rangeFNR[2], length.out = nvalues)

      if (intensity == "power") {
        beta <- object$parameters[1]
        eta <- object$parameters[2]
        parms <- object$parameters[1]
        Np <- ((1 / eta)^beta) * (timeInterval[2]^beta - timeInterval[1]^beta)
      }

      else if (intensity == "loglinear") {
        gamma0 <- object$parameters[1]
        gamma1 <- object$parameters[2]
        parms <- object$parameters[2]
        Np <- (exp(gamma0) / gamma1) * (exp(gamma1 * timeInterval[2]) - exp(gamma1 * timeInterval[1]))
      }

      # profile the future number of recurrences
      ll <- profileLogLik(parms, times, Tend, timeInterval, Ns, intensity)

      # compute confidence bounds
      loglikw <- object$loglik
      limit <- loglikw - .5 * qchisq(alpha, df = 1)

      limitlkh <- function(parms, times, Tend, timeInterval, Ns, intensity) {
        profileLogLik(parms, times, Tend, timeInterval, Ns, intensity) - limit
      }

      lowerci <- NA
      upperci <- NA

      try(lowerci <- round(fzero(limitlkh, c(rangeFNR[1], Np),
        parms = parms,
        times = times, Tend = Tend, timeInterval = timeInterval,
        intensity = intensity
      )$x, 6), TRUE)
      try(upperci <- round(fzero(limitlkh, c(Np, rangeFNR[2]),
        parms = parms,
        times = times, Tend = Tend, timeInterval = timeInterval,
        intensity = intensity
      )$x, 6), TRUE)

      # plot profile
      if (plot == TRUE) {
        jpeg(paste0("Outputs/Plot3_", SB, ".jpg"))
        plot(Ns, ll,
          type = "l",
          xlab = paste("Future number of recurrences in time interval [",
            timeInterval[1], ",", timeInterval[2], "]",
            sep = ""
          ),
          ylab = "log-likelihood"
        )
        abline(h = limit, col = "blue", lty = 2) # log-likelihood limit
        abline(v = Np, col = "red", lty = 2) # include MLE for FNR
        # include interval bounds
        if (!is.na(lowerci)) abline(v = lowerci, col = "darkgreen", lty = 2)
        if (!is.na(upperci)) abline(v = upperci, col = "darkgreen", lty = 2)
        dev.off()
      }

      # return bounds
      data.frame(Estimate = as.vector(Np), lcl = lowerci, ucl = upperci)
    }

    # compute the likelihood based 95% confidence intervals for the future
    # number of recurrences (=FNR) in the time interval [a,b]

    # power-NHPP model
    # note: inspect whether the curve of the log-likelihood function
    # intersects the blue line

    profileFNR(object = ppPower, rangeFNR = c(FNRlow, FNRupp), timeInterval = c(TI1, TI2))

    # on the right, the curve does not intersect with the blue line and ucl=NA
    # therefore, increase the width of the range

    profileFNR(object = ppPower, rangeFNR = c(FNRlow, FNRupp), timeInterval = c(TI1, TI2)) -> temp1

    # loglinear-NHPP model
    # profileFNR(object=ppLoglin, rangeFNR=c(FNRlow, FNRupp), timeInterval=c(TI1, TI2))->temp2

    # add facet graph
    # data.frame(Fail_Day=ct, Cumulative_Reoccurence=cf, Cause=Failure.Cause.Val) -> dataf

    # sum(is.na(dataf$Failure.Cause.Description))->j
    # nrow(dataf)->k

    # if(j/k<1){
    # if(plot==TRUE){
    # jpeg(paste0("Outputs/Plot4_",SB,".jpg"))
    # p <- ggplot(data=dataf, aes(x = Fail_Day, y = Cumulative_Reoccurence, colour = Failure.Cause.Description)) +
    # geom_point(size=1) +
    # labs(title = "Cumulative Failures", x = "Time in Days", y = "Cumulative recurrences", color = "Failure Cause\n") +
    # theme_bw()
    # print(p)
    # dev.off()}
    # }
  } else {
    ppPower <- list()
    temp1 <- list()
    temp1$Estimate <- NA
    temp1$lcl <- NA
    NA -> temp1$ucl
    NA -> ppPower$parameters[1]
    NA -> ppPower$parameters[2]
    NA -> b_lower
    NA -> b_upper
    NA -> eta_lower
    NA -> eta_upper
    NA -> power_AIC
    NA -> power_loglik
    NA -> n_greaterthan2peaks_lessthan45
    peak_timespeakdf <- NULL
    peakdf <- NULL
    peakClassStructure <- NA
  }

  list(
    SOB = c(
      SOB = SB_IDs[SOBNr], Ti1 = TI1, TI2 = TI2, FNR_power = temp1$Estimate, FNRCI1_power = temp1$lcl, FNRCI2_power = temp1$ucl,
      PowerLaw1 = as.numeric(ppPower$parameters[1]), PowerLaw2 = as.numeric(ppPower$parameters[2]),
      PowerLawLC1 = b_lower, PowerLawUC1 = b_upper, PowerLawLC2 = eta_lower,
      PowerLawUC2 = eta_upper, PowerAIC = power_AIC, PowerLL = power_loglik, NSLIDs = NSLID,
      averageLength = mean_length, totalLength = total_length, Avgdiameter = diameter,
      AvgAge = Install.year, NAssets = assetspershutoff, FailCause = mode_Failure.Cause, PipeMaterial = mode_Pipe.Material, meanStaticP = mean.staticPressure, BulkDensity = BD,
      Clay = Clay, DepthSoil = DOS, CatEx = CE, SoilpH = pH, totalP = totP, Silt = Silt, Sand = Sand,
      Most_recent_Construction_Date = max_Install.year, LengthAC = LengthACSOB, LengthCI = LengthCI, Lat = mean_Latitude, Lon = mean_Longitude,
      Numberfails = Nfails, averageMTlast3 = averagetimesincelast3, Nsuccessivefails = n_greaterthan2peaks_lessthan45, WDZ = WDZ,
      Nbursts = Nbursts, Nhydrants = NHydrant, NServiceLine = NSLines, ActualPipeFailVal = pipeFailval, ActualPipeFailTest = pipeFailtest, ActualFailVal = Nfailval, ActualFailTest = Nfailtest, cohortFNR = cohortFNR, maxcohortFNR = MaxcohortFNR,
      cohortInten = cohortIntensity, maxcohortInten = MaxcohortIntensity,
      TWOLength = total_Fail_length, MeanWOLength = mean_Fail_length, CohortEta = cohortEta, CohortBeta = cohortBeta, MaxCohortBeta = MaxcohortBeta, MinCohortEta = MincohortEta, maxStaticP = max.staticPressure, assetSpread = heterogeneity
    ), data = pipeFail,
    timePeaks = peak_timespeakdf, peakDF = peakdf
  ) -> result1


  if (inclsoilmoist == TRUE) {
    result1 <- NULL
    list(SOB = c(
      SOB = SB_IDs[SOBNr], Ti1 = TI1, TI2 = TI2, FNR_power = temp1$Estimate, FNRCI1_power = temp1$lcl, FNRCI2_power = temp1$ucl,
      PowerLaw1 = as.numeric(ppPower$parameters[1]), PowerLaw1 = as.numeric(ppPower$parameters[2]),
      PowerLawLC1 = b_lower, PowerLawUC1 = b_upper, PowerLawLC2 = eta_lower,
      PowerLawUC2 = eta_upper, PowerAIC = power_AIC, PowerLL = power_loglik,
      averageLength = mean_length, totalLength = total_length, Avgdiameter = diameter,
      AvgAge = Install.year, NAssets = assetspershutoff, FailCause = mode_Failure.Cause, PipeMaterial = mode_Pipe.Material, meanZ = mean.DZ, BulkDensity = BD,
      Clay = Clay, DepthSoil = DOS, CatEx = CE, SoilpH = pH, totalP = totP, Silt = Silt, Sand = Sand,
      Most_recent_Construction_Date = max_Install.year, LengthAC = LengthACSOB, LengthCI = LengthCI, Lat = mean_Latitude, Lon = mean_Longitude,
      Numberfails = Nfails, averageMTlast3 = averagetimesincelast3, Nsuccessivefails = n_greaterthan2peaks_lessthan45, WDZ = WDZ,
      Nbursts = Nbursts, Nhydrants = NHydrant, NServiceLine = NSLines, ActualFailVal = Nfailval, ActualFailTest = Nfailtest, maxSoilMoist = MaxSM,
      minSoilMoist = MinSM, rangeSoilMoist = RangeSM,
      TWOLength = total_Fail_length, MeanWOLength = mean_Fail_length
    ), data = pipeFail, timePeaks = peak_timespeakdf, peakDF = peakdf) -> result1
  }

  which(as.Date(seq(start_date, last_date + 82000, by = "day")) %in% as.Date(Fail_Dates[, ])) -> FFF
  length(seq(start_date, last_date, by = "day")) -> L
  diff(c(0, FFF, (L + 1))) -> diffFFF
  diff(c(0, FFF)) -> diffFFF

  cbind(Pipe.Material2, Fail_Dates, Work_Order_Type, Installed.year, diffFFF) -> result2
  as.data.frame(result2) -> result2
  colnames(result2) <- c("PipeMaterial", "FailDates", "WorkType", "Installed_year", "TimeTNE")

  list(parameters = result1, timeseries = result2, clusteredFails = peakClassStructure) -> result

  return(result)

  # check
  # ppPower$parameters[1]
  # ppPower$parameters[2]

  # duane(diffct[1:10])
}

#' Title
#'
#' @param mainOnly
#' @param val_start
#' @param val_end
#' @param outages
#' @param maxNinSOB
#' @param work_Order_Data
#' @param asset_data
#' @param SOB_data
#' @param minNinSOB
#' @param test_start
#' @param test_end
#' @param subDir
#' @param cohortResults
#'
#' @return
#' @export
#'
#' @examples
SOBNHPP_load <- function(mainOnly, val_start, val_end, outages, maxNinSOB, work_Order_Data, asset_data, SOB_data, minNinSOB, test_start, test_end, cohortResults) {

  round(as.numeric(as.Date(val_end) - as.Date(val_start)) / 366, 0) ->> valyears

  work_Order_Data %>% dplyr::arrange(Reported.Date) -> work_Order_Data

  as.data.frame(work_Order_Data) -> work_Order_Data
  work_Order_Data %>% dplyr::filter(Reported.Date > "1995/01/01") -> work_Order_Data

  as.data.frame(asset_data) -> asset_data

  make.names(colnames(work_Order_Data)) -> colnames(work_Order_Data)

  SOB_data %>% dplyr::rename("SOB"=`Shutoff Block`) ->SOB_data
  sort(unique(SOB_data$SOB)) ->> all_SOBs

  print(paste0("the number of SOBS is equal to ", length(all_SOBs)))

  if (outages == TRUE) {
    work_Order_Data %>% dplyr::filter(Number.of.Work.Order.Water.Outages > 0) -> work_Order_Data
  }

  asset_data %>% dplyr::select(Asset.Number, Shutoff.Block) ->tmp
  dplyr::left_join(work_Order_Data, tmp, by = "Asset.Number") -> work_Order_Data

  # Note the order in the coalesce function, we are reassigning SOBs to WOs based on the right joined table,  i.e. the SOB/Asset table)

  coalesce2(work_Order_Data$Shutoff.Block.y, work_Order_Data$Shutoff.Block.x) -> work_Order_Data$Shutoff.Block
  work_Order_Data$Shutoff.Block.x <- NULL
  work_Order_Data$Shutoff.Block.y <- NULL

  work_Order_Data %>%
    dplyr::group_by(Shutoff.Block) %>%
    dplyr::mutate(NWo = dplyr::n()) %>%
    dplyr::ungroup() %>%
    dplyr::select(NWo) %>%
    as.data.frame() -> H
  work_Order_Data$totalNpershutoffblock <- H$NWo

  # example of assetid with no SOB in new table

  work_Order_Data %>% dplyr::filter(Asset.Number == 3175599)
  # example of asset id with more than 1 SOBs  - previously had only counted one of the 3 for Asset 1470291

  work_Order_Data %>% dplyr::filter(Asset.Number == 1470291)

  work_Order_Data %>% dplyr::filter(totalNpershutoffblock < maxNinSOB) -> work_Order_Data

  work_Order_Data %>% dplyr::filter(totalNpershutoffblock > minNinSOB) -> work_Order_Data
  #lubridate::year(as.Date(work_Order_Data$Install.Date)) -> work_Order_Data$Install.year

  work_Order_Data[work_Order_Data$Shutoff.Block==194072,]
  #SOBID 194072  check number of workorders only one coming up???

  # Get Dates
  dplyr::first(work_Order_Data$Reported.Date) ->> start_date
  dplyr::last(work_Order_Data$Reported.Date) ->> last_date

  last_date2 <- as.POSIXlt(last_date)
  as.POSIXlt(as.Date(val_start)) -> val_startdate
  as.POSIXlt(as.Date(val_end)) -> val_enddate

  # last_date2$year <- val_startdate$year  # changed this to Val start date
  # as.Date(last_date2)->>end_date
  as.Date(val_startdate) ->> end_date

  as.numeric(as.Date(last_date) - as.Date(start_date)) ->> NdaysAll # entire data set
  as.numeric(as.Date(end_date) - as.Date(start_date)) ->> Ndays # less validation data
  seq.Date(as.Date(start_date), as.Date(end_date), by = "day") ->> All.Dates # excluding validation
  seq.Date(as.Date(val_start), as.Date(val_end), by = "day") ->> Val.Dates
  seq.Date(as.Date(test_start), as.Date(test_end), by = "day") ->> Test.Dates

  # loadSoilMoisture() -> soilMoisture
  # soilMoisture[rowSums(is.na(soilMoisture[, -c(1:2)])) != ncol(soilMoisture[, -c(1:2)]), ] ->> soilMoisture

  if (mainOnly) {
    work_Order_Data %>%
      dplyr::filter(Class.Structure.1 == "Drinking Water Pipes") %>%
      dplyr::filter(Class.Structure %in% c("Repair Leaking Trunk Service", "Repair Leaking Water Main", "Repair Burst Water Main")) -> work_Order_Data
  }


  # Sys.Date()-1->end_date
  work_Order_Data %>% dplyr::arrange(desc(totalNpershutoffblock)) -> work_Order_Data

  work_Order_Data %>%
    dplyr::group_by(Shutoff.Block) %>%
    dplyr::mutate(Nevents = dplyr::n()) %>%
    as.data.frame() -> work_Order_Data
  work_Order_Data %>% dplyr::filter(Nevents < maxNinSOB) -> work_Order_Data
  work_Order_Data %>% dplyr::filter(Nevents > minNinSOB) -> work_Order_Data

  unique(work_Order_Data$Asset.Number) ->> asset_IDs


  # Add Cohort a and B
  #check columns  this is the NHPP cohort result
  ncol(cohortResults)
  cohortResults[, c(1, 4, 7, 8, 24)] -> cohortResults

  as.character(cohortResults$Cohort) -> cohortResults$Cohort
  as.character(asset_data$I.D) -> asset_data$I.D

  dplyr::left_join(work_Order_Data, cohortResults, by = c("I.D" = "Cohort")) -> work_Order_Data2

  work_Order_Data2 %>%
    dplyr::group_by(Shutoff.Block) %>%
    dplyr::mutate("NinSOB" = dplyr::n()) %>%
    dplyr::arrange(desc(NinSOB)) -> work_Order_Data2
  as.data.frame(work_Order_Data2) -> work_Order_Data2
  as.numeric(work_Order_Data2$Shutoff.Block) -> work_Order_Data2$Shutoff.Block
  work_Order_Data2$NinSOB <- NULL

  unique(work_Order_Data2$Shutoff.Block) ->> SB_IDs

  length(SB_IDs) ->> NSOBS
  work_Order_Data2[!work_Order_Data2$Shutoff.Block %in% "", ] -> work_Order_Data2

  return(work_Order_Data2)
}
