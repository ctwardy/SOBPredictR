#' NHPP_loadDatacohort
#'
#'Analagous function to SOB, applied to pipe Cohorts
#' @param work_order datafrane
#' @param val_start date
#' @param val_end date
#' @param outages logical
#' @param maxN integer
#' @param minN integer
#' @param test_start date
#' @param test_end date
#'
#' @return datafrane for NHPP Cohort Fitting
#'
#' @examples \dontrun{
#' NHPP_loadDatacohort <- function(work_order, val_start, val_end, outages, maxN, minN, test_start, test_end)
#' }
NHPP_loadDatacohort <- function(work_order, val_start, val_end, outages, maxN, minN, test_start, test_end) {

  # questions/additions,  change point analysis,
  # deviation from NHPP fit,  error at each time, > 2 rolling average

  round(as.numeric(as.Date(val_end) - as.Date(val_start)) / 366, 0) ->> valyears

  work_order %>% dplyr::arrange((Reported.Date)) -> work_order

  as.data.frame(work_order) -> work_order
  work_order %>% dplyr::filter(Reported.Date > as.Date("1995/01/01")) -> work_order

  make.names(colnames(work_order)) -> colnames(work_order)

  # work_order[!is.na(work_order$Pipe.Material),]->work_order
  work_order[!is.na(work_order$I.D), ] -> work_order
  # work_order[!work_order$Pipe.Material=="",]->work_order

  print(nrow(work_order))
  if (outages == TRUE) {
    work_order %>% dplyr::filter(Number.of.Work.Order.Water.Outages > 0) -> work_order
  }

  work_order %>% dplyr::filter(NFails < maxN) -> work_order
  work_order %>% dplyr::filter(NFails > minN) -> work_order
  work_order$Install.Date -> work_order$Install.year

  # Get Dates
  dplyr::first(work_order$Reported.Date) ->> start_date

  dplyr::last(work_order$Reported.Date) ->> last_date

  last_date2 <- as.POSIXlt(last_date)
  as.POSIXlt(as.Date(val_start)) -> val_startdate
  as.POSIXlt(as.Date(val_end)) -> val_enddate

  as.Date(last_date2) ->> end_date

  as.numeric(as.Date(last_date) - as.Date(start_date)) ->> NdaysAll # entire data set

  as.numeric(as.Date(end_date) - as.Date(start_date)) ->> Ndays # less validation data

  seq.Date(as.Date(start_date), as.Date(end_date), by = "day") ->> All.Dates # excluding validation

  seq.Date(as.Date(val_start), as.Date(val_end), by = "day") ->> Val.Dates

  seq.Date(as.Date(test_start), as.Date(test_end), by = "day") ->> Test.Dates

  # Sys.Date()-1->end_date
  work_order %>% dplyr::arrange(dplyr::desc(NfailperNasset)) -> work_order

  unique(work_order$I.D) ->> cohort_IDs

  length(cohort_IDs) ->> Ncohorts

  return(work_order)
}


#' NHPP_fit_cohort
#'
#' Analagous function to SOB NHPP fit.  Note this function is not called directly but via the modelTrain function
#'
#' @param asset_data dataframe
#' @param work_Order_Data dataframe
#' @param cohortNr integer
#' @param TI1 integer
#' @param TI2 integer
#' @param FNRupp numeric
#' @param FNRlow numeric
#' @param plot logical
#' @param minpkh numeric
#' @param rollingwin integer
#' @param inclsoilmoist logical
#'
#' @return dataframe of NHPP fitted parameters for all pipe Cohorts.  This is required for the NHPP
#'
#' @examples \dontrun{
#' NHPP_fit_cohort <- function(asset_data, work_Order_Data, cohortNr, TI1, TI2, FNRupp, FNRlow, plot, minpkh, rollingwin, inclsoilmoist)
#' }
NHPP_fit_cohort <- function(asset_data, work_Order_Data, cohortNr, TI1, TI2, FNRupp, FNRlow, plot, minpkh, rollingwin, inclsoilmoist) {
  result <- list(c())
  cohort_IDs[cohortNr] -> CohortID
  print(CohortID)

  work_Order_Data %>%
    dplyr::filter(I.D %in% CohortID) %>%
    dplyr::select(Reported.Date) %>%
    dplyr::distinct(as.Date(Reported.Date)) -> Fail_Dates
  nrow(Fail_Dates) -> Nfails

  work_Order_Data %>%
    dplyr::filter(I.D %in% CohortID) %>%
    dplyr::distinct(as.Date(Reported.Date), .keep_all = TRUE) %>%
    dplyr::select(Class.Structure) -> Work_Order_Type

  work_Order_Data %>%
    dplyr::filter(I.D %in% CohortID) %>%
    dplyr::select(Asset.Number, Length) %>%
    dplyr::distinct(Asset.Number, .keep_all = TRUE) -> Asset_with_WO

  asset_data %>%
    dplyr::filter(I.D %in% CohortID) %>%
    dplyr::select(Asset.Number, Length) %>%
    dplyr::distinct(Asset.Number, .keep_all = TRUE) %>%
    dplyr::summarize(FUN = mean(Length[!Length == 0], na.rm = TRUE)) -> mean_length
  asset_data %>%
    dplyr::filter(I.D %in% CohortID) %>%
    dplyr::select(Asset.Number, Length) %>%
    dplyr::distinct(Asset.Number, .keep_all = TRUE) %>%
    dplyr::summarize(FUN = sum(Length[!Length == 0], na.rm = TRUE)) -> total_length
  asset_data %>%
    dplyr::filter(I.D %in% CohortID) %>%
    dplyr::select(Asset.Number, Nominal.Pipe.Size..mm.) %>%
    dplyr::distinct(Asset.Number, .keep_all = TRUE) %>%
    dplyr::summarize(FUN = mean(Nominal.Pipe.Size..mm.[!Nominal.Pipe.Size..mm. == 0], na.rm = TRUE)) -> diameter

  asset_data %>%
    dplyr::filter(I.D %in% CohortID) %>%
    dplyr::select(Static_Pressure_Final) %>%
    dplyr::summarize(FUN = mean(Static_Pressure_Final, na.rm = TRUE)) -> mean.DZ
  asset_data %>%
    dplyr::filter(I.D %in% CohortID) %>%
    dplyr::summarise(Nassets = dplyr::n_distinct(Asset.Number)) -> assetspercohort

  work_Order_Data %>%
    dplyr::filter(I.D %in% CohortID) %>%
    dplyr::select(Water.Distribution.Zone) -> WDZ

  # work_Order_Data %>% filter(ID %in% CohortID) %>% dplyr::select(N.slids.affected)->NSLID
  # sum(NSLID[,])->NSLID

  # Added two lines to remove dates preceding pipe installation

  length(All.Dates) -> L1
  work_Order_Data %>%
    dplyr::filter(I.D %in% CohortID) %>%
    dplyr::select(Install.year) %>%
    dplyr::distinct() %>%
    as.numeric() -> Year
  All.Dates[!lubridate::year(All.Dates) < Year] -> All.Dates
  length(All.Dates) -> L2
  (L1 - L2) -> D
  print(D)
  (TI1 - D) -> TI1
  (TI2 - D) -> TI2
  print(TI1)

  # NHPP
  which(as.Date(All.Dates) %in% as.Date(Fail_Dates[, ])) -> Fail_dates_period
  array(0, dim = c(1, length(All.Dates))) -> px
  px[Fail_dates_period] <- 1
  d <- dplyr::first(Fail_dates_period)

  # seq.Date(as.Date(start_date), as.Date(end_date), by="day")->Dates  #replace with following to reduce date period
  seq.Date(as.Date(All.Dates[[1]]), as.Date(end_date), by = "day") -> Dates

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
    dplyr::mutate(cumsum = cumsum(status)) -> pipeFailval
  pipeFailval %>% dplyr::mutate(cum_rolling = zoo::rollapplyr(status, width = rollingwin, FUN = sum, partial = TRUE)) -> pipeFailval
  Nfailval <- utils::tail(pipeFailval$cumsum, 1)

  # new code, repeat above for the test period, this is separate to validation for ANN but should follow,  i.e. if want to test 12 months, in 2016, set validation to 3 years
  # if want to test 2017, validation =2, 2018, validation = 1,

  data.frame(times = cumsum(c(1, as.numeric(diff(Test.Dates))))) -> dft
  dft %>%
    dplyr::mutate(status = ifelse(Test.Dates %in% as.Date(Fail_Dates[, ]), 1, 0)) %>%
    dplyr::mutate(cumsum = cumsum(status)) -> pipeFailtest
  pipeFailtest %>% dplyr::mutate(cum_rolling = zoo::rollapplyr(status, width = rollingwin, FUN = sum, partial = TRUE)) -> pipeFailtest
  Nfailtest <- utils::tail(pipeFailtest$cumsum, 1)

  # Step 1 Plot Cumulative Count,  if slope is not constant implies a NHPP minimal repair/deteriorating or improving system
  if (plot == TRUE) {
    plot(Dates, pipeFail$cumsum)
    plot(pipeFail$times, pipeFail$status)
    plot(pipeFail$times, pipeFail$cum_rolling, type = "l")
    plot(Dates, pipeFail$cum_rolling, type = "l")
  }

  pracma::findpeaks(pipeFail$cum_rolling, zero = "+", minpeakheight = minpkh, minpeakdistance = 50) -> peakdf
  peakdf[, 2] -> peakDays
  peakDays + as.Date(start_date) -> peakDates
  sort(peakDates) -> peakDates2

  peakDateList <- list()
  if (length(peakDates2) > 0) {
    for (k in 1:length(peakDates2)) {
      which(abs(as.numeric((as.Date(Fail_Dates[, ]) - peakDates2[[k]]))) == min(abs(as.numeric(as.Date(Fail_Dates[, ]) - peakDates2[[k]])))) -> f
      as.Date(Fail_Dates[f, ]) -> peakDateList[[k]]
    }
    peakDates3 <- do.call("c", peakDateList)

    work_Order_Data %>%
      dplyr::filter(I.D %in% CohortID) %>%
      dplyr::filter(as.Date(Reported.Date) %in% peakDates) %>%
      dplyr::select(Class.Structure) -> peakClassStructure
  } else {
    peakClassStructure <- NA
  }


  if (!is.null(peakdf)) {
    if (nrow(peakdf) > 1) {
      nrow(peakdf) - 1 -> h
      stats::kmeans(peakdf[, 2:4], h) -> clusterpeak
      clusterpeak$centers[, 1] -> peak_timespeakdf
    } else {
      peak_timespeakdf <- "NA"
    }
  } else {
    peak_timespeakdf <- "NA"
  }

  max(pipeFail$times) -> end_time

  # plot of time versus cumulative recurrences
  ct <- Fail_dates_period
  cf <- 1:length(ct)

  # added code to compute the time to next failure
  # pad cts with 0 and end time, left is truncated right is censored.
  diff(c(0, ct, (end_time + 1))) -> diffct

  my_seq <- function(x) {
    seq(x, 1, -1)
  }
  lapply(diffct, FUN = my_seq) -> temp
  unlist(temp) -> timetofailure

  cbind(pipeFail, utils::head(timetofailure, -1)) -> pipeFail
  names(pipeFail)[5] <- "timetofailure"

  if (plot == TRUE) {
    plot(ct, cf, type = "p", xlab = "Time", ylab = "Cumulative recurrences")
  }
  if (plot == TRUE) {
    plot(pipeFail$times, pipeFail$timetofailure, type = "l", xlab = "Time", ylab = "Time to Failure")
  }

  with(rle(diffct < rollingwin), sum(lengths[values] >= minpkh)) -> n_greaterthan2peaks_lessthan45

  # non-parametric estimate of the intensity
  start_dateT <- 0 # lower bound of the observation window
  endT <- max(ct) # upper bound of the observation window
  sigma <- 150 # bandwidth for Gaussian kernel
  densityEstimate <- stats::density(ct, from = start_dateT, to = endT, n = 500, bw = sigma) # Gaussian kernel
  timesD <- densityEstimate$x # extract times
  nRecurrences <- length(ct) # total number of recurrences
  intensityEstimate <- densityEstimate$y * nRecurrences # estimate of the intensity
  # implement edge correction
  edgeCorrection <- stats::pnorm(endT, timesD, sigma) - stats::pnorm(start_dateT, timesD, sigma)
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
      pow <- stats::optim(c(beta, eta), llp,
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
      gamma1 <- pracma::fzero(rootgamma1, x = 1e-3, nobs = nobs, times = times, Tend = Tend)$x
      gamma0 <- log((nobs * gamma1) / (exp(Tend * gamma1) - 1))
      # log-likelihood of the loglinear-NHPP
      lll <- function(theta, times, Tend) {
        gamma0 <- theta[1]
        gamma1 <- theta[2]
        nobs * gamma0 + gamma1 * sum(times) - (exp(gamma0) * (exp(gamma1 * Tend) - 1)) / gamma1
      }
      # refit the loglinear-NHPP by optimization of the log-likelihood function
      loglin <- stats::optim(c(gamma0, gamma1), lll,
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
      hom <- stats::optim(lambda, lll,
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
  ppPower$parameters[1] * exp(c(1, -1) * stats::qnorm(.05 / 2) * SEparms[1] / ppPower$parameters[1])[1] -> b_lower
  ppPower$parameters[1] * exp(c(1, -1) * stats::qnorm(.05 / 2) * SEparms[1] / ppPower$parameters[1])[2] -> b_upper

  ppPower$parameters[2] * exp(c(1, -1) * stats::qnorm(.05 / 2) * SEparms[2] / ppPower$parameters[2])[1] -> eta_lower
  ppPower$parameters[2] * exp(c(1, -1) * stats::qnorm(.05 / 2) * SEparms[2] / ppPower$parameters[2])[2] -> eta_upper

  ppPower$AIC -> power_AIC
  ppPower$loglik -> power_loglik

  # fit loglinear-NHPP (that is, NHPP model with a loglinear function as intensity)
  InclogLin <- FALSE
  if (InclogLin == TRUE) {
    try(ppLoglin <- fitNHPP(
      times = ct[-length(ct)], Tend = ct[length(ct)],
      intensity = "loglinear"
    ))[1:4]

    # 95% confidence intervals for gamma0 and gamma1 (normal-approximation)
    SEparms <- sqrt(diag(solve(-1 * ppLoglin$hessian)))
    ppLoglin$parameters[1] + c(1, -1) * stats::qnorm(.05 / 2) * SEparms[1][1] -> gamma1_lower
    ppLoglin$parameters[1] + c(1, -1) * stats::qnorm(.05 / 2) * SEparms[1][2] -> gamma1_upper


    ppLoglin$parameters[2] + c(1, -1) * stats::qnorm(.05 / 2) * SEparms[2][1] -> gamma2_lower
    ppLoglin$parameters[2] + c(1, -1) * stats::qnorm(.05 / 2) * SEparms[2][2] -> gamma2_upper

    ppLoglin$AIC -> Loglin_AIC
    ppLoglin$loglik -> Loglin_loglik
  }

  # fit loglinear-NHPP (that is, NHPP model with a loglinear function as intensity)
  # (ppHom <- fitNHPP(times=ct[-length(ct)], Tend=ct[length(ct)],
  #                     intensity="homogeneous"))[1:4]
  ## 95% confidence intervals for gamma0 and gamma1 (normal-approximation)
  # SEparms <- sqrt(diag(solve(-1*ppHom$hessian)))
  # ppHom$parameters[1] + c(1,-1)*stats::qnorm(.05/2)*SEparms[1]

  # plot the fitted NHPP models
  ct <- Fail_dates_period
  cf <- 1:length(ct)
  timesseq <- seq(1, max(ct), length.out = 100)
  if (plot == TRUE) {
    grDevices::jpeg(paste0("Outputs/Plot1_", CohortID, ".jpg"))
    plot(ct, cf, type = "p", xlab = "Time", ylab = "Cumulative recurrences", xlim = c(0, max(ct)))
    graphics::lines(timesseq, (timesseq / ppPower$parameters[2])^ppPower$parameters[1], col = "red", lty = 2)
    # graphics::lines(timesseq, exp(ppLoglin$parameters[1])*(exp(ppLoglin$parameters[2]*timesseq)-1)/ppLoglin$parameters[2], col="blue", lty=2)
    # graphics::lines(timesseq, ppHom$parameters[1]*timesseq, col="orange", lty=2)
    graphics::legend("topleft",
      pch = "-", col = c("blue"),
      legend = c("Power-NHPP"), bty = "n", y.intersp = 1.5
    )
    grDevices::dev.off()
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
    grDevices::jpeg(paste0("Outputs/Plot2_", CohortID, ".jpg"))

    plot(hppPower$eventIndex, hppPower$hppTimes,
      type = "l", col = "red",
      xlab = "Cumulative recurrences", ylab = "HPP-time"
    )
    graphics::lines(hppLoglin$eventIndex, hppLoglin$hppTimes, type = "l", col = "blue")
    graphics::abline(a = 0, b = 1, col = "gray", lty = 2) # line y=x
    graphics::legend("topleft",
      pch = "-", col = c("red", "blue"),
      legend = c("Power-NHPP", "Loglinear-NHPP"), bty = "n", y.intersp = 1.5
    )

    grDevices::dev.off()
  }

  # For any NHPP or HPP, the interrecurrence times should be independent.
  if (plot == TRUE) {
    stats::acf(diff(hppPower$hppTimes), main = "Interrecurrence times HPP") # Power-NHPP
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
    plci <- unlist(SEs[1, ]) * exp((stats::qnorm(.025) * unlist(SEs[2, ])) / unlist(SEs[1, ]))
    puci <- unlist(SEs[1, ]) * exp((stats::qnorm(.975) * unlist(SEs[2, ])) / unlist(SEs[1, ]))

    # plot fit
    plot(ct, cf,
      type = "p", xlab = "Time",
      ylab = "Cumulative recurrences", main = "Power-NHPP"
    )
    graphics::lines(timesseq, (timesseq / ppPower$parameters[2])^ppPower$parameters[1],
      col = "red", lty = 2
    )
    graphics::lines(x = ts, y = plci, col = "blue", lty = 2)
    graphics::lines(x = ts, y = puci, col = "blue", lty = 2)

    # plot the fitted loglinear-NHPP model including confidence intervals
    # ts <- seq(1, max(ct), length.out=100)
    # SEs <- sapply(1:length(ts), function (i) car::deltaMethod(ppLoglin$parameters,
    #                                                   g="(exp(gamma0)*(exp(gamma1*ts)-1))/gamma1",
    #                                                  vcov.=solve(-1*ppLoglin$hessian),
    #                                                 constants=c(ts=ts[i])))

    # 95% confidence intervals for cumulative number of recurrences
    # llci <- unlist(SEs[1,])*exp((stats::qnorm(.025)*unlist(SEs[2,]))/unlist(SEs[1,]))
    # luci <- unlist(SEs[1,])*exp((stats::qnorm(.975)*unlist(SEs[2,]))/unlist(SEs[1,]))

    # plot fit
    # plot(ct, cf, type="p", xlab="Time",
    #    ylab="Cumulative recurrences", main="Loglinear-NHPP")
    #  graphics::lines(timesseq, exp(ppLoglin$parameters[1])*(exp(ppLoglin$parameters[2]*timesseq)-1)/ppLoglin$parameters[2],
    #       col="red", lty=2)
    # graphics::lines(x=ts, y=llci, col="blue", lty=2)
    # graphics::lines(x=ts, y=luci, col="blue", lty=2)
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
  fnrp$Estimate * exp(c(1, -1) * ((stats::qnorm(.05 / 2) * fnrp$SE) / fnrp$Estimate)) # confidence interval

  # loglinear-NHPP
  # fnrl <- car::deltaMethod(ppLoglin$parameters,
  #                   g="(exp(gamma0)/gamma1)*(exp(gamma1*tb)-exp(gamma1*ta))",
  #                  vcov.=solve(-1*ppLoglin$hessian), constants=c(ta=ta, tb=tb))
  # fnrl$Estimate #point estimate
  # fnrl$Estimate*exp(c(1,-1)*((stats::qnorm(.05/2)*fnrl$SE)/fnrl$Estimate)) #confidence interval

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
      temp <- stats::optim(parms, llikFNR,
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
    limit <- loglikw - .5 * stats::qchisq(alpha, df = 1)

    limitlkh <- function(parms, times, Tend, timeInterval, Ns, intensity) {
      profileLogLik(parms, times, Tend, timeInterval, Ns, intensity) - limit
    }

    lowerci <- NA
    upperci <- NA

    try(lowerci <- round(pracma::fzero(limitlkh, c(rangeFNR[1], Np),
      parms = parms,
      times = times, Tend = Tend, timeInterval = timeInterval,
      intensity = intensity
    )$x, 6), TRUE)
    try(upperci <- round(pracma::fzero(limitlkh, c(Np, rangeFNR[2]),
      parms = parms,
      times = times, Tend = Tend, timeInterval = timeInterval,
      intensity = intensity
    )$x, 6), TRUE)

    # plot profile
    if (plot == TRUE) {
      grDevices::jpeg(paste0("Outputs/Plot3_", CohortID, ".jpg"))
      plot(Ns, ll,
        type = "l",
        xlab = paste("Future number of recurrences in time interval [",
          timeInterval[1], ",", timeInterval[2], "]",
          sep = ""
        ),
        ylab = "log-likelihood"
      )
      graphics::abline(h = limit, col = "blue", lty = 2) # log-likelihood limit
      graphics::abline(v = Np, col = "red", lty = 2) # include MLE for FNR
      # include interval bounds
      if (!is.na(lowerci)) graphics::abline(v = lowerci, col = "darkgreen", lty = 2)
      if (!is.na(upperci)) graphics::abline(v = upperci, col = "darkgreen", lty = 2)
      grDevices::dev.off()
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
  # grDevices::jpeg(paste0("Outputs/Plot4_",CohortID,".jpg"))
  # p <- ggplot(data=dataf, aes(x = Fail_Day, y = Cumulative_Reoccurence, colour = Failure.Cause.Description)) +
  # geom_point(size=1) +
  # labs(title = "Cumulative Failures", x = "Time in Days", y = "Cumulative recurrences", color = "Failure Cause\n") +
  # theme_bw()
  # print(p)
  # grDevices::dev.off()}
  # }

  InclogLin <- FALSE
  # return parameter lists
  if (InclogLin == TRUE) {
    list(SOB = c(
      SOB = SB_IDs[SOBNr], Ti1 = TI1, TI2 = TI2, FNR_power = temp1$Estimate, FNRCI1_power = temp1$lcl, FNRCI2_power = temp1$ucl,
      FNR_loglin = temp2$Estimate, FNRCI1_loglin = temp2$lcl, FNRCI2_loglin = temp2$ucl,
      PowerLaw1 = ppPower$parameters[1], PowerLaw1 = ppPower$parameters[2],
      PowerLawLC1 = b_lower, PowerLawUC1 = b_upper, PowerLawLC2 = eta_lower,
      PowerLawUC2 = eta_upper, PowerAIC = power_AIC, PowerLL = power_loglik,
      LogLin1 = ppLoglin$parameters[1], LogLin2 = ppLoglin$parameters[2],
      LogLinLC1 = gamma1_lower, LogLinUC1 = gamma1_upper, LogLinLC2 = gamma2_lower, LogLinUC2 = gamma2_upper,
      LogLinAIC = Loglin_AIC, LogLinLL = Loglin_loglik,
      averageLength = mean_length, totalLength = total_length, Avgdiameter = diameter,
      AvgAge = Install.year, FailCause = mode_Failure.Cause,
      NAssets = assetspershutoff, PipeMaterial = mode_Pipe.Material,
      Most_recent_Construction_Date = max_Install.year,
      Numberfails = Nfails, Nsuccessivefails = n_greaterthan2peaks_lessthan45, ActualFail = Nfailval
    ), data = pipeFail, timePeaks = peak_timespeakdf, peakDF = peakdf) -> result1
  }

  list(
    cohort = c(
      Cohort = cohort_IDs[cohortNr], Ti1 = TI1, TI2 = TI2, FNR_power = temp1$Estimate, FNRCI1_power = temp1$lcl, FNRCI2_power = temp1$ucl,
      PowerLaw1 = ppPower$parameters[1], PowerLaw1 = ppPower$parameters[2],
      PowerLawLC1 = b_lower, PowerLawUC1 = b_upper, PowerLawLC2 = eta_lower,
      PowerLawUC2 = eta_upper, PowerAIC = power_AIC, PowerLL = power_loglik,
      averageLength = mean_length, totalLength = total_length, Avgdiameter = diameter,
      NAssets = assetspercohort, meanZ = mean.DZ,
      Numberfails = Nfails, Nsuccessivefails = n_greaterthan2peaks_lessthan45,
      ActualFailVal = Nfailval, ActualFailTest = Nfailtest
    ), data = pipeFail,
    timePeaks = peak_timespeakdf, peakDF = peakdf, FailType = Work_Order_Type, AssetList = Asset_with_WO, zones = WDZ
  ) -> result1

  which(as.Date(seq(start_date, last_date + 82000, by = "day")) %in% as.Date(Fail_Dates[, ])) -> FFF
  length(seq(start_date, last_date, by = "day")) -> L
  diff(c(0, FFF, (L + 1))) -> diffFFF
  diff(c(0, FFF)) -> diffFFF

  cbind(Fail_Dates, Work_Order_Type, diffFFF) -> result2
  as.data.frame(result2) -> result2
  colnames(result2) <- c("FailDates", "WorkType", "TimeTNE")

  list(parameters = result1, timeseries = result2, clusteredFails = peakClassStructure) -> result
  return(result)
}

#' NHPP_fit_cohort_update
#'
#' Similar function as NHPP fit cohort.  Update used when for making a year ahead prediction.  Not called directly
#'
#' @param asset_data dataframe
#' @param work_Order_Data dataframe
#' @param cohortNr integer
#' @param TI1 integer
#' @param TI2 integer
#' @param FNRupp integer
#' @param FNRlow integer
#' @param plot logical
#' @param minpkh integer
#' @param rollingwin integer
#' @param inclsoilmoist logical
#'
#' @return dataframe of fitted parameters for pipe cohorts
#' @export
#'
#' @examples \dontrun{
#' NHPP_fit_cohort_update <- function(asset_data, work_Order_Data, cohortNr, TI1, TI2, FNRupp, FNRlow, plot, minpkh, rollingwin, inclsoilmoist)
#' }
NHPP_fit_cohort_update <- function(asset_data, work_Order_Data, cohortNr, TI1, TI2, FNRupp, FNRlow, plot, minpkh, rollingwin, inclsoilmoist) {
  result <- list(c())
  cohort_IDs[cohortNr] -> CohortID
  print(CohortID)

  work_Order_Data %>%
    dplyr::filter(I.D %in% CohortID) %>%
    dplyr::select(Reported.Date) %>%
    dplyr::distinct(as.Date(Reported.Date)) -> Fail_Dates
  nrow(Fail_Dates) -> Nfails

  work_Order_Data %>%
    dplyr::filter(I.D %in% CohortID) %>%
    dplyr::distinct(as.Date(Reported.Date), .keep_all = TRUE) %>%
    dplyr::select(Class.Structure) -> Work_Order_Type

  work_Order_Data %>%
    dplyr::filter(I.D %in% CohortID) %>%
    dplyr::select(Asset.Number, Length) %>%
    dplyr::distinct(Asset.Number, .keep_all = TRUE) -> Asset_with_WO

  asset_data %>%
    dplyr::filter(I.D %in% CohortID) %>%
    dplyr::select(Asset.Number, Length) %>%
    dplyr::distinct(Asset.Number, .keep_all = TRUE) %>%
    dplyr::summarize(FUN = mean(Length[!Length == 0], na.rm = TRUE)) -> mean_length
  asset_data %>%
    dplyr::filter(I.D %in% CohortID) %>%
    dplyr::select(Asset.Number, Length) %>%
    dplyr::distinct(Asset.Number, .keep_all = TRUE) %>%
    dplyr::summarize(FUN = sum(Length[!Length == 0], na.rm = TRUE)) -> total_length
  asset_data %>%
    dplyr::filter(I.D %in% CohortID) %>%
    dplyr::select(Asset.Number, Nominal.Pipe.Size..mm.) %>%
    dplyr::distinct(Asset.Number, .keep_all = TRUE) %>%
    dplyr::summarize(FUN = mean(Nominal.Pipe.Size..mm.[!Nominal.Pipe.Size..mm. == 0], na.rm = TRUE)) -> diameter

  asset_data %>%
    dplyr::filter(I.D %in% CohortID) %>%
    dplyr::select(Static_Pressure_Final) %>%
    dplyr::summarize(FUN = mean(Static_Pressure_Final, na.rm = TRUE)) -> mean.DZ
  asset_data %>%
    dplyr::filter(I.D %in% CohortID) %>%
    dplyr::summarise(Nassets = dplyr::n_distinct(Asset.Number)) -> assetspercohort

  work_Order_Data %>%
    dplyr::filter(I.D %in% CohortID) %>%
    dplyr::select(Water.Distribution.Zone) -> WDZ

  # work_Order_Data %>% filter(ID %in% CohortID) %>% dplyr::select(N.slids.affected)->NSLID
  # sum(NSLID[,])->NSLID

  # Added two lines to remove dates preceding pipe installation

  length(All.Dates) -> L1
  work_Order_Data %>%
    dplyr::filter(I.D %in% CohortID) %>%
    dplyr::select(Install.year) %>%
    dplyr::distinct() %>%
    as.numeric() -> Year
  All.Dates[!lubridate::year(All.Dates) < Year] -> All.Dates
  length(All.Dates) -> L2
  (L1 - L2) -> D
  print(D)
  (TI1 - D) -> TI1
  (TI2 - D) -> TI2
  print(TI1)

  # NHPP
  which(as.Date(All.Dates) %in% as.Date(Fail_Dates[, ])) -> Fail_dates_period
  array(0, dim = c(1, length(All.Dates))) -> px
  px[Fail_dates_period] <- 1
  d <- dplyr::first(Fail_dates_period)

  # seq.Date(as.Date(start_date), as.Date(end_date), by="day")->Dates  #replace with following to reduce date period
  seq.Date(as.Date(All.Dates[[1]]), as.Date(end_date), by = "day") -> Dates

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
    dplyr::mutate(cumsum = cumsum(status)) -> pipeFailval
  pipeFailval %>% dplyr::mutate(cum_rolling = zoo::rollapplyr(status, width = rollingwin, FUN = sum, partial = TRUE)) -> pipeFailval
  Nfailval <- utils::tail(pipeFailval$cumsum, 1)

  # new code, repeat above for the test period, this is separate to validation for ANN but should follow,  i.e. if want to test 12 months, in 2016, set validation to 3 years
  # if want to test 2017, validation =2, 2018, validation = 1,

  data.frame(times = cumsum(c(1, as.numeric(diff(Test.Dates))))) -> dft
  dft %>%
    dplyr::mutate(status = ifelse(Test.Dates %in% as.Date(Fail_Dates[, ]), 1, 0)) %>%
    dplyr::mutate(cumsum = cumsum(status)) -> pipeFailtest
  pipeFailtest %>% dplyr::mutate(cum_rolling = zoo::rollapplyr(status, width = rollingwin, FUN = sum, partial = TRUE)) -> pipeFailtest
  Nfailtest <- utils::tail(pipeFailtest$cumsum, 1)
  print(Nfailtest)

  # Step 1 Plot Cumulative Count,  if slope is not constant implies a NHPP minimal repair/deteriorating or improving system
  if (plot == TRUE) {
    plot(Dates, pipeFail$cumsum)
    plot(pipeFail$times, pipeFail$status)
    plot(pipeFail$times, pipeFail$cum_rolling, type = "l")
    plot(Dates, pipeFail$cum_rolling, type = "l")
  }

  pracma::findpeaks(pipeFail$cum_rolling, zero = "+", minpeakheight = minpkh, minpeakdistance = 50) -> peakdf
  peakdf[, 2] -> peakDays
  peakDays + as.Date(start_date) -> peakDates
  sort(peakDates) -> peakDates2

  peakDateList <- list()
  if (length(peakDates2) > 0) {
    for (k in 1:length(peakDates2)) {
      which(abs(as.numeric((as.Date(Fail_Dates[, ]) - peakDates2[[k]]))) == min(abs(as.numeric(as.Date(Fail_Dates[, ]) - peakDates2[[k]])))) -> f
      as.Date(Fail_Dates[f, ]) -> peakDateList[[k]]
    }
    peakDates3 <- do.call("c", peakDateList)

    work_Order_Data %>%
      dplyr::filter(I.D %in% CohortID) %>%
      dplyr::filter(as.Date(Reported.Date) %in% peakDates) %>%
      dplyr::select(Class.Structure) -> peakClassStructure
  } else {
    peakClassStructure <- NA
  }


  if (!is.null(peakdf)) {
    if (nrow(peakdf) > 1) {
      nrow(peakdf) - 1 -> h
      stats::kmeans(peakdf[, 2:4], h) -> clusterpeak
      clusterpeak$centers[, 1] -> peak_timespeakdf
    } else {
      peak_timespeakdf <- "NA"
    }
  } else {
    peak_timespeakdf <- "NA"
  }

  max(pipeFail$times) -> end_time

  # plot of time versus cumulative recurrences
  ct <- Fail_dates_period
  cf <- 1:length(ct)

  # added code to compute the time to next failure
  # pad cts with 0 and end time, left is truncated right is censored.
  diff(c(0, ct, (end_time + 1))) -> diffct

  my_seq <- function(x) {
    seq(x, 1, -1)
  }
  lapply(diffct, FUN = my_seq) -> temp
  unlist(temp) -> timetofailure

  cbind(pipeFail, utils::head(timetofailure, -1)) -> pipeFail
  names(pipeFail)[5] <- "timetofailure"

  if (plot == TRUE) {
    plot(ct, cf, type = "p", xlab = "Time", ylab = "Cumulative recurrences")
  }
  if (plot == TRUE) {
    plot(pipeFail$times, pipeFail$timetofailure, type = "l", xlab = "Time", ylab = "Time to Failure")
  }

  with(rle(diffct < rollingwin), sum(lengths[values] >= minpkh)) -> n_greaterthan2peaks_lessthan45

  # non-parametric estimate of the intensity
  start_dateT <- 0 # lower bound of the observation window
  endT <- max(ct) # upper bound of the observation window
  sigma <- 150 # bandwidth for Gaussian kernel
  densityEstimate <- stats::density(ct, from = start_dateT, to = endT, n = 500, bw = sigma) # Gaussian kernel
  timesD <- densityEstimate$x # extract times
  nRecurrences <- length(ct) # total number of recurrences
  intensityEstimate <- densityEstimate$y * nRecurrences # estimate of the intensity
  # implement edge correction
  edgeCorrection <- stats::pnorm(endT, timesD, sigma) - stats::pnorm(start_dateT, timesD, sigma)
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
      pow <- stats::optim(c(beta, eta), llp,
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
      gamma1 <- pracma::fzero(rootgamma1, x = 1e-3, nobs = nobs, times = times, Tend = Tend)$x
      gamma0 <- log((nobs * gamma1) / (exp(Tend * gamma1) - 1))
      # log-likelihood of the loglinear-NHPP
      lll <- function(theta, times, Tend) {
        gamma0 <- theta[1]
        gamma1 <- theta[2]
        nobs * gamma0 + gamma1 * sum(times) - (exp(gamma0) * (exp(gamma1 * Tend) - 1)) / gamma1
      }
      # refit the loglinear-NHPP by optimization of the log-likelihood function
      loglin <- stats::optim(c(gamma0, gamma1), lll,
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
      hom <- stats::optim(lambda, lll,
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
  ppPower$parameters[1] * exp(c(1, -1) * stats::qnorm(.05 / 2) * SEparms[1] / ppPower$parameters[1])[1] -> b_lower
  ppPower$parameters[1] * exp(c(1, -1) * stats::qnorm(.05 / 2) * SEparms[1] / ppPower$parameters[1])[2] -> b_upper

  ppPower$parameters[2] * exp(c(1, -1) * stats::qnorm(.05 / 2) * SEparms[2] / ppPower$parameters[2])[1] -> eta_lower
  ppPower$parameters[2] * exp(c(1, -1) * stats::qnorm(.05 / 2) * SEparms[2] / ppPower$parameters[2])[2] -> eta_upper

  ppPower$AIC -> power_AIC
  ppPower$loglik -> power_loglik

  # fit loglinear-NHPP (that is, NHPP model with a loglinear function as intensity)
  InclogLin <- FALSE
  if (InclogLin == TRUE) {
    try(ppLoglin <- fitNHPP(
      times = ct[-length(ct)], Tend = ct[length(ct)],
      intensity = "loglinear"
    ))[1:4]

    # 95% confidence intervals for gamma0 and gamma1 (normal-approximation)
    SEparms <- sqrt(diag(solve(-1 * ppLoglin$hessian)))
    ppLoglin$parameters[1] + c(1, -1) * stats::qnorm(.05 / 2) * SEparms[1][1] -> gamma1_lower
    ppLoglin$parameters[1] + c(1, -1) * stats::qnorm(.05 / 2) * SEparms[1][2] -> gamma1_upper


    ppLoglin$parameters[2] + c(1, -1) * stats::qnorm(.05 / 2) * SEparms[2][1] -> gamma2_lower
    ppLoglin$parameters[2] + c(1, -1) * stats::qnorm(.05 / 2) * SEparms[2][2] -> gamma2_upper

    ppLoglin$AIC -> Loglin_AIC
    ppLoglin$loglik -> Loglin_loglik
  }

  # fit loglinear-NHPP (that is, NHPP model with a loglinear function as intensity)
  # (ppHom <- fitNHPP(times=ct[-length(ct)], Tend=ct[length(ct)],
  #                     intensity="homogeneous"))[1:4]
  ## 95% confidence intervals for gamma0 and gamma1 (normal-approximation)
  # SEparms <- sqrt(diag(solve(-1*ppHom$hessian)))
  # ppHom$parameters[1] + c(1,-1)*stats::qnorm(.05/2)*SEparms[1]

  # plot the fitted NHPP models
  ct <- Fail_dates_period
  cf <- 1:length(ct)
  timesseq <- seq(1, max(ct), length.out = 100)
  if (plot == TRUE) {
    grDevices::jpeg(paste0("Outputs/Plot1_", CohortID, ".jpg"))
    plot(ct, cf, type = "p", xlab = "Time", ylab = "Cumulative recurrences", xlim = c(0, max(ct)))
    graphics::lines(timesseq, (timesseq / ppPower$parameters[2])^ppPower$parameters[1], col = "red", lty = 2)
    # graphics::lines(timesseq, exp(ppLoglin$parameters[1])*(exp(ppLoglin$parameters[2]*timesseq)-1)/ppLoglin$parameters[2], col="blue", lty=2)
    # graphics::lines(timesseq, ppHom$parameters[1]*timesseq, col="orange", lty=2)
    graphics::legend("topleft",
      pch = "-", col = c("blue"),
      legend = c("Power-NHPP"), bty = "n", y.intersp = 1.5
    )
    grDevices::dev.off()
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
    grDevices::jpeg(paste0("Outputs/Plot2_", CohortID, ".jpg"))

    plot(hppPower$eventIndex, hppPower$hppTimes,
      type = "l", col = "red",
      xlab = "Cumulative recurrences", ylab = "HPP-time"
    )
    graphics::lines(hppLoglin$eventIndex, hppLoglin$hppTimes, type = "l", col = "blue")
    graphics::abline(a = 0, b = 1, col = "gray", lty = 2) # line y=x
    graphics::legend("topleft",
      pch = "-", col = c("red", "blue"),
      legend = c("Power-NHPP", "Loglinear-NHPP"), bty = "n", y.intersp = 1.5
    )

    grDevices::dev.off()
  }

  # For any NHPP or HPP, the interrecurrence times should be independent.
  if (plot == TRUE) {
    stats::acf(diff(hppPower$hppTimes), main = "Interrecurrence times HPP") # Power-NHPP
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
    plci <- unlist(SEs[1, ]) * exp((stats::qnorm(.025) * unlist(SEs[2, ])) / unlist(SEs[1, ]))
    puci <- unlist(SEs[1, ]) * exp((stats::qnorm(.975) * unlist(SEs[2, ])) / unlist(SEs[1, ]))

    # plot fit
    plot(ct, cf,
      type = "p", xlab = "Time",
      ylab = "Cumulative recurrences", main = "Power-NHPP"
    )
    graphics::lines(timesseq, (timesseq / ppPower$parameters[2])^ppPower$parameters[1],
      col = "red", lty = 2
    )
    graphics::lines(x = ts, y = plci, col = "blue", lty = 2)
    graphics::lines(x = ts, y = puci, col = "blue", lty = 2)

    # plot the fitted loglinear-NHPP model including confidence intervals
    # ts <- seq(1, max(ct), length.out=100)
    # SEs <- sapply(1:length(ts), function (i) car::deltaMethod(ppLoglin$parameters,
    #                                                   g="(exp(gamma0)*(exp(gamma1*ts)-1))/gamma1",
    #                                                  vcov.=solve(-1*ppLoglin$hessian),
    #                                                 constants=c(ts=ts[i])))

    # 95% confidence intervals for cumulative number of recurrences
    # llci <- unlist(SEs[1,])*exp((stats::qnorm(.025)*unlist(SEs[2,]))/unlist(SEs[1,]))
    # luci <- unlist(SEs[1,])*exp((stats::qnorm(.975)*unlist(SEs[2,]))/unlist(SEs[1,]))

    # plot fit
    # plot(ct, cf, type="p", xlab="Time",
    #    ylab="Cumulative recurrences", main="Loglinear-NHPP")
    #  graphics::lines(timesseq, exp(ppLoglin$parameters[1])*(exp(ppLoglin$parameters[2]*timesseq)-1)/ppLoglin$parameters[2],
    #       col="red", lty=2)
    # graphics::lines(x=ts, y=llci, col="blue", lty=2)
    # graphics::lines(x=ts, y=luci, col="blue", lty=2)
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
  fnrp$Estimate * exp(c(1, -1) * ((stats::qnorm(.05 / 2) * fnrp$SE) / fnrp$Estimate)) # confidence interval

  # loglinear-NHPP
  # fnrl <- car::deltaMethod(ppLoglin$parameters,
  #                   g="(exp(gamma0)/gamma1)*(exp(gamma1*tb)-exp(gamma1*ta))",
  #                  vcov.=solve(-1*ppLoglin$hessian), constants=c(ta=ta, tb=tb))
  # fnrl$Estimate #point estimate
  # fnrl$Estimate*exp(c(1,-1)*((stats::qnorm(.05/2)*fnrl$SE)/fnrl$Estimate)) #confidence interval

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
      temp <- stats::optim(parms, llikFNR,
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
    limit <- loglikw - .5 * stats::qchisq(alpha, df = 1)

    limitlkh <- function(parms, times, Tend, timeInterval, Ns, intensity) {
      profileLogLik(parms, times, Tend, timeInterval, Ns, intensity) - limit
    }

    lowerci <- NA
    upperci <- NA

    try(lowerci <- round(pracma::fzero(limitlkh, c(rangeFNR[1], Np),
      parms = parms,
      times = times, Tend = Tend, timeInterval = timeInterval,
      intensity = intensity
    )$x, 6), TRUE)
    try(upperci <- round(pracma::fzero(limitlkh, c(Np, rangeFNR[2]),
      parms = parms,
      times = times, Tend = Tend, timeInterval = timeInterval,
      intensity = intensity
    )$x, 6), TRUE)

    # plot profile
    if (plot == TRUE) {
      grDevices::jpeg(paste0("Outputs/Plot3_", CohortID, ".jpg"))
      plot(Ns, ll,
        type = "l",
        xlab = paste("Future number of recurrences in time interval [",
          timeInterval[1], ",", timeInterval[2], "]",
          sep = ""
        ),
        ylab = "log-likelihood"
      )
      graphics::abline(h = limit, col = "blue", lty = 2) # log-likelihood limit
      graphics::abline(v = Np, col = "red", lty = 2) # include MLE for FNR
      # include interval bounds
      if (!is.na(lowerci)) graphics::abline(v = lowerci, col = "darkgreen", lty = 2)
      if (!is.na(upperci)) graphics::abline(v = upperci, col = "darkgreen", lty = 2)
      grDevices::dev.off()
    }

    # return bounds
    data.frame(Estimate = as.vector(Np), lcl = lowerci, ucl = upperci)
  }

  # compute the likelihood based 95% confidence intervals for the future
  # number of recurrences (=FNR) in the time interval [a,b]

  # power-NHPP model
  # note: inspect whether the curve of the log-likelihood function
  # intersects the blue line

  profileFNR(object = ppPower, rangeFNR = c(FNRlow, FNRupp), timeInterval = c(TI1, TI1 + 365))

  # on the right, the curve does not intersect with the blue line and ucl=NA
  # therefore, increase the width of the range

  profileFNR(object = ppPower, rangeFNR = c(FNRlow, FNRupp), timeInterval = c(TI1, TI1 + 365)) -> temp1

  list(
    cohort = c(
      Cohort = cohort_IDs[cohortNr], Ti1 = TI1, TI2 = TI2, FNR_power = temp1$Estimate, FNRCI1_power = temp1$lcl, FNRCI2_power = temp1$ucl,
      PowerLaw1 = ppPower$parameters[1], PowerLaw1 = ppPower$parameters[2],
      PowerLawLC1 = b_lower, PowerLawUC1 = b_upper, PowerLawLC2 = eta_lower,
      PowerLawUC2 = eta_upper, PowerAIC = power_AIC, PowerLL = power_loglik,
      averageLength = mean_length, totalLength = total_length, Avgdiameter = diameter,
      NAssets = assetspercohort, meanZ = mean.DZ,
      Numberfails = Nfails, Nsuccessivefails = n_greaterthan2peaks_lessthan45,
      ActualFailVal = Nfailval, ActualFailTest = Nfailtest
    ), data = pipeFail,
    timePeaks = peak_timespeakdf, peakDF = peakdf, FailType = Work_Order_Type, AssetList = Asset_with_WO, zones = WDZ
  ) -> result1

  which(as.Date(seq(start_date, last_date + 82000, by = "day")) %in% as.Date(Fail_Dates[, ])) -> FFF
  length(seq(start_date, last_date, by = "day")) -> L
  diff(c(0, FFF, (L + 1))) -> diffFFF
  diff(c(0, FFF)) -> diffFFF

  cbind(Fail_Dates, Work_Order_Type, diffFFF) -> result2
  as.data.frame(result2) -> result2
  colnames(result2) <- c("FailDates", "WorkType", "TimeTNE")

  list(parameters = result1, timeseries = result2, clusteredFails = peakClassStructure) -> result
  return(result)
}
