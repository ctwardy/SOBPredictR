% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/NHPPSOB.R
\name{SOBNHPP_load}
\alias{SOBNHPP_load}
\title{SOBNHPP_load}
\usage{
SOBNHPP_load(
  mainOnly,
  val_start,
  val_end,
  outages,
  maxNinSOB,
  work_Order_Data,
  asset_data,
  SOB_data,
  minNinSOB,
  test_start,
  test_end,
  cohortResults
)
}
\arguments{
\item{mainOnly}{logical}

\item{val_start}{date}

\item{val_end}{date}

\item{outages}{logical}

\item{maxNinSOB}{integer}

\item{work_Order_Data}{dataframe}

\item{asset_data}{datafrane}

\item{SOB_data}{datafrane}

\item{minNinSOB}{integer}

\item{test_start}{date}

\item{test_end}{date}

\item{cohortResults}{datafrane}
}
\value{
dataframe for inputting to NHPP fit
}
\description{
Loads the data in preparation for the NHPP fitting step
}
\examples{
\dontrun{
SOBNHPP_load <- function(mainOnly, val_start, val_end, outages, maxNinSOB, work_Order_Data, asset_data, SOB_data, minNinSOB, test_start, test_end, cohortResults)
}
}
