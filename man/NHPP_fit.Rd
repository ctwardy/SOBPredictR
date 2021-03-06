% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/NHPPSOB.R
\name{NHPP_fit}
\alias{NHPP_fit}
\title{NHPP_fit}
\usage{
NHPP_fit(
  minNinSOB,
  asset_data,
  soil_data,
  work_Order_Data,
  SOBNr,
  TI1,
  TI2,
  FNRupp,
  FNRlow,
  plot,
  minpkh,
  rollingwin,
  inclsoilmoist
)
}
\arguments{
\item{minNinSOB}{integer}

\item{asset_data}{dataframe}

\item{soil_data}{dataframe}

\item{work_Order_Data}{dataframe}

\item{SOBNr}{integer}

\item{TI1}{integer}

\item{TI2}{integer}

\item{FNRupp}{integer}

\item{FNRlow}{integer}

\item{plot}{logical}

\item{minpkh}{integer}

\item{rollingwin}{integer}

\item{inclsoilmoist}{logical}
}
\value{
dataframe
}
\description{
Fits Non Homogeneous Poisson Model to Shut Off Blocks
}
\examples{
\dontrun{
NHPP_fit <- function(minNinSOB, asset_data, soil_data, work_Order_Data, SOBNr, TI1, TI2, FNRupp, FNRlow, plot, minpkh, rollingwin, inclsoilmoist)
}
}
