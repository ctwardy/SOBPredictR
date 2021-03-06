% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/SOBmodelmain.R
\name{SOBmodelTrain}
\alias{SOBmodelTrain}
\title{SOB Model Train}
\usage{
SOBmodelTrain(
  workorder_data,
  asset_data,
  soil_data,
  SOB_data,
  predictors,
  val_start,
  val_end,
  test_start,
  test_end,
  Nfailcutoff,
  outages = FALSE,
  mainDir,
  modelUpdate,
  rmOutlier,
  forceUpdate,
  forcePreProcess,
  rfe,
  CVfolds,
  Trainsplit
)
}
\arguments{
\item{workorder_data}{dataframe}

\item{asset_data}{dataframe}

\item{soil_data}{dataframe}

\item{SOB_data}{dataframe}

\item{predictors}{character}

\item{val_start}{date}

\item{val_end}{date}

\item{test_start}{date}

\item{test_end}{date}

\item{Nfailcutoff}{integer}

\item{outages}{logical}

\item{mainDir}{character}

\item{savePaths}{character}
}
\value{
returns a list of dataframe outputs from Cohort, SOB and SOB modelling
}
\description{
Main Function to Train a new SOB model
}
\examples{
\dontrun{
SOBmodelTrain <- function(workorder_data, asset_data, soil_data, SOB_data,predictors, val_start,  val_end, test_start, test_end, Nfailcutoff,
outages = FALSE, mainDir, savePaths)
}
}
