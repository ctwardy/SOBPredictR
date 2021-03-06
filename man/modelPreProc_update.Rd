% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/H20.R
\name{modelPreProc_update}
\alias{modelPreProc_update}
\title{Model PreProcess Update}
\usage{
modelPreProc_update(
  combined.df,
  predictors,
  classification,
  Nfailcutoff,
  outlierRemove,
  trained_rec
)
}
\arguments{
\item{combined.df}{dataframe}

\item{predictors}{list}

\item{classification}{logical}

\item{Nfailcutoff}{integer}

\item{outlierRemove}{logical}

\item{trained_rec}{character}
}
\value{
list of dataframes which are used as inputs to Machine Learning Train
}
\description{
PreProcess new data for input to SOB model
}
\examples{
\dontrun{
modelPreProc_update<- function(combined.df, predictors, classification, Nfailcutoff, outlierRemove, trained_rec)
}
}
