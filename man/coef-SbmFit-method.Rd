% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/model_sbm.R
\name{coef,SbmFit-method}
\alias{coef,SbmFit-method}
\title{Extract parameters from an \code{\link{SbmFit-class}} object}
\usage{
\S4method{coef}{SbmFit}(object)
}
\arguments{
\item{object}{a \code{\link{SbmFit-class}}}
}
\value{
a list with the model parameters estimates (MAP), the fields are:
\itemize{
\item \code{'pi'}: cluster proportions
\item \code{'thetakl'}: between clusters connections probabilities (matrix of size K x K)
}
}
\description{
Extract parameters from an \code{\link{SbmFit-class}} object
}
