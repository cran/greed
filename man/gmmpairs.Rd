% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tools_ploting.R
\name{gmmpairs}
\alias{gmmpairs}
\title{Make a matrix of plots with a given data and gmm fitted parameters}
\usage{
gmmpairs(sol, X)
}
\arguments{
\item{sol}{a \code{\link{GmmFit-class}} or \code{\link{DiagGmmFit-class}}}

\item{X}{the data used for the fit a data.frame or matrix.}
}
\value{
a \code{\link{ggplot2}} graphic
}
\description{
Make a matrix of plots with a given data and gmm fitted parameters with ellipses.
}
