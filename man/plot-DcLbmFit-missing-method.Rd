% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/model_dclbm.R
\name{plot,DcLbmFit,missing-method}
\alias{plot,DcLbmFit,missing-method}
\title{Plot a \code{\link{DcLbmFit-class}}}
\usage{
\S4method{plot}{DcLbmFit,missing}(x, type = "blocks")
}
\arguments{
\item{x}{a \code{\link{DcLbmFit-class}}}

\item{type}{a string which specify plot type:
\itemize{
\item \code{'blocks'}: plot a block matrix with summarizing connections between row and column clusters
\item \code{'nodelink'}: plot a nodelink diagram of the bipartite graph summarizing connections between row and column clusters
}}
}
\value{
a \code{\link{ggplot2}} graphic
}
\description{
Plot a \code{\link{DcLbmFit-class}}
}
