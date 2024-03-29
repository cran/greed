% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/model_dclbm.R
\name{plot,DcLbmPath,missing-method}
\alias{plot,DcLbmPath,missing-method}
\title{Plot a \code{\link{DcLbmPath-class}}}
\usage{
\S4method{plot}{DcLbmPath,missing}(x, type = "tree")
}
\arguments{
\item{x}{a \code{\link{DcLbmPath-class}}}

\item{type}{a string which specify plot type:
\itemize{
\item \code{'tree'}: plot a co-dendogram of rows and columns clusters
\item \code{'blocks'}: plot a block matrix with summarizing connections between row and column clusters
\item \code{'biplot'}: plot a block matrix with summarizing connections between row and column clusters aligned with row and clusters drendograms
\item \code{'nodelink'}: plot a nodelink diagram of the bipartite graph summarizing connections between row and column clusters
}}
}
\value{
a \code{\link{ggplot2}} graphic
}
\description{
Plot a \code{\link{DcLbmPath-class}}
}
