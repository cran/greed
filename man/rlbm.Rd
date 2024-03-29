% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tools_generator.R
\name{rlbm}
\alias{rlbm}
\title{Generate a data matrix using a Latent Block Model}
\usage{
rlbm(Nr, Nc, pir, pic, mu)
}
\arguments{
\item{Nr}{desired Number of rows}

\item{Nc}{desired Number of column}

\item{pir}{A numeric vector of length Kr with rows clusters proportions (will be normalized to sum up to 1).}

\item{pic}{A numeric vector of length Kc with columns clusters proportions (will be normalized to sum up to 1).}

\item{mu}{A numeric matrix of dim Kr x Kc with the connectivity pattern to generate. elements in [0,1].}
}
\value{
A list with fields:
\itemize{
\item x: the generated data matrix as a \code{dgCMatrix}
\item clr: vector of row clusters labels
\item clc: vector of column clusters labels
\item Kr: number of generated row clusters
\item Kc: number of generated column clusters
\item Nr: number of rows
\item Nc: number of column
\item pir: row clusters proportions
\item pic: column clusters proportions
\item mu: connectivity matrix
}
}
\description{
\code{rlbm} returns the adjacency matrix and the cluster labels generated randomly with a Latent Block Model.
}
\details{
This function takes the desired graph size, cluster proportions and connectivity matrix as input and sample a graph accordingly together with the clusters labels.
}
\examples{
simu <- rlbm(500, 1000, rep(1 / 5, 5), rep(1 / 10, 10), matrix(runif(50), 5, 10))
}
