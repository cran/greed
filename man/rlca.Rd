% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tools_generator.R
\name{rlca}
\alias{rlca}
\title{Generate data from lca model}
\usage{
rlca(N, pi, theta)
}
\arguments{
\item{N}{The size of the graph to generate}

\item{pi}{A numeric vector of length K with clusters proportions (will be normalized to sum up to 1).}

\item{theta}{A list of size V}
}
\value{
A list with fields:
\itemize{
\item x: the multi-graph adjacency matrix as an \code{array}
\item K: number of generated clusters
\item N: number of vertex
\item cl: vector of clusters labels
\item pi: clusters proportions
\item theta:
}
}
\description{
\code{rlca} returns a data.frame with factor sampled from an lca model
}
\details{
This function takes the desired graph size, cluster proportions and connectivity matrix as input and sample a graph accordingly together with the clusters labels.
}
\examples{
theta <- list(
  matrix(c(0.1, 0.9, 0.9, 0.1, 0.5, 0.5, 0.3, 0.7), ncol = 2, byrow = TRUE),
  matrix(c(0.5, 0.5, 0.3, 0.7, 0.05, 0.95, 0.3, 0.7), ncol = 2, byrow = TRUE),
  matrix(c(0.5, 0.5, 0.9, 0.1, 0.5, 0.5, 0.1, 0.9), ncol = 2, byrow = TRUE)
)
lca.data <- rlca(100, rep(1 / 4, 4), theta)
}
