% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tools_misc.R
\name{MI}
\alias{MI}
\title{Compute the mutual information of two discrete samples}
\usage{
MI(cl1, cl2)
}
\arguments{
\item{cl1}{vector of discrete labels}

\item{cl2}{vector of discrete labels}
}
\value{
the mutual information between the two discrete samples
}
\description{
Compute the mutual information of two discrete samples
}
\examples{
cl1 <- sample(2, 500, replace = TRUE)
cl2 <- sample(2, 500, replace = TRUE)
MI(cl1, cl2)
}
