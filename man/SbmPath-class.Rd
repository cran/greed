% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/model_sbm.R
\docType{class}
\name{SbmPath-class}
\alias{SbmPath-class}
\title{Stochastic Block Model hierarchical fit results class}
\description{
An S4 class to represent a hierarchical fit of a stochastic block model, extend \code{\link{IclPath-class}}.
}
\section{Slots}{

\describe{
\item{\code{model}}{a \code{\link{Sbm-class}} object to store the model fitted}

\item{\code{name}}{generative model name}

\item{\code{icl}}{icl value of the fitted model}

\item{\code{K}}{number of extracted clusters over row and columns}

\item{\code{cl}}{a numeric vector with row and columns cluster indexes}

\item{\code{obs_stats}}{a list with the following elements:
\itemize{
\item counts: numeric vector of size K with number of elements in each clusters
\item x_counts: matrix of size K*K with the number of links between each pair of clusters
}}

\item{\code{path}}{a list of size K-1 with that store all the solutions along the path. Each element is a list with the following fields:
\itemize{
\item icl1: icl value reach with this solution for alpha=1
\item logalpha: log(alpha) value were this solution is better than its parent
\item K: number of clusters
\item cl: vector of cluster indexes
\item k,l: index of the cluster that were merged at this step
\item merge_mat: lower triangular matrix of delta icl values
\item obs_stats: a list with the following elements:
\itemize{
\item counts: numeric vector of size K with number of elements in each clusters
\item x_counts: matrix of size K*K with the number of links between each pair of clusters
}
}}

\item{\code{logalpha}}{value of log(alpha)}

\item{\code{ggtree}}{data.frame with complete merge tree for easy plotting with \code{ggplot2}}

\item{\code{tree}}{numeric vector with merge tree \code{tree[i]} contains the index of \code{i} father}

\item{\code{train_hist}}{data.frame with training history information (details depends on the training procedure)}
}}

\seealso{
\code{\link{plot,SbmFit,missing-method}}
}
