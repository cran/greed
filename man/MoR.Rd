% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/model_mor.R
\docType{class}
\name{MoR}
\alias{MoR}
\alias{MoRPrior-class}
\alias{MoR-class}
\alias{MoRPrior}
\title{Multivariate mixture of regression Prior model description class}
\usage{
MoRPrior(formula, tau = 0.001, N0 = NaN, epsilon = as.matrix(NaN))

MoR(formula, alpha = 1, tau = 0.1, N0 = NaN, epsilon = as.matrix(NaN))
}
\arguments{
\item{formula}{a \code{\link{formula}} that describe the linear model to use}

\item{tau}{Prior parameter (inverse variance) default 0.001}

\item{N0}{Prior parameter (default to NaN, in this case N0 will be fixed equal to the number of columns of Y.)}

\item{epsilon}{Covariance matrix prior parameter (default to NaN, in this case epsilon will be fixed to a diagonal variance matrix equal to 0.1 time the variance of the regression residuals with only one cluster.)}

\item{alpha}{Dirichlet prior parameter over the cluster proportions (default to 1)}
}
\value{
a \code{MoRPrior-class} object

a \code{MoR-class} object
}
\description{
An S4 class to represent a multivariate mixture of regression model.
The model follows [minka-linear](https://tminka.github.io/papers/minka-linear.pdf) .
The model corresponds to the following generative model:
\deqn{ \pi \sim Dirichlet(\alpha)}
\deqn{ Z_i  \sim \mathcal{M}(1,\pi)}
\deqn{ V_k \sim \mathcal{W}(\varepsilon^{-1},n_0)}
\deqn{ A_k \sim \mathcal{MN}(0,(V_k)^{-1},\tau XX^\top)}
\deqn{ Y_{i.}|X_{i.}, A_k, Z_{ik}=1 \sim \mathcal{N}(A_k x_{i.},V_{k}^{-1})}
with \eqn{\mathcal{W}(\epsilon^{-1},n_0)} the Wishart distribution and \eqn{\mathcal{MN}} the matrix-normal distribution.
The \code{MoR-class} must be used when fitting a simple Mixture of Regression whereas the \code{MoRPrior-class} must be used when fitting a \code{\link{CombinedModels-class}}.
}
\examples{
MoRPrior(y ~ x1 + x2)
MoRPrior(y ~ x1 + x2, N0 = 100)
MoRPrior(cbind(y1, y2) ~ x1 + x2, N0 = 100)
MoR(y ~ x1 + x2)
MoR(y ~ x1 + x2, N0 = 100)
MoR(cbind(y1, y2) ~ x1 + x2, N0 = 100)
}
\seealso{
\code{\link{MoRFit-class}}, \code{\link{MoRPath-class}}

Other DlvmModels: 
\code{\link{CombinedModels}},
\code{\link{DcLbm}},
\code{\link{DcSbm}},
\code{\link{DiagGmm}},
\code{\link{DlvmPrior-class}},
\code{\link{Gmm}},
\code{\link{Lca}},
\code{\link{MoM}},
\code{\link{MultSbm}},
\code{\link{Sbm}},
\code{\link{greed}()}
}
\concept{DlvmModels}
