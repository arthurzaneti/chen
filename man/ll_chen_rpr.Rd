% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ll_chen_rpr.R
\name{ll_chen_rpr}
\alias{ll_chen_rpr}
\title{Log-likelihood function for the reparameterized Chen distribution}
\usage{
ll_chen_rpr(y, theta, tau)
}
\arguments{
\item{y}{A numeric vector with the random sample}

\item{theta}{A length 2 or n+1 vector or list with \lambda and \mu respectively.
The values will be coersed to vector using \code{as.vector(unlist(theta))} and
lambda will always be considered as the first element in that vector, if the length is two than all values will
be generated with the same \mu, if the length is n+1 than one value will be generated with each \mu.}

\item{tau}{The quantile}
}
\value{
The output of the log-likelihood formula, that is, the likelihood of the given set
of parameters given the output variables.
}
\description{
Mathematical formula given by \deqn{
\ell(\theta | y) = \log(\log(1 - \tau)) - \log(1 - \exp(\mu^{\lambda})) +
(\lambda - 1) \log(y_t) + \log(\lambda) + \frac{\log(1 - \tau)(1 - \exp(y_t^{\lambda}))}
{1 - \exp(\mu^{\lambda})} + y_t^{\lambda}
}
Where \eqn{\mu} and \eqn{\lambda} are parameters, \eqn{\tau} is quantile
and \eqn{y} is the random sample.
}
