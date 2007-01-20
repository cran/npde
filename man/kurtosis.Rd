\name{kurtosis}
\alias{kurtosis}
\title{Kurtosis}
\description{
  Computes the kurtosis.
}
\usage{
kurtosis(x)
}
\arguments{
  \item{x}{a numeric vector containing the values whose kurtosis is to
    be computed. NA values are removed in the computation.}
}
\details{
  If \eqn{N = \mathrm{length}(x)}{N = length(x)}, then the kurtosis of
  \eqn{x} is defined as:
\deqn{N sum_i (x_i-\mathrm{mean}(x))^4 (sum_i (x_i-\mathrm{mean}(x))^2)^(-2) -
3}{N sum_i (x_i-mean(x))^4 (sum_i (x_i-mean(x))^2)^(-2) - 3}
}
\value{
  The kurtosis of \code{x}.
}
\examples{
x <- rnorm(100)
kurtosis(x)
}
\keyword{univar}


