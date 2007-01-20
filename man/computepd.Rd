\name{computepd}
\alias{computepd}
\title{Internal functions used to compute prediction discrepancies}
\description{
  Functions used by \code{npde} and \code{autonpde} to compute prediction
 discrepancies (not intended for the end-user).
  }
\usage{
computepd(tabobs, tabsim, calc.npde)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{tabobs}{observed data, a dataframe containing 3 columns (id=patient ID,
  xobs=independent variable (X), yobs=dependent variable (Y)}
  \item{tabsim}{simulated data, a dataframe containing 4 columns (idsim=patient
  ID, irsim=integer identifying the replicate; xsim=independent variable (X),
  ysim=dependent variable (Y)}
  \item{calc.npde}{a boolean (T if npde are computed, F otherwise); used to
  decide whether the function should compute ypred}
}
\details{
  These functions are normally not called by the end-user.
}
\value{
  \item{pd}{the vector of prediction discrepancies}
  \item{ypred}{a vector giving the mean of the predicted distribution for each
observation, if calc.npde=F (NA otherwise)}
}
\references{K. Brendel, E. Comets, C. Laffont, C. Laveille, and F. Mentré.
Metrics for external model evaluation with an application to the population
pharmacokinetics of gliclazide. \emph{Pharmaceutical Research}, 23:2036--49,
2006.}
\author{Emmanuelle Comets <emmanuelle.comets@bichat.inserm.fr>}

\seealso{\code{\link{npde}}, \code{\link{autonpde}}}
\examples{
}
\keyword{internal}
