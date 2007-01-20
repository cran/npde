\name{computenpde}
\alias{computenpde}
\title{Internal functions used to compute normalised prediction distribution errors}
\description{
  Functions used by \code{npde} and \code{autonpde} to compute normalised
  prediction distribution errors (not intended for the end-user).
  }
\usage{
computenpde(tabobs, tabsim, verbose)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{tabobs}{observed data, a dataframe containing 3 columns (id=patient ID,
  xobs=independent variable (X), yobs=dependent variable (Y)}
  \item{tabsim}{simulated data, a dataframe containing 4 columns (idsim=patient
  ID, irsim=integer identifying the replicate; xsim=independent variable (X),
  ysim=dependent variable (Y)}
  \item{verbose}{a boolean (T if messages are to be printed as each subject is
  processed, F otherwise)}
}
\details{
  These functions are normally not called by the end-user.
}
\value{
  \item{ydsim}{the vector of simulated dependent variable (Y), decorrelated}
  \item{ydobs}{the vector of observed dependent variable (Y), decorrelated}
  \item{xerr}{an integer code to keep track of errors during the computation;
  after a successful computation the value of \code{xerr} should be 0. A value
  of 1 or 2 signals errors during the computation.}
  \item{npde}{the vector of normalised prediction distribution errors}
  \item{ypred}{a vector giving the mean of the predicted distribution for each
observation}
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
