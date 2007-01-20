\name{calcnpde}
\alias{calcnpde}
\title{Compute the prediction distribution errors in one subject}
\description{
  For each subject, this function computes the prediction errors and
  returns the decorrelated simulated and observed data.
}
\usage{
calcnpde(isuj, msuj, matsim, nrep, verbose)
}
\arguments{
  \item{isuj}{subject ID}
  \item{msuj}{observed data for the subject, a dataframe containing 3 columns
  (id=patient ID, xobs=independent variable (X), yobs=dependent variable (Y)}
  \item{matsim}{simulated data for the subject, a dataframe containing 4 columns
  (idsim=patient ID, irsim=integer identifying the replicate; xsim=independent
  variable (X),
  ysim=dependent variable (Y)}
  \item{nrep}{number of replications}
  \item{verbose}{a boolean (T if messages are to be printed as each subject is
  processed, F otherwise)}
}
\value{
  \item{xerr}{an integer code to keep track of errors during the computation;
  after a successful computation the value of \code{xerr} should be 0. A value
  of 1 or 2 signals errors during the computation.}
  \item{pde}{the vector of prediction distribution errors for the subject}
  \item{ydsim}{the vector of simulated dependent variable (Y) for the subject,
decorrelated}
  \item{ydobs}{the vector of observed dependent variable (Y) for the subject,
decorrelated}
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
