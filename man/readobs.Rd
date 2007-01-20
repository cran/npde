\name{readobs}
\alias{readobs}
\alias{readsim}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Functions to read the data }
\description{
These functions read the data from which npde are to be computed (not intended
for the end-user).
}
\usage{
readobs(namobs, iid, ix, iy, imdv)
readsim(namsim, datobs, mdv)
}
\arguments{
  \item{namobs}{name of the file containing the observed data}
  \item{namsim}{name of the file containing the simulated data}
  \item{iid}{number of the column containing the patient ID}
  \item{ix}{number of the column containing the independent variable (X)}
  \item{iy}{number of the column containing the dependent variable (Y)}
  \item{imdv}{number of the column containing information about missing data
  (MDV); if this column does not exist, imdv is 0 and missing data in Y are
  assumed to be coded as NA or a dot "." in the file}
  \item{datobs}{Observed data, a dataframe containing 3 columns (id=patient ID,
  xobs=independent variable (X), yobs=dependent variable (Y)}
  \item{mdv}{a vector of 0 and 1; lines containing 1 correspond to missing
  observations and are removed from the dataset before the computation of pde}
}
\details{
These functions are private functions used by \code{npde} and \code{autonpde}.
They are not intended to be used elsewhere.
}
\value{
  \item{tabobs}{observed data, a dataframe containing 3 columns (id=patient ID,
  xobs=independent variable (X), yobs=dependent variable (Y)}
  \item{mdv}{a vector of 0 and 1; lines containing 1 correspond to missing
  observations and are removed from the dataset before the computation of pde}
  \item{tabsim}{simulated data, a dataframe containing 4 columns (idsim=patient
  ID, irsim=integer identifying the replicate; xsim=independent variable (X),
  ysim=dependent variable (Y)}
}
\references{K. Brendel, E. Comets, C. Laffont, C. Laveille, and F. Mentré.
Metrics for external model evaluation with an application to the population
pharmacokinetics of gliclazide. \emph{Pharmaceutical Research}, 23:2036--49,
2006.}
\author{Emmanuelle Comets <emmanuelle.comets@bichat.inserm.fr>}

\seealso{\code{\link{npde}}, \code{\link{autonpde}}}
%\examples{
%}
\keyword{internal}
