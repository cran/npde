\name{pdemenu}
\alias{pdemenu}
\title{Interactive query of the arguments needed for the computation of
normalised prediction distribution errors}
\description{
  This function can be used to set interactively the arguments needed to compute
  the normalised prediction distribution errors. It is called by \code{npde}.
}
\usage{
pdemenu()
}
\details{
  This function is normally not called by the end-user.
}
\value{
  \item{namobs}{name of the file containing the observed data}
  \item{namsim}{name of the file containing the simulated data}
  \item{iid}{number of the column containing the patient ID}
  \item{ix}{number of the column containing the independent variable (X)}
  \item{iy}{number of the column containing the dependent variable (Y)}
  \item{imdv}{number of the column containing information about missing data
  (MDV), 0 if this column is not present}
  \item{boolsave}{a boolean (T if results are to be saved to a file, F if not)}
  \item{type.graph}{type of graph (one of "eps","jpeg","png","pdf"}
  \item{namegr}{name of the file in which graphs are to be saved}
  \item{calc.npde}{a boolean (T if npde are to be computed, F otherwise)}
  \item{calc.npd}{a boolean (T if npd are to be computed, F otherwise)}
  \item{nameres}{name of the file in which numerical results are to be saved}
  \item{output}{a boolean (T if the function should return values, F otherwise)}
  \item{verbose}{a boolean (T if messages are to be printed as each subject is
  processed, F otherwise)}
}
\references{K. Brendel, E. Comets, C. Laffont, C. Laveille, and F. Mentré.
Metrics for external model evaluation with an application to the population
pharmacokinetics of gliclazide. \emph{Pharmaceutical Research}, 23:2036--49,
2006.}
\author{Emmanuelle Comets <emmanuelle.comets@bichat.inserm.fr>}

\seealso{\code{\link{npde}}, \code{\link{autonpde}}}
\examples{
## Not run (needs interactive input)
# x<-pdemenu()
}
\keyword{models}
