\name{graphnpde}
\alias{graphnpde}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Save graphs of the normalised prediction distribution errors to a file}
\description{
  Saves the graphs plotted by \code{plotnpde} to a file.
}
\usage{
graphnpde(namegr, xobs, npde, ypred, type.graph)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{namegr}{name of the file in which graphs are to be saved}
  \item{xobs}{the vector of observed independent variable (x)}
  \item{npde}{the vector of normalised prediction distribution errors (returned
  by the functions \code{npde} or \code{autonpde} with the option output=TRUE)}
  \item{ypred}{the vector giving the mean of the predicted distribution for each
observation}
  \item{type.graph}{type of graph (one of "eps","jpeg","png","pdf"}
}
\details{
  See \code{\link{plotnpde}} for a description of the graphs plotted.
}
\value{
  None
}
\references{K. Brendel, E. Comets, C. Laffont, C. Laveille, and F. Mentré.
Metrics for external model evaluation with an application to the population
pharmacokinetics of gliclazide. \emph{Pharmaceutical Research}, 23:2036--49,
2006.}
\author{Emmanuelle Comets <emmanuelle.comets@bichat.inserm.fr>}

\seealso{\code{\link{npde}}, \code{\link{autonpde}}, \code{\link{plotnpde}}}
\examples{
}
\keyword{models}
