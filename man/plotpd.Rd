\name{plotpd}
\alias{plotpd}
\title{Plots graphs of the normalised prediction distribution errors}
\description{
  Plots 4 graphs to evaluate the shape of the distribution of prediction
discrepancies

}
\usage{
plotpd(xobs, pd, ypred)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{xobs}{the vector of the observed independent variable (X)}
  \item{pd}{the vector of normalised prediction distribution errors (returned
  by the functions \code{npde} or \code{autonpde} with the option output=TRUE)}
  \item{ypred}{a vector giving the mean of the predicted distribution for each
observation}
}
\details{Four graphs are produced:
\item{a quantile-quantile plot}{plot of the pd versus the corresponding
quantiles of a uniform distribution, with the line y=x overlayed.}
\item{a histogram of the pd}{the line corresponding to the uniform distribution
is also shown}
\item{two scatterplots of the pd}{a plot of the pd versus the independent
variable X and a plot of the pd versus the empirical mean of the predicted
distribution; for these last two graphs, we plot the lines corresponding to
y=0.5, y=0.05 and y=0.95.}
}
\value{
  None
}
\references{K. Brendel, E. Comets, C. Laffont, C. Laveille, and F. Mentré.
Metrics for external model evaluation with an application to the population
pharmacokinetics of gliclazide. \emph{Pharmaceutical Research}, 23:2036--49,
2006.}
\author{Emmanuelle Comets <emmanuelle.comets@bichat.inserm.fr>}

\seealso{\code{\link{npde}}, \code{\link{autonpde}}}
\examples{

#Computing pd
data(theopp)
data(simtheopp)

x<-autonpde(theopp,simtheopp,1,3,4,boolsave=FALSE,calc.pd=TRUE,calc.npde=FALSE)
x$pd

#Using the npde in object x for the plot
plotpd(x$obsdat$xobs,x$pd,x$ypred)
}
\keyword{models}
