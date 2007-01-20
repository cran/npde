\name{plotnpde}
\alias{plotnpde}
\title{Plots graphs of the normalised prediction distribution errors}
\description{
  Plots 4 graphs to evaluate the shape of the distribution of the normalised
prediction distribution errors (npde)

}
\usage{
plotnpde(xobs, npde, ypred)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{xobs}{the vector of the observed independent variable (X)}
  \item{npde}{the vector of normalised prediction distribution errors (returned
  by the functions \code{npde} or \code{autonpde} with the option output=TRUE)}
  \item{ypred}{a vector giving the mean of the predicted distribution for each
observation}
}
\details{Four graphs are produced:
\item{a quantile-quantile plot}{plot of the npde versus the corresponding
quantiles of a normal distribution, with the line y=x overlayed.}
\item{a histogram of the npde}{the shape of the normal distribution is also
shown}
\item{two scatterplots of the npde}{a plot of the npde versus the independent
variable X and a plot of the npde versus the empirical mean of the predicted
distribution; for these last two graphs, we plot the lines corresponding to y=0
and to the 5% and 95% critical value of the normal distribution delimiting a
90% prediction interval for the npde}
}}
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

#Computing npde
data(theopp)
data(simtheopp)

x<-autonpde(theopp,simtheopp,1,3,4,boolsave=FALSE)
x$npde

#Using the npde in object x for the plot
plotnpde(x$obsdat$xobs,x$npde,x$ypred)
}
\keyword{models}
