\name{testnpde}
\alias{testnpde}
\title{Tests for normalised prediction distribution errors}
\description{
  Performs tests for the normalised prediction distribution errors returned by
\code{npde}
}
\usage{
testnpde(npde)
}
\arguments{
  \item{npde}{the vector of prediction distribution errors (returned by the
  functions \code{npde} or \code{autonpde} with the option output=TRUE)}
}
\details{
  Given a vector of normalised prediction distribution errors (npde) computed by \code{npde}
  or \code{autonpde}, this function compares the npde to the standardised
  normal distribution N(0,1) using a Wilcoxon test of the mean, a Fisher test of the
  variance, and a Shapiro-Wilks test for normality. A global test is also
  reported.
}
\value{
  a list containing 4 components:
  \item{Wilcoxon test of mean=0}{compares the mean of the npde
  to 0 using a Wilcoxon test}
  \item{variance test       }{compares the variance of the npde
  to 1 using a Fisher test}
  \item{SW test of normality}{compares the npde to the normal
  distribution using a Shapiro-Wilks test}
  \item{global test         }{an adjusted p-value corresponding to the minimum
  of the 3 previous p-values multiplied by the number of tests (3), or 1 if
  this p-value is larger than 1.}
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

#Testing npde
y<-testnpde(x$npde)

# Not Run
# Assuming the results were saved to a file output.npde
# using the boolsave=T option and namsav="output" (default)
# dat<-read.table("output.npde",header=T)
# testnpde(dat$npde) 
}
\keyword{models}
