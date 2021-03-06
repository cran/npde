% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mainNpde.R
\name{npde.save}
\alias{npde.save}
\alias{npde.save,NpdeObject-method}
\title{Save the results contained in a NpdeObject object to a file}
\usage{
npde.save(object, ...)
}
\arguments{
\item{object}{a NpdeObject object}

\item{\dots}{optional arguments to replace options in object}
}
\value{
No return value, called for side effects
}
\description{
Save the results to a table on disk
}
\details{
The following options can be changed by passing the appropriate arguments: namsav (string giving the root name of the files, an extension .npde will be added), nameres (string giving the full name of the file)
}
\references{
K. Brendel, E. Comets, C. Laffont, C. Laveille, and F.Mentre. Metrics for external model evaluation with an application to the population pharmacokinetics of gliclazide. \emph{Pharmaceutical Research}, 23:2036--49, 2006.
}
\keyword{IO}
\keyword{files}
