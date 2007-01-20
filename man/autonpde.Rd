\name{autonpde}
\alias{autonpde}
\alias{npde}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Compute normalised prediction distribution errors}
\description{
  These functions compute normalised prediction distribution errors (npde) and
  optionally prediction discrepancies (pd). \code{npde} asks the
  user the name and structure of the files containing the data, using
  \code{pdemenu}, while \code{autonpde} takes these variables and others as
  arguments.
}
\usage{
autonpde(namobs, namsim, iid = 1, ix = 2, iy = 3, imdv = 0, namsav = "output", 
boolsave = TRUE, type.graph = "eps", output = TRUE, verbose = FALSE, 
calc.npde=TRUE, calc.pd=FALSE)
npde()
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{namobs}{name of the file containing the observed data, or a dataframe
  containing the observed data (in both cases, the column containing the various
  data required for the computation of the pde can be set using the arguments
  \code{iid},\code{ix} and \code{iy} below)}
  \item{namsim}{name of the file containing the simulated data, or a dataframe
  containing the simulated data (the program will assume that subject ID are in
  column 1 and simulated Y in column 3, see User Guide)}
  \item{iid}{number of the column in the observed data containing the patient
  ID, defaults to 1 (will be ignored if the dataframe contains a column named
  id)}
  \item{ix}{number of the column in the observed data containing the
  independent variable (X), defaults to 2 (will be ignored if the dataframe
  contains a column named xobs)}
  \item{iy}{number of the column in the observed data containing the dependent
  variable (Y), defaults to 3 (will be ignored if the dataframe contains a
  column named yobs)}
  \item{imdv}{number of the column containing information about missing data
  (MDV), defaults to 0 (column not present)}
  \item{namsav}{name of the files to which results are to be saved (defaults to
  "output", which will produce a file called output.eps (if the default format
  of postscript is kept, see type.graph) for the graphs and a file called
  output.npde for the numerical results (see value)}
  \item{boolsave}{a boolean (T if graphs and results are to be saved to a file,
  F otherwise), defaults to T}
  \item{type.graph}{type of graph (one of "eps","jpeg","png","pdf"), defaults to
  postscript ("eps")}
  \item{output}{a boolean (T if the function should return values, F otherwise),
  defaults to T}
  \item{verbose}{a boolean (T if messages are to be printed as each subject is
  processed, F otherwise), defaults to F}
  \item{calc.npde}{a boolean (T if npde are to be computed,
  F otherwise), defaults to T}
  \item{calc.pd}{a boolean (T if pd are to be computed, F otherwise), defaults
  to F}
}
\details{
  Both functions compute the normalised prediction distribution errors (and/or
  prediction discrepancies) in the same way. \code{npde} is an
  interactive function whereas \code{autonpde} takes all required input as
  arguments.
  
  When the computation of npde fails because of numerical problems, error
  messages are printed out, then pd are computed instead and graphs of pd are
  plotted so that the user may evaluate why the computation failed.
}
\value{ A list containing the following components:
  \item{obsdat}{observed data, a dataframe containing 3 columns (id=patient ID,
  xobs=independent variable (X), yobs=dependent variable (Y)}
  \item{simdat}{simulated data, a dataframe containing 4 columns (idsim=patient
  ID, irsim=integer identifying the replicate; xsim=independent variable (X),
  ysim=dependent variable (Y)}
  \item{ydobs}{the vector of observed dependent variable (Y), decorrelated}
  \item{ydsim}{the vector of simulated dependent variable (Y), decorrelated}
  \item{ypred}{a vector giving the mean of the predicted distribution for each
observation}
  \item{xerr}{an integer code to keep track of errors during the computation;
  after a successful computation the value of \code{xerr} should be 0. A value
  of 1 or 2 signals errors during the computation.}
  \item{npde}{the vector of normalised prediction distribution errors, if
  calc.npde=T (NA otherwise)}
  \item{pd}{the vector of prediction discrepancies, if calc.npd=T
  (NA otherwise)}
  
  The function also prints out the characteristics of the distribution of the
  npde (mean, variance, skewness and kurtosis) as well as the results of the
  statistical tests applied to npde. In addition, if boolsave is T, two files
  are created:
  \item{results file}{the numerical results are saved in a
  file with extension .npde (the name of which is given by the user). The file
  contains the components id, xobs, ypred, npde, pd stored in columns}
  \item{graph file}{the graphs are saved to a file with the same name as the
  results file, and with extension depending on the format. See also
  \code{\link{graphnpde}}} }
\references{K. Brendel, E. Comets, C. Laffont, C. Laveille, and F. Mentré.
Metrics for external model evaluation with an application to the population
pharmacokinetics of gliclazide. \emph{Pharmaceutical Research}, 23:2036--49,
2006.}
\author{Emmanuelle Comets <emmanuelle.comets@bichat.inserm.fr>}

\seealso{\code{\link{graphnpde}}, \code{\link{testnpde}}}
\examples{
data(theopp)
data(simtheopp)

# Calling autonpde with dataframes

x<-autonpde(theopp,simtheopp,1,3,4,boolsave=FALSE)
x$npde

# Calling autonpde with names of files to be read from disk

write.table(theopp,"theopp.tab",quote=FALSE,row.names=FALSE)
write.table(simtheopp,"simtheopp.tab",quote=FALSE,row.names=FALSE)
x<-autonpde(namobs="theopp.tab", namsim="simtheopp.tab", iid = 1,
ix = 3, iy = 4, imdv=0, boolsave = FALSE)

x$npde
}
\keyword{models}
