####################################################################################
# User-level functions

#' Compute normalised prediction distribution errors
#'
#' These functions compute normalised prediction distribution errors (npde) and
#' prediction discrepancies (pd). \code{npde} asks the user the name
#' and structure of the files containing the data, using \code{pdemenu}, while
#' \code{autonpde} takes these variables and others as arguments. 
#' 
#' Both functions compute the normalised prediction distribution errors (and/or
#' prediction discrepancies) in the same way. \code{npde} is an interactive
#' function whereas \code{autonpde} takes all required input as arguments.
#' 
#' Diagnostic graphs are produced for npd, and npde are used in the tests as
#' their distribution takes into account the correlation between repeated observations.
#'
#' When the computation of npde fails because of numerical problems, error
#' messages are printed out, then pd are computed instead and graphs of pd are
#' plotted so that the user may evaluate why the computation failed.
#'
#' @name autonpde
#' 
#' @usage autonpde(namobs, namsim, iid, ix, iy, imdv = 0, icens = 0,
#' icov = 0, iipred = 0, boolsave = TRUE, namsav = "output", type.graph = "eps",
#' verbose = FALSE, calc.npde=TRUE, calc.npd=TRUE, decorr.method = "cholesky",
#'  cens.method = "cdf", units = list(x="",y=""), detect=FALSE, ties=TRUE, header=TRUE)
#' @usage npde()
#' 
#' @param namobs name of the file containing the observed data, or a dataframe
#' containing the observed data (in both cases, the column containing the
#' various data required for the computation of the pde can be set using the
#' arguments \code{iid},\code{ix} and \code{iy} below)
#' @param namsim name of the file containing the simulated data, or a dataframe
#' containing the simulated data (the program will assume that subject ID are
#' in column 1 and simulated Y in column 3, see User Guide)
#' @param iid name/number of the column in the observed data containing the patient
#' ID; if missing, the program will attempt to detect a column named id
#' @param ix name/number of the column in the observed data containing the
#' independent variable (X); ; if missing, the program will attempt to detect a column named X
#' @param iy name/number of the column in the observed data containing the dependent
#' variable (Y); if missing, the program will attempt to detect a column with the response
#' @param imdv name/number of the column containing information about missing data
#' (MDV), defaults to 0 (column not present)
#' @param icens name/number of the column containing information about censored data
#' (cens), defaults to 0 (column not present)
#' @param icov name/number of the column(s) containing covariate information
#' defaults to 0 (no covariates)
#' @param iipred name/number of the column(s) with individual predictions
#' (ipred), defaults to 0 (individual predictions not available)
#' @param units a list with components x, y and cov (optional), specifying the
#' units respectively for the predictor (x), the response (y), and the covariates
#' (a vector of length equal to the number of covariates). Units will default to (-) if not given.
#' @param detect a boolean controlling whether automatic recognition of columns in
#' the dataset is on, defaults to FALSE
#' @param boolsave a boolean (TRUE if graphs and results are to be saved to a
#' file, FALSE otherwise), defaults to TRUE
#' @param namsav name of the files to which results are to be saved (defaults
#' to "output", which will produce a file called output.eps (if the default
#' format of postscript is kept, see type.graph) for the graphs and a file
#' called output.npde for the numerical results (see value)
#' @param type.graph type of graph (one of "eps","jpeg","png","pdf"), defaults
#' to postscript ("eps")
#' @param calc.npde a boolean (TRUE if npde are to be computed, FALSE otherwise),
#' defaults to TRUE
#' @param calc.npd a boolean (TRUE if npd are to be computed, FALSE otherwise), defaults
#' to TRUE
#' @param cens.method a character string indicating the method used to handle
#' censored data (see \code{\link{npde.cens.method}})
#' defaults to cdf
#' @param decorr.method a character string indicating the method used to decorrelate
#' observed and simulated data in the computation of npde (see \code{\link{npde.decorr.method}})
#' defaults to cholesky
#' @param ties a boolean (if FALSE, the distributions of pd and npde are smoothed by jittering the values so that there are no ties), defaults to TRUE
#' @param verbose a boolean (TRUE if messages are to be printed as each subject is
#' processed, FALSE otherwise), defaults to FALSE
#' @param header a boolean (TRUE if input files have headers,
#' FALSE otherwise), defaults to TRUE
#' @return An object of class \code{\link{NpdeObject}}
#'
#' @details The function also prints out the characteristics of the distribution of the
#' npde (mean, variance, skewness and kurtosis) as well as the results of the
#' statistical tests applied to npde. In addition, if boolsave is TRUE, two files
#' are created:
#' \describe{
#' \item{results file}{the numerical results are saved in a file
#' with extension .npde (the name of which is given by the user). The file
#' contains the components id, xobs, ypred, npde, pd stored in columns}
#' \item{graph file}{the graphs are saved to a file with the same name as the
#' results file, and with extension depending on the format.}
#' }
#' @author Emmanuelle Comets <emmanuelle.comets@@bichat.inserm.fr>
#' @seealso \code{\link{npde.graphs}}, \code{\link{gof.test}}
#' @references K. Brendel, E. Comets, C. Laffont, C. Laveille, and F.
#' Mentre. Metrics for external model evaluation with an application to the
#' population pharmacokinetics of gliclazide. \emph{Pharmaceutical Research},
#' 23:2036--49, 2006.
#' @references PDF documentation for npde 3.0: \url{https://github.com/ecomets/npde30/blob/main/userguide_npde_3.0.pdf}
#' @keywords models
#' @export
#' @examples
#' data(theopp)
#' data(simtheopp)
#'
#' # Calling autonpde with dataframes
#' x<-autonpde(theopp,simtheopp,1,3,4,boolsave=FALSE)
#' x
#' head(x["results"]["res"])
#' \donttest{
#' plot(x)
#' }

autonpde<-function(namobs,namsim,iid,ix,iy,imdv=0,icens=0,icov=0, iipred=0,boolsave=TRUE,namsav="output",type.graph="eps",verbose=FALSE, calc.npde=TRUE,calc.npd=TRUE,decorr.method="cholesky",cens.method="cdf", units=list(x="",y=""), detect=FALSE, ties=TRUE,header=TRUE) {

   # output is deprecated, now using invisible
#  if(is.data.frame(namobs)) namobs<-deparse(substitute(namobs))
#  if(is.data.frame(namsim)) namsim<-deparse(substitute(namsim))
  if(missing(iid)) iid<-""
  if(missing(ix)) ix<-""
  if(missing(iy)) iy<-""

  xdat<-npdeData(name.data=namobs,header=header,name.group=iid, name.predictor=ix,name.response=iy,name.covariates=icov,name.miss=imdv, name.cens=icens,name.ipred=iipred,units=units,verbose=verbose,detect=detect)

  xsim<-npdeSimData(npde.data=xdat,name.simdata=namsim,verbose=verbose,header=header)

  if(cens.method!="omit" & !calc.npd) {
    calc.npd<-TRUE
    if(verbose) message("To compute npde with the",cens.method," method, pd need to be computed first, changing to calc.npd.\n")
  }

  opt<-list(boolsave=boolsave,namsav=namsav,type.graph=type.graph, verbose=verbose,calc.npde=calc.npde, calc.npd=calc.npd,decorr.method=decorr.method, cens.method=cens.method, ties=ties)

  npde.obj<-new(Class="NpdeObject",data=xdat,sim.data=xsim,options=opt)

  npde.obj["prefs"]<-set.plotoptions(npde.obj)

  xret<-npde.main(npde.obj)
# ----------------------------------------------------------------------
  # Saving results
  if(npde.obj["options"]$boolsave) {
    npde.save(xret)
    npde.graphs(xret)
  }
  
  invisible(xret)

}

#' @rdname autonpde
#' @export

npde<-function() {
  xinput<-pdemenu()
  
  xdat<-npdeData(name.data=xinput$namobs,header=TRUE,name.group=xinput$iid, name.predictor=xinput$ix,name.response=xinput$iy,name.miss=xinput$imdv, name.cens=xinput$icens,name.ipred=xinput$iipred,name.covariates=xinput$icov, detect=xinput$detect)
  if(xdat$verbose) message("Simulated data:",xinput$namsim,"\n")
  xsim<-npdeSimData(npde.data=xdat,name.simdata=xinput$namsim)
  
  opt<-list(boolsave=xinput$boolsave,namsav=xinput$namfile, type.graph=xinput$type.graph,verbose=xinput$verbose,calc.npde=xinput$calc.npde, calc.npd=xinput$calc.npd,decorr.method=xinput$decorr.method,cens.method=xinput$cens.method,ties=xinput$ties, header=xinput$header)
  npde.obj<-new(Class="NpdeObject",data=xdat,sim.data=xsim,options=opt)
  
  
  npde.obj["prefs"]<-set.plotoptions(npde.obj)
  
  xret<-npde.main(npde.obj)
  # Saving results
  if(npde.obj["options"]$boolsave) {
    npde.save(xret)
    npde.graphs(xret)
  }
  invisible(xret)
  
}

#' Interactive menu to set the options for the npde() function
#'
#' Interactive menu to set the options for the npde() function
#'
#' @return A list with the information needed to compute the pd/npde
#' @author Emmanuelle Comets <emmanuelle.comets@@bichat.inserm.fr>
#' @seealso \code{\link{npde.graphs}}, \code{\link{gof.test}}
#' @references K. Brendel, E. Comets, C. Laffont, C. Laveille, and F.
#' Mentre. Metrics for external model evaluation with an application to the
#' population pharmacokinetics of gliclazide. \emph{Pharmaceutical Research},
#' 23:2036--49, 2006.
#' @keywords internal

pdemenu<-function() {
  ick<-0
  while(ick==0) {
    namobs<-readline(prompt="Name of the file containing the observed data : ")
    datobs<-try(read.table(namobs,na.strings=c(".","NA"),nrows=10))
    if(is(datobs,"try-error")) ick<-1 else
      cat("\n      File",namobs,"does not exist.\n")
  }

  cok<-readline(prompt="Automatic recognition of columns in the dataset (y/Y) [default=no] ? ")
  if(tolower(cok)=="y"|tolower(cok)=="yes") detect<-TRUE else detect<-FALSE
  if(detect) {
    iid<-1;ix<-2;iy<-3;imdv<-0
    cat("I'm assuming file",namobs,"has the following structure :\n")
    cat("        ID X Y ...\n")
    cat("and does not contain a column signaling missing data.\n")
    cok<-readline(prompt="To keep, press ENTER, to change, type any letter : ")
    if(cok=="")
      cat("Keeping this structure\n")
  } else cok<-"c"
  if(cok!="") {
    cat("In the following, you may enter the name of the column or the column number\n")
    iid<-as.integer(readline(prompt="         Column with ID information ? "))
    ix<-as.integer(readline(prompt="         Column with X (eg time) information ? "))
    iy<-as.integer(readline(prompt="         Column with Y (eg DV) information ? "))
    imdv<-as.integer(readline(prompt="         Column signaling missing data (eg MDV, press ENTER if none) ? "))
    if(is.na(imdv)) imdv<-0
    icens<-as.integer(readline(prompt="         Column signaling censoring (eg CENS, press ENTER if none) ? "))
    if(is.na(icens)) icens<-0
    iipred<-as.integer(readline(prompt="         Column with individual predictions (eg ipred, press ENTER if none) ? "))
    if(is.na(iipred)) iipred<-0
    icov<-c()
    ic<-as.integer(readline(prompt="         Columns with covariates (eg WT; enter one at a time, press ENTER if none or when finished) ? "))
    while(!is.na(ic)) {
      icov<-c(icov,ic)
      ic<-as.integer(readline(prompt="next covariate (press ENTER when finished)"))
    }
    if(is.null(icov)) icov<-0
  }
  ick<-0
  while(ick==0) {
    namsim<-readline(prompt="Name of the file containing the simulated data : ")
    datobs<-try(read.table(namsim,na.strings=c(".","NA"),nrows=10))
    if(is(datobs,"try-error")) ick<-1 else
      cat("\n      File",namsim,"does not exist.\n")
  }
  cok<-readline(prompt="Do the input files have a header line (y/Y) [default=yes] ? ")
  if(cok=="y"|cok=="Y"|cok=="yes"|cok=="") header<-TRUE else header<-FALSE
  cok<-readline(prompt="Do you want results and graphs to be saved to files (y/Y) [default=yes] ? ")
  if(cok=="y"|cok=="Y"|cok=="yes"|cok=="") boolsave<-TRUE else boolsave<-FALSE
  type.graph<-"eps";namegr<-nameres<-"";decorr.method<-"cholesky";cens.method<-"cdf"
  if(boolsave) {
    cat("Different formats of graphs are possible :\n")
    cat("         1. Postscript (extension eps, default)\n")
    cat("         2. JPEG (extension jpeg)\n")
    cat("         3. PNG (extension png)\n")
    cat("         4. Acrobat PDF (extension pdf)\n")
    cok<-as.integer(readline(prompt="Which format would you like for the graph (1-4) [default 1] ? "))
    if(is.na(cok) || cok>4 || cok<0) {
      cok<-1
      cat("       postscript graph selected\n")
    }
    if(cok==2) type.graph<-"jpeg"
    if(cok==3) type.graph<-"png"
    if(cok==4) type.graph<-"pdf"
    namfile<-readline(prompt="Name of the file (extension will be added, default=output):")
    namfile<-ifelse(namfile=="","output",namfile)
  }
  ick<-0
  while(ick==0) {
    cok<-readline(prompt="Do you want to compute npde (y/Y) [default=yes] ? ")
    if(cok=="y"|cok=="Y"|cok=="yes"|cok=="") {calc.npde<-TRUE;ick<-1} else
      calc.npde<-FALSE
    cok<-readline(prompt="Do you want to compute npd (y/Y) [default=yes] ? ")
    if(cok=="y"|cok=="Y"|cok=="yes"|cok=="") {calc.npd<-TRUE;ick<-1} else
      calc.npd<-FALSE
    if(ick==0) cat("\n Please choose to compute at least one of npde or pd.\n")
  }
  cat("Different decorrelation methods are available:\n")
  cat("         1. Cholesky decomposition (default)\n")
  cat("         2. Inverse using diagonalisation (as in Monolix and Nonmem)\n")
  cat("         3. Cholesky followed by polar decomposition\n")
  cok<-as.integer(readline(prompt="Which method should be used for the decorrelation (1-3)  [default 1] ? "))
  if(is.na(cok) || cok>3 || cok<0) {
    cok<-1
    cat("       Cholesky method selected\n")
  }
  if(cok==2) decorr.method<-"inverse"
  if(cok==3) decorr.method<-"polar"
  cat("Method used to handle censored observations:\n")
  cat("         1. omit: pd will be set to NaN for missing data\n")
  cat("         2. cdf: pd will be imputed using a random sample from U(0,p_LOQ) where p_LOQ is the probability, according to the model, that a given observation is less than LOQ (default)\n")
  cat("         3. loq: an observation below the LOQ will be imputed to the LOQ\n")
  cat("         4. ppred: an observation below the LOQ will be imputed to the population model prediction\n")
  cat("         5. ipred: an observation below the LOQ will be imputed to the individual model prediction\n")
  cok<-as.integer(readline(prompt="Which method should be used (1-5)  [default 2] ? "))
  if(is.na(cok) || cok>5 || cok<0) {
    cok<-2
    cat("       cdf method selected\n")
  }
  if(cok==1) cens.method<-"omit"
  if(cok==3) cens.method<-"loq"
  if(cok==4) cens.method<-"ppred"
  if(cok==5) cens.method<-"ipred"
  if(cens.method!="omit" & !calc.npd) {
    calc.npd<-TRUE
    cat("To compute npde with the",cens.method," method, pd need to be computed first, changing calc.npd to TRUE\n")
  }
  verbose<-FALSE
  if(calc.npde) {
    cok<-readline(prompt="Do you want a message printed as the computation of npde begins in a new subject (y/Y) [default=no] ? ")
    if(cok=="y"|cok=="Y"|cok=="yes") verbose<-TRUE
  }
  ties<-TRUE
  cok<-readline(prompt="Do you want to allow different observations to have the same value of pd/npde (y/Y) [default=yes, if no, pd and npde will be jittered] ? ")
  if(tolower(cok)=="n"|tolower(cok)=="no") ties<-TRUE
  return(list(namobs=namobs,namsim=namsim,iid=iid,ix=ix,iy=iy,imdv=imdv,icens=icens, iipred=iipred,icov=icov,boolsave=boolsave,type.graph=type.graph,namfile=namfile, calc.npd=calc.npd,calc.npde=calc.npde,verbose=verbose,decorr.method=decorr.method, cens.method=cens.method,detect=detect,ties=ties,header=header))
}

####################################################################################
######## core package function

#' Main npde function
#'
#' Main npde function, used to compute pd and npde
#'
#' @param object A NpdeObject object
#' @return A NpdeObject object updated with the results
#'
##' @keywords model internal

#setMethod("npde.main","NpdeObject",
npde.main <- function(object) {
    #  	cat("Entering npde.main\n")
    if(object["options"]$calc.npd) object<-computepd(object)
    if(!object["options"]$calc.npde) {
      if(object["options"]$verbose) cat("npde have not been computed, showing tests based on npd. Warning: these tests suffer from inflated type I error rates!! n")
      gof.test(object,which="npd")
    }
    #    cat("Computation of pd successful\n")
    if(object["options"]$calc.npde) {
      object<-computenpde(object)
      gof.test(object)
    }
    #    cat("Computation of npde successful\n")
    return(object)
  }
#)

####################################################################################
######## Save results

#' Save the results contained in a NpdeObject object to a file
#'
#' Save the results to a table on disk
#'
##' @aliases npde.save,NpdeObject-method
#' @usage npde.save(object, ...)
#' @param object a NpdeObject object
#' @param \dots optional arguments to replace options in object
#' @return No return value, called for side effects
#' @details The following options can be changed by passing the appropriate arguments: namsav (string giving the root name of the files, an extension .npde will be added), nameres (string giving the full name of the file)
#' @references K. Brendel, E. Comets, C. Laffont, C. Laveille, and F.Mentre. Metrics for external model evaluation with an application to the population pharmacokinetics of gliclazide. \emph{Pharmaceutical Research}, 23:2036--49, 2006.
#' @keywords IO files
#'
#' @export
#' @importFrom utils write.table

npde.save <- function(object, ...) {
  args1<-match.call(expand.dots=TRUE)
  i1<-match("namsav",names(args1))
  i2<-match("namres",names(args1))
  namres<-object["options"]$namres
  if(!is.na(i2)) {
    namres<-as.character(args1[[i2]])
  } else {
    if(!is.na(i1)) {
      namres<-paste(as.character(args1[[i1]]),".npde",sep="")
    }
  }
  if(namres=="") {
    cat("Please provide a filename in the namres item of the options component.\n")
    invisible()
  } else {
    if(object@options$verbose) cat("Saving results in file",namres,"\n")
  }
  namcol<-c(object@data@name.group,object@data@name.predictor, object@data@name.response,object@data@name.cens,object@data@name.miss, object@data@name.covariates,object@data@name.ipred)
  saveres<-object["data"]["data"][,intersect(namcol, colnames(object@data@data))]
  namcol2<-c("ypred","pd","npde", "npd")
  saveres<-cbind(saveres,object["results"]["res"][,intersect(namcol2,colnames(object@results@res))])
  write.table(saveres,namres,row.names=FALSE,quote=FALSE)
}

######## Save graphs

#' Save the graphs for a NpdeObject object to a file
#'
#' Save the graphs to a file on disk
#'
##' @aliases npde.graphs,NpdeObject-method
#' @usage npde.graphs(object, ...)
#' @param object a NpdeObject object
#' @param \dots optional arguments to replace options in object
#' 
#' @return No return value, called for side effects
#' @details The following options can be changed by passing the appropriate arguments: namsav (string giving the root name of the files, an extension depending on the type of graph will be added), namgr (string giving the full name of the file), type.graph (one of "eps", "pdf", "jpeg", "png")
#' @references K. Brendel, E. Comets, C. Laffont, C. Laveille, and F.Mentre. Metrics for external model evaluation with an application to the population pharmacokinetics of gliclazide. \emph{Pharmaceutical Research}, 23:2036--49, 2006.
#' @keywords IO files
#'
#' @importFrom grDevices dev.off jpeg pdf png postscript
#' @export

npde.graphs <- function(object,...) {
  args1<-match.call(expand.dots=TRUE)
  i1<-match("namgr",names(args1))
  i2<-match("namsav",names(args1))
  i3<-match("type.graph",names(args1))
  namsav<-object["options"]$namsav
  namgr<-object["options"]$namgr
  if(!is.na(i3)) {
    type.graph<-as.character(args1[[i3]])
  } else type.graph<-object["options"]$type.graph
  if(!is.na(i3) & is.na(i1) & is.na(i2)) namgr<-paste(namsav,type.graph,sep=".")
  if(!is.na(i1)) {
    if(is.na(i3)) namgr<-as.character(args1[[i1]]) else namgr<-as.character(args1[[i1]])
  } else {
    if(!is.na(i2)) {
      namsav<-as.character(args1[[i2]])
      namgr<-paste(namsav,type.graph,sep=".")
    }
  }
  if(length(namgr)==0 || namgr=="") {
    cat("Please provide a filename in the namgr item of the options component or as an option 'namgr=XXX' when calling npde.graphs().\n")
    return()
  } else {
    if(object@options$verbose) cat("Saving graphs in file",namgr,"\n")
  }
  if(type.graph=="jpeg") {
    if(!capabilities("jpeg")) {
      if(object@options$verbose) cat("R was not compiled with jpeg capabilities, switching to PDF format.\n")
      type.graph<-"pdf"
      namgr<-paste(namsav,type.graph,sep=".")
  }}
  if(type.graph=="png") {
    if(!capabilities("png")) {
      if(object@options$verbose) cat("R was not compiled with png capabilities, switching to PDF format.\n")
      type.graph<-"pdf"
      namgr<-paste(namsav,type.graph,sep=".")
  }}
  if(type.graph=="eps") {
      if(object@options$verbose) cat("Transparency options not working with ggsave, please use cairo to save the plot in eps format or choose another output format (pdf, png, jpeg).\n")
  }
  if(type.graph %in% c("eps","jpeg","pdf","png")) ggsave(plot=plot(object, ...), filename=namgr, width=29.7, height=21, units="cm")
}

####################################################################################
