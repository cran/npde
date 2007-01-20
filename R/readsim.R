`readsim` <-
function(namsim,datobs,mdv) 
{
    mysep<-""
    datsim<-read.table(namsim,na=c(".","NA"))
    x1<-unlist(strsplit(as.character(datsim[1,1]),",",fixed=TRUE))
    if(length(x1)>1) mysep<-","
    x1<-unlist(strsplit(as.character(datsim[1,1]),";",fixed=TRUE))
    if(length(x1)>1) mysep<-";"
    if(mysep!="") datsim<-read.table(namsim,na=c(".","NA"),sep=mysep)
    if(!is.numeric(datsim[1,1]))
      datsim<-read.table(namsim,na=c(".","NA"),header=TRUE,sep=mysep)
    if(!is.numeric(datsim[1,1])) {
      cat("The format of the file containing the simulated data is unknown.\n")
      cat("Please use a standard R table format, with or without header,\n")
      cat("and with one of the following separators: \n")
      cat("         TAB or space(s), commas (',') or semicolons (';')\n")
      cat("Also note that a dot should be used to indicate digits in numbers.\n")
      stop("Exiting npde\n")
    }
    nsuj<-length(unique(datobs$id))
    nobs<-length(mdv)
    nrep<-dim(datsim)[1]/nobs
    if(trunc(nrep)!=nrep) {
       cat("The number of observed and simulated observations do not match:\n")
       cat("    number of observations:",nobs,"\n")
       cat("    number of simulated observations:",dim(datsim)[1],"\n")
       cat("Please check the input files and run npde again.\n")
       stop("Exiting npde\n")
    }
    if(nrep<1000) {
      cat("Warning: the number of simulations is",nrep,"which may be too small.\n")
      cat("We advise performing at least 1000 simulations to compute npde.\n")
      } 
    irsim<-rep(1:nrep,each=nobs)
    idsim<-datsim[,1]
    xsim<-datsim[,2]
    ysim<-datsim[,3]
    idsim<-idsim[mdv==0]
    irsim<-irsim[mdv==0]
    xsim<-xsim[mdv==0]
    ysim<-ysim[mdv==0]
    tabsim<-data.frame(idsim,irsim,xsim,ysim)
    return(tabsim)
}

