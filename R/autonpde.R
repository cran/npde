`autonpde` <-
function(namobs,namsim,iid=1,ix=2,iy=3,imdv=0,namsav="output",
    boolsave=TRUE,type.graph="eps",output=TRUE,verbose=FALSE,
    calc.npde=TRUE,calc.pd=FALSE) 
{
    if(boolsave) {
    	namegr<-paste(namsav,".",type.graph,sep="")
    	namout<-paste(namsav,".npde",sep="")
	}
    if(is.character(namobs)) {
    #namobs=name of a file to read
      if (verbose) cat("Reading observed data file",namobs,"\n")
      x<-readobs(namobs,iid,ix,iy,imdv)
      tabobs<-x$tabobs;mdv<-x$mdv
    } else {
    #namobs=name of a datafile
      tabobs<-namobs
      names(tabobs)<-tolower(names(tabobs))
      if(verbose) cat("Checking the dataframe containing the observed data\n")
      if(sum(is.na(match(c("id","xobs","yobs"),names(tabobs))))>0) {
        if(is.na(match(c("id"),names(tabobs)))) {
           names(tabobs)[iid]<-"id"
           if (verbose) cat("      Assuming ID is in column",iid,"\n")
           }
        if(is.na(match(c("xobs"),names(tabobs)))) {
           names(tabobs)[ix]<-"xobs"
           if (verbose) cat("      Assuming independent variable (X) is in column",ix,"\n")
           }
        if(is.na(match(c("yobs"),names(tabobs)))) {
           names(tabobs)[iy]<-"yobs"
           if (verbose) cat("      Assuming dependent variable (Y) is in column",iy,"\n")
           }
        if(is.na(match(c("MDV"),names(tabobs))) & imdv>0) {
           names(tabobs)[imdv]<-"mdv"
           if (verbose) cat("      Assuming missing data variable (MDV) is in column",imdv,"\n")
           }
      }
      if(imdv==0) mdv<-is.na(tabobs$yobs) else mdv<-tabobs$mdv
      tabobs<-tabobs[mdv==0,c("id","xobs","yobs")]
    }
    if(is.character(namsim)) {
    #namsim=name of a file to read
      if (verbose) cat("Reading simulated data file",namsim,"\n")
      tabsim<-readsim(namsim,tabobs,mdv)
    } else {
    #namsim=name of a datafile
      if(verbose) cat("Checking the dataframe containing the simulated data\n")
      tabsim<-namsim
      names(tabsim)<-tolower(names(tabsim))
      if(sum(is.na(match(c("idsim","irsim","ysim"),names(tabsim))))>0) {
        if(is.na(match(c("idsim"),names(tabsim)))) {
           names(tabsim)[1]<-"idsim"
           if (verbose) cat("      Assuming ID of simulated data is in column 1\n")
           }
        if(is.na(match(c("irsim"),names(tabsim)))) {
           nobs<-length(mdv)
           nrep<-dim(tabsim)[1]/nobs
           irsim<-rep(1:nrep,each=nobs)
           tabsim[,2]<-irsim
           names(tabsim)[2]<-"irsim"
          }
        if(is.na(match(c("ysim"),names(tabsim)))) {
           names(tabsim)[3]<-"ysim"
           if (verbose) cat("      Assuming dependent variable (Y) of simulated data is in column 3\n")
           }
      }
      tabsim<-tabsim[mdv==0,c("idsim","irsim","ysim")] 
    }
    #Preparing the list to return
    xdum<-rep(NA,length(tabobs$id))
    xret<-list(obsdat=tabobs,ydobs=xdum,ydsim=xdum,ypred=xdum,
        xerr=NA,npde=NA,pd=NA)
    if(calc.npde) {
      cat("Computing npde\n")
      x<-computenpde(tabobs,tabsim,verbose)
    #plotting graphs
      if(x$xerr==0) {
       if(boolsave) graphnpde(namegr,tabobs$xobs,x$npde,x$ypred,type.graph)
       plotnpde(tabobs$xobs,x$npde,x$ypred)
       }
    #performing tests
      if(x$xerr==0) testnpde(x$npde) else calc.pd<-TRUE
      xret$ydobs<-x$ydobs;xret$ydsim<-x$ydsim;xret$xerr<-x$xerr
      xret$npde<-x$npde;xret$ypred<-x$ypred
    }
    if(calc.pd) {
      cat("Computing pd\n")
      xpd<-computepd(tabobs,tabsim,calc.npde)
      if(calc.npde) ypred<-x$ypred else ypred<-xpd$ypred
      xret$pd<-xpd$pd
      if(!calc.npde) xret$ypred<-xpd$ypred
      if(!calc.npde || (!is.na(xret$xerr) & xret$xerr>0))
         plotpd(tabobs$xobs,xpd$pd,xpd$ypred)
    }
    if(boolsave) {
        if(calc.npde && xret$xerr==0) npde<-xret$npde else npde<-xdum
        if(calc.pd) pd<-xret$pd else pd<-xdum
        saveres<-data.frame(id=xret$obsdat$id,xobs=xret$obsdat$xobs,
          yobs=xret$obsdat$yobs,ypred=xret$ypred,npde=npde,pd=pd)
        if(namout!="") {
          write.table(saveres,namout,row.names=FALSE,quote=FALSE)
          cat("Saving results in file",namout,"\n")
        }
    }
    if (output==TRUE) return(xret) else return(NULL)
}

