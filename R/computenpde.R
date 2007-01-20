`computenpde` <-
function(tabobs,tabsim,verbose) 
{
    #prediction distribution errors
    nobs<-dim(tabobs)[1]
    nrep<-dim(tabsim)[1]/nobs
    ypred<-ydobs<-pde<-tabobs$yobs
    ydsim<-tabsim$ysim
    #computing pde
    for(isuj in unique(tabobs$id)) {
      msuj<-tabobs[tabobs$id==isuj,]
      matsim<-matrix(tabsim$ysim[tabsim$idsim==isuj],ncol=nrep)
      x<-calcnpde(isuj,msuj,matsim,nrep,verbose)
      xerr<-x[[1]];pde[tabobs$id==isuj]<-x[[2]]
      ydsim[tabsim$idsim==isuj]<-x[[3]];ydobs[tabobs$id==isuj]<-x[[4]]
      ypred[tabobs$id==isuj]<-x[[5]]
      if(xerr>0) {
        cat("The computation of the pde has failed for subject",isuj,"because \n")
        if(xerr==1) cat("the Cholesky decomposition of the covariance matrix of the simulated data could not be obtained.\n")
        if(xerr==2) cat("the covariance matrix of the simulated data could not be inverted.\n")
        cat("This usually means that the covariance matrix is not positive-definite.\n")
        cat("This can be caused by simulations widely different from observations (in \n")
        cat("other words, a poor model).\n")
        cat("We suggest to plot a prediction interval from the simulated data to check\n")
        cat("whether the simulations are reasonable, and to consider prediction\n")
        cat("discrepancies.\n")
        cat("Prediction discrepancies will now be computed.\n")
        break}
      }
    #saving pde
    if(xerr==0) npde<-qnorm(pde) else npde<-rep(NA,length(pde))
    return(list(ydsim=ydsim,ydobs=ydobs,xerr=xerr,npde=npde,ypred=ypred))
}

