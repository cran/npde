`calcnpde` <-
function(isuj,msuj,matsim,nrep,verbose) 
{
    if (verbose) cat("Computing the npde for subject ",isuj,"\n")
    #Computing decorrelated ysim* and yobs* for subject isuj
    #variance-covariance matrix computed using the cov function
    #Computing ypred
    ypred<-apply(matsim,1,mean)
    varsim<-cov(t(matsim))
    moysim<-apply(matsim,1,mean)
    #computing V-1/2 with Cholesky
    xerr<-0
    xmat<-try(chol(varsim))
    if(is.numeric(xmat)) {
      ymat<-try(solve(xmat))
      if(!is.numeric(ymat)) 
        xerr<-2
    } else 
      xerr<-1
    if(xerr==0) {
    #decorrelation of the simulations
    decsim<-t(ymat)%*%(matsim-moysim)
    decobs<-t(ymat)%*%(msuj$yobs-moysim)
    ydsim<-c(decsim)
    #decorrelation of the observations
    ydobs<-decobs
    #Computing the pde
    tcomp<-apply(decsim,2,"<",decobs)
    if(!is.matrix(tcomp)) tcomp<-t(as.matrix(tcomp))
    ycal<-apply(tcomp,1,mean)
    ycal[ycal==0]<-1/nrep
    ycal[ycal==1]<-1-1/nrep
    pde<-ycal
    }
    return(list(xerr,pde,ydsim,ydobs,ypred))
}

