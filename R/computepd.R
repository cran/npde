`computepd` <-
function(tabobs,tabsim,calc.npde) 
{
    #prediction discrepancies
    ypredall<-c();pd<-c()
    nobs<-dim(tabobs)[1]
    nrep<-dim(tabsim)[1]/nobs
    for(isuj in unique(tabobs$id)) {
      matsim<-matrix(tabsim$ysim[tabsim$idsim==isuj],ncol=nrep)
      tcomp<-apply(matsim,2,"<",tabobs$yobs[tabobs$id==isuj])
      if(!is.matrix(tcomp)) tcomp<-t(as.matrix(tcomp))
      ycal<-apply(tcomp,1,mean)
      ycal[ycal==0]<-1/nrep
      ycal[ycal==1]<-1-1/nrep
      pd<-c(pd,ycal)
      if(!calc.npde) {
	ypred<-apply(matsim,1,mean)
	ypredall<-c(ypredall,ypred)
      } 
    }
    #Computing ypred if not computed previously in computenpde
    if(calc.npde) ypredall<-NA
    #saving pd
    return(list(pd=pd,ypred=ypredall))
}

