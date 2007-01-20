`npde` <-
function() 
{
    x<-pdemenu()
    namobs<-x[[1]]
    namsim<-x[[2]]
    iid<-x[[3]]
    ix<-x[[4]]
    iy<-x[[5]]
    imdv<-x[[6]]
    boolsave<-x[[7]]
    type.graph<-x[[8]]
    namegr<-x[[9]]
    calc.pd<-x[[10]]
    calc.npde<-x[[11]]
    namout<-x[[12]]
    output<-x[[13]]
    verbose<-x[[14]]
    x<-readobs(namobs,iid,ix,iy,imdv)
    tabobs<-x$tabobs;mdv<-x$mdv
    tabsim<-readsim(namsim,tabobs,mdv)
    #Preparing the list to return
    xdum<-rep(NA,length(tabobs$id))
    xret<-list(obsdat=tabobs,ydobs=xdum,ydsim=xdum,ypred=xdum,
        xerr=NA,npde=NA,pd=NA)
    if(calc.npde) {
      cat("Computing npde\n")
      x<-computenpde(tabobs,tabsim,verbose)
    #plotting graphs
      if(x$xerr==0) {
        if(boolsave==TRUE) graphnpde(namegr,tabobs$xobs,x$npde,x$ypred,type.graph)
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
      xret$pd<-xpd$pd
      if(!calc.npde) xret$ypred<-xpd$ypred
      if(!calc.npde || (!is.na(xret$xerr) & xret$xerr>0))
         plotpd(tabobs$xobs,xret$pd,xret$ypred)
    }
    if(boolsave) {
        if(calc.npde && xret$xerr==0) npde<-xret$npde else npde<-xdum
        if(calc.pd) pd<-xret$pd else pd<-xdum
        saveres<-data.frame(id=xret$obsdat$id,xobs=xret$obsdat$xobs,
          yobs=xret$obsdat$yobs,ypred=xret$ypred,npde=npde,pd=pd)
        if(namout!="")
          write.table(saveres,namout,row.names=FALSE,quote=FALSE)
          cat("Saving results in file",namout,"\n")
    }
    if (output==TRUE) return(xret) else return(NULL)
}

