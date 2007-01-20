`plotpd` <-
function(xobs,pd,ypred) 
{
    nclass<-10
    par(mfrow=c(2,2))
    samp<-sort(pd);ndat<-length(samp)
    theo<-c(1:ndat)/ndat
    qqplot(samp,theo,xlab="Sample quantiles (pd)",ylab="Theoretical Quantiles",
    cex.lab=1.5,main="Q-Q plot versus U(0,1) for pd")
    segments(0,0,1,1)
    #Histogram of pd, with N(0,1) superimposed on the plot
    xh<-hist(pd,nclass=nclass,xlab="pd",main="",cex.lab=1.5)
    abline(h=ndat/nclass,lty=2,lwd=2)
    
    #residuals
    plot(xobs,pd,xlab="X",ylab="pd",cex.lab=1.5)
    abline(h=0.5,lty=2)
    abline(h=0.05,lty=3);abline(h=0.95,lty=3)
    plot(ypred,pd,xlab="Predicted Y",ylab="pd",cex.lab=1.5)
    abline(h=0.5,lty=2)
    abline(h=0.05,lty=3);abline(h=0.95,lty=3)
}

