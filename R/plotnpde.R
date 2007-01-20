`plotnpde` <-
function(xobs,npde,ypred) 
{
    nclass<-10
    par(mfrow=c(2,2))
    qqnorm(sort(npde),xlab="Sample quantiles (npde)",ylab="Theoretical Quantiles",
    cex.lab=1.5,main="Q-Q plot versus N(0,1) for npde")
    qqline(sort(npde))
    #Histogram of npde, with N(0,1) superimposed on the plot
    xh<-hist(npde,nclass=nclass,xlab="npde",main="",cex.lab=1.5)
    xpl<-min(npde)+c(0:100)/100*(max(npde)-min(npde))
    ypl<-dnorm(xpl)
    ypl<-ypl/max(ypl)*max(xh$counts)
    lines(xpl,ypl,lwd=2)
    
    #residuals
    plot(xobs,npde,xlab="X",ylab="npde",cex.lab=1.5)
    abline(h=0,lty=2)
    x1<-qnorm(0.05)
    abline(h=x1,lty=3);abline(h=(-x1),lty=3)
    
    plot(ypred,npde,xlab="Predicted Y",ylab="npde",cex.lab=1.5)
    abline(h=0,lty=2)
    abline(h=x1,lty=3);abline(h=(-x1),lty=3)
}

