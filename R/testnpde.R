`testnpde` <-
function(npde) 
{
    cat("---------------------------------------------\n")
    cat("Distribution of npde:\n")
    sev<-var(npde)*sqrt(2/(length(npde)-1))
    sem<-sd(npde)/sqrt(length(npde))
    cat("           mean=",format(mean(npde),digits=4),"  (SE=",format(sem,digits=2),")\n")
    cat("       variance=",format(var(npde),digits=4),"  (SE=",format(sev,digits=2),")\n")
    cat("       skewness=",format(skewness(npde),digits=4),"\n")
    cat("       kurtosis=",format(kurtosis(npde),digits=4),"\n")
    cat("---------------------------------------------\n\n")
    myres<-rep(0,4)
    y<-wilcox.test(npde)
    myres[1]<-y$p.val
    y<-shapiro.test(npde)
    myres[3]<-y$p.val

    # test de variance pour 1 échantillon
    # chi=s2*(n-1)/sigma0 et test de H0={s=sigma0} vs chi2 à n-1 df
    semp<-sd(npde)
    n1<-length(npde)
    chi<-(semp**2)*(n1-1)
    y<-2*min(pchisq(chi,n1-1),1-pchisq(chi,n1-1))
    myres[2]<-y
    xcal<-3*min(myres[1:3])
    myres[4]<-min(1,xcal)
    names(myres)<-c("  Wilcoxon signed rank test ","  Fisher variance test      ",
    "  SW test of normality      ","Global adjusted p-value     ")
    cat("Statistical tests\n")
    for(i in 1:4) {
      cat(names(myres)[i],": ")
      #if (myres[i]<1) 
      cat(format(myres[i],digits=3)) 
      #else cat(myres[i])
      if(as.numeric(myres[i])<0.1 & as.numeric(myres[i])>=0.05) cat(" .")
      if(as.numeric(myres[i])<0.05) cat(" *")
      if(as.numeric(myres[i])<0.01) cat("*")
      if(as.numeric(myres[i])<0.001) cat("*")
      cat("\n")
    }
      cat("---\n")
      cat("Signif. codes: '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 \n")
    cat("---------------------------------------------\n")
    return(myres)
}

