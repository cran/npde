`skewness` <-
function (x) 
{
#from Snedecor and Cochran, p 79
    x<-x[!is.na(x)]
    m3<-sum((x - mean(x))^3)
    m2<-sum((x - mean(x))^2)
    skew<-m3/(m2*sqrt(m2/length(x)))
    return(skewness=skew)
}

