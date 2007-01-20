`kurtosis` <-
function (x) 
{
#from Snedecor and Cochran, p 80
    x<-x[!is.na(x)]
    m4<-sum((x - mean(x))^4)
    m2<-sum((x - mean(x))^2)
    kurt<-m4*length(x)/(m2**2)-3
    return(kurtosis=kurt)
}

