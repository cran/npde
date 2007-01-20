`graphnpde` <-
function(namegr,xobs,npde,ypred,type.graph) 
{
    if(namegr=="") {
      if(is.na(match(type.graph,c("eps","jpeg","png","pdf")))) {
        cat("Different formats of graphs are possible :\n")
        cat("         1. Postscript (extension eps)\n")
        cat("         2. JPEG (extension jpeg)\n")
        cat("         3. PNG (extension png)\n")
        cat("         4. Acrobat PDF (extension pdf)\n")
        cok<-as.integer(readline(prompt="Which graph would you like (1-4) ? "))
        if(cok==2) type.graph<-"jpeg"
        if(cok==3) type.graph<-"png"
        if(cok==4) type.graph<-"pdf"
      }
      namfile<-readline(prompt="Name of the graph (extension will be added) : ")
      namegr<-paste(ifelse(namfile=="","output",namfile),".",type.graph,sep="")
    }
    cat("Saving graphs in file",namegr,"\n")
      if(type.graph=="eps") postscript(namegr, onefile = TRUE, print.it=FALSE,horizontal=TRUE)
      if(type.graph=="jpeg") if(capabilities("jpeg")) jpeg(namegr) else {
         cat("R was not compiled with jpeg capabilities, switching to PDF format.\n")
	 type.graph<-"pdf"
      }
      if(type.graph=="png") if(capabilities("png")) png(namegr) else {
         cat("R was not compiled with png capabilities, switching to PDF format.\n")
	 type.graph<-"pdf"
      }
      if(type.graph=="pdf") pdf(namegr, onefile = TRUE)
      plotnpde(xobs,npde,ypred)
      dev.off()
}

