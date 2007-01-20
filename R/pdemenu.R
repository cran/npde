`pdemenu` <-
function()
{
    ick<-0
    while(ick==0) {
      namobs<-readline(prompt="Name of the file containing the observed data : ")
      datobs<-try(read.table(namobs,na=c(".","NA"),nrows=10))
      if(class(datobs)!="try-error") ick<-1 else 
         cat("\n      File",namobs,"does not exist.\n")
    }

    iid<-1;ix<-2;iy<-3;imdv<-0
    cat("I'm assuming file",namobs,"has the following structure :\n")
    cat("        ID X Y ...\n")
    cat("and does not contain a column signaling missing data.\n")
    cok<-readline(prompt="To keep, press ENTER, to change, type any letter : ")
    if(cok=="") 
      cat("Keeping this structure\n") 
    else {
      iid<-as.integer(readline(prompt="         Column with ID information ? "))
      ix<-as.integer(readline(prompt="         Column with X (eg time) information ? "))
      iy<-as.integer(readline(prompt="         Column with Y (eg DV) information ? "))
      imdv<-as.integer(readline(prompt="         Column signaling missing data (eg MDV, press ENTER if none) ? "))
      if(is.na(imdv)) imdv<-0
    } 
    ick<-0
    while(ick==0) {
      namsim<-readline(prompt="Name of the file containing the simulated data : ")
      datobs<-try(read.table(namsim,na=c(".","NA"),nrows=10))
      if(class(datobs)!="try-error") ick<-1 else 
         cat("\n      File",namsim,"does not exist.\n")
    }
    cok<-readline(prompt="Do you want results and graphs to be saved to files (y/Y) [default=yes] ? ")
    if(cok=="y"|cok=="Y"|cok=="yes"|cok=="") boolsave<-TRUE else boolsave<-FALSE
    type.graph<-"eps";namegr<-nameres<-""
    if(boolsave) {
      cat("Different formats of graphs are possible :\n")
      cat("         1. Postscript (extension eps)\n")
      cat("         2. JPEG (extension jpeg)\n")
      cat("         3. PNG (extension png)\n")
      cat("         4. Acrobat PDF (extension pdf)\n")
      cok<-as.integer(readline(prompt="Which format would you like for the graph (1-4) ? "))
      if(cok==2) type.graph<-"jpeg"
      if(cok==3) type.graph<-"png"
      if(cok==4) type.graph<-"pdf"
      namfile<-readline(prompt="Name of the file (extension will be added, default=output):")
      namegr<-paste(ifelse(namfile=="","output",namfile),".",type.graph,sep="")
      nameres<-paste(ifelse(namfile=="","output",namfile),".npde",sep="")
    }
    ick<-0
    while(ick==0) {
      cok<-readline(prompt="Do you want to compute npde (y/Y) [default=yes] ? ")
      if(cok=="y"|cok=="Y"|cok=="yes"|cok=="") {calc.npde<-TRUE;ick<-1} else 
         calc.npde<-FALSE
      cok<-readline(prompt="Do you want to compute pd (y/Y) [default=no] ? ")
      if(cok=="y"|cok=="Y"|cok=="yes") {calc.pd<-TRUE;ick<-1} else 
         calc.pd<-FALSE
      if(ick==0) cat("\n Please choose to compute at least one of npde or pd.\n") 
    }
    verbose<-FALSE
    if(calc.npde) {
    cok<-readline(prompt="Do you want a message printed as the computation of npde begins in a new subject (y/Y) [default=no] ? ")
    if(cok=="y"|cok=="Y"|cok=="yes") verbose<-TRUE 
    }
    cok<-readline(prompt="Do you want the function to return an object (y/Y) [default=yes] ? ")
    if(cok=="n"|cok=="N"|cok=="no") output<-FALSE else output<-TRUE
    return(list(namobs,namsim,iid,ix,iy,imdv,boolsave,type.graph,namegr,calc.pd,
    calc.npde,nameres,output,verbose))
}

