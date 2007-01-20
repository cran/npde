`readobs` <-
function(namobs,iid,ix,iy,imdv) 
{
#Read files checking for the presence of a header and for separator=none,",",";"
    mysep<-""
    datobs<-read.table(namobs,na=c(".","NA"))
    x1<-unlist(strsplit(as.character(datobs[1,1]),",",fixed=TRUE))
    if(length(x1)>1) mysep<-","
    x1<-unlist(strsplit(as.character(datobs[1,1]),";",fixed=TRUE))
    if(length(x1)>1) mysep<-";"
    if(mysep!="") datobs<-read.table(namobs,na=c(".","NA"),sep=mysep)
    if(!is.numeric(datobs[1,1]))
      datobs<-read.table(namobs,na=c(".","NA"),header=TRUE,sep=mysep)
    if(!is.numeric(datobs[1,1])) {
      cat("The format of the file containing the observed data is unknown.\n")
      cat("Please use a standard R table format, with or without header,\n")
      cat("and with one of the following separators: \n")
      cat("         TAB or space(s), commas (',') or semicolons (';')\n")
      cat("Also note that a dot should be used to indicate digits in numbers.\n")
      stop("Exiting npde\n")
    }
    xobs<-datobs[,ix]
    id<-datobs[,iid]
    yobs<-datobs[,iy]
    if(imdv>0) 
      mdv<-datobs[,imdv]
    else
      mdv<-as.integer(is.na(yobs))    
    id<-id[mdv==0]
    xobs<-xobs[mdv==0]
    yobs<-yobs[mdv==0]
    ydobs<-pde<-yobs
    tabobs<-data.frame(id,xobs,yobs)
#    print(summary(tabobs))
    return(list(tabobs=tabobs,mdv=mdv))
}

