####################################################################################################################################
# Diagnostic plot for covariates (npde vs boxplots of covariates)
####################################################################################################################################
aux.npdeplot.boxcov <- function(obsmat, pimat, plot.opt) {

  if ("ylim" %in% names(plot.opt) & length(plot.opt$ylim)==2)
    y.limits = c(plot.opt$ylim[1],plot.opt$ylim[2])  else
      y.limits = c(min(pimat$pinf.lower),max(pimat$psup.upper))

  p<-ggplot(obsmat, aes(x=.data$grp, y=.data$y, group=factor(.data$grp, ordered=TRUE))) + 
    geom_boxplot(varwidth=plot.opt$varwidth, width=plot.opt$boxwidth) +
    theme(plot.title = element_text(hjust = 0.5, size = plot.opt$size.sub),
          axis.title.y = element_text(size = plot.opt$size.ylab),
          axis.title.x = element_text(size = plot.opt$size.xlab),
          axis.text.x = element_text(size=plot.opt$size.text.x),
          axis.text.y = element_text(size=plot.opt$size.text.y),
          axis.line.x = element_line(color=ifelse(plot.opt$xaxt==TRUE,"black","white")),
          axis.line.y = element_line(color=ifelse(plot.opt$yaxt==TRUE,"black","white")),
          panel.background=element_rect("white"),
          panel.grid.major.x = element_line(ifelse(plot.opt$grid==TRUE,"grey80","white"),linetype = plot.opt$lty.grid),
          panel.grid.minor.x = element_line(ifelse(plot.opt$grid==TRUE,"grey80","white"),linetype = plot.opt$lty.grid),
          panel.grid.major.y = element_line(ifelse(plot.opt$grid==TRUE,"grey80","white"),linetype = plot.opt$lty.grid),
          panel.grid.minor.y = element_line(ifelse(plot.opt$grid==TRUE,"grey80","white"),linetype = plot.opt$lty.grid))+
    expand_limits(y = 0) +  guides( fill = "none" ) +
    { if(plot.opt$bands) 
      geom_point(data = pimat, aes(x = .data$grp, y = .data$pmid.median), color=plot.opt$col.ther, alpha = plot.opt$alpha, size=plot.opt$size.pobs)  }+
    scale_y_continuous( plot.opt$ylab, limits = y.limits, scales::pretty_breaks(n = plot.opt$breaks.y) ) +
    scale_x_discrete( plot.opt$xlab) +
    {if (plot.opt$main!="") ggtitle(plot.opt$main)}
  # to plot in the waffle plot
#  if (plot.opt$plot.default==TRUE){
    return(p)
#  } else{
#    print(p)
#  }
}

####################################################################################################################################
# aux.npdeplot.plot renamed to aux.npdeplot.scatter
# function : plot scatterplots of y versus x with 3 PI around median and IIV quantiles (applies to vpc, x.scatter, pred.scatter)
# with covsplit, called for each covariate separately
####################################################################################################################################

aux.npdeplot.scatter <- function(obsmat, pimat, plot.opt) {

  # obsmat: matrix of Y observations to plot (Y= yobs, npde, npd, pd, tnpde, tnpd) versus X (X=independent variable (eg time), predictions (pred), covariates (cov)), with the following columns
  ### x,y: values of X and Y
  ### grp: grouping factor - used to sort groups if plot.box=TRUE
  ### cens: 1 if censored, 0 otherwise
  ### loq: value of LOQ for the observation (used in VPC plot to optionally plot the line y=LOQ)
  ### category: value of the category ("all" or "none" if not split by a covariate)
  # pimat: matrix of PI and empirical percentiles to plot for each bin, with columns
  ## grp: grouping factor (same as obsmat) - not used
  ## xcent: X-value used as center of each bin (one value of xcent per grp)
  ## category: covariate category ("all" if over all)
  ## 3 prediction intervals: pinf, pmid, psup (mid=middle, inf, sup= extreme PIs)
  ### for each PI, 3 quantiles: lower, median, upper (ie usually 0.025, 0.5, 0.975)
  ### for each PI, the empirical percentile for the observed data: obs.inf, obs.median, obs.sup
  # plot.opt: a list of graphical options used in the plot
  ## graphical options can be set by a call to set.plotoptions.default
  ## additional options used here must be added to the list:
  ### plot.opt$which.cov (the name of the covariate when xaxis="cov", or the name of the covariate when the function is called over several categories of a covariate)

  nameCovariate<-plot.opt$which.cov

  # data not censored
  plotdatapoint <- data.frame(obsmat$x[obsmat$cens == 0], obsmat$y[obsmat$cens == 0], obsmat$category[obsmat$cens == 0])
  colnames(plotdatapoint) = c("x1", "y1", "category")
  # data censored
  plotdatapoint2 <-  data.frame(obsmat$x[obsmat$cens == 1], obsmat$y[obsmat$cens == 1], obsmat$category[obsmat$cens == 1])
  colnames(plotdatapoint2) = c("x2", "y2", "category")

  # data loq # not sure we need this ?
  loq <- unique(obsmat$loq[obsmat$cens==1])
  if(length(loq)==0) loq<-NA
  if(length(loq)>1) loq<-min(loq, na.rm=TRUE)

  # take the plot options
  # plot.opt <- sapply(plot.opt, "[[", 1)

  # list to stack the ggplot
  list_plot = list()

  nameCovariate = plot.opt$which.cov    # nom de la covariable
  namesCategories = sort(unique(pimat$category ))   # catégories pour la covariable
  if(is.null(namesCategories)) namesCategories<-c("all")
  numberCategories =  length(namesCategories)  # nombre de catégories pour la covariable

  # set xlim & ylim - added a scale_free option
  if ("xlim" %in% names(plot.opt) & length(plot.opt$xlim)==2)
    x.limits = c(plot.opt$xlim[1],plot.opt$xlim[2])  else
      x.limits = c(min(obsmat$x,na.rm=TRUE),max(obsmat$x,na.rm=TRUE))
  if ("ylim" %in% names(plot.opt) & length(plot.opt$ylim)==2)
    y.limits = c(plot.opt$ylim[1],plot.opt$ylim[2])  else {
      if(plot.opt$bands) yvec<-c(pimat$pinf.lower,pimat$psup.upper) else yvec<-c()
      if(plot.opt$plot.obs) yvec<-c(yvec, plotdatapoint$y1,plotdatapoint2$y1)
      if(length(yvec)>0) y.limits = c(min(yvec),max(yvec)) else y.limits<-NULL # the plot will automatically adjust
    }
  if(plot.opt$scales %in% c("free_x", "free")) x.limits<-NULL
  if(plot.opt$scales %in% c("free_y", "free")) y.limits<-NULL

  # intersection area between ICs and curves to fill intersection area
  n_interp = 500 # nb of point for interpolation
  #if (plot.opt$plot.default == FALSE) colorYAxis = "white" else
  #colorYAxis = "black"

  # Interpolation data - loop over the covariate
  if(length(unique(pimat$xcent))<2) { # repeat 
    needinterpol<-FALSE
    pimat <- rbind(pimat, pimat)
    xc1 <- pimat$xcent[1]
    pimat$xcent <- pimat$xcent*c(0.99, 1.01)
  } else needinterpol<-TRUE

  if(plot.opt$bands & needinterpol) { # no need to compute if bands are not plotted
    plotdatainterpol1<-plotdatainterpol2<-plotdatainterpol3<-NULL
    for (iter in 1:numberCategories){
      icat = as.character(namesCategories[iter])
      if(numberCategories>1) plotdata = pimat[pimat$category==icat,] else plotdata<-pimat

      # interpolation des prediction pour filling area outliers
      interp1 <- approx(plotdata$xcent, plotdata$obs.inf,  n = n_interp)
      interp2 <- approx(plotdata$xcent, plotdata$pinf.upper, n = n_interp)
      interp3 <- approx(plotdata$xcent, plotdata$pinf.lower, n = n_interp)
      plotdatainterpol1.cov <- data.frame(interp1$x, interp1$y, interp2$x, interp2$y,interp3$x, interp3$y)
      plotdatainterpol1.cov$category<-icat
      plotdatainterpol1<-rbind(plotdatainterpol1, plotdatainterpol1.cov)

      interp1 <-  approx(plotdata$xcent, plotdata$obs.median, n = n_interp)
      interp2 <-  approx(plotdata$xcent, plotdata$pmid.upper,  n = n_interp)
      interp3 <-  approx(plotdata$xcent, plotdata$pmid.lower,  n = n_interp)
      plotdatainterpol2.cov <- data.frame(interp1$x,interp1$y,interp2$x,interp2$y,interp3$x,interp3$y)
      plotdatainterpol2.cov$category<-icat
      plotdatainterpol2<-rbind(plotdatainterpol2, plotdatainterpol2.cov)

      interp1 <- approx(plotdata$xcent, plotdata$obs.sup,  n = n_interp)
      interp2 <- approx(plotdata$xcent, plotdata$psup.upper, n = n_interp)
      interp3 <- approx(plotdata$xcent, plotdata$psup.lower, n = n_interp)
      plotdatainterpol3.cov <- data.frame(interp1$x, interp1$y,interp2$x, interp2$y,interp3$x, interp3$y)
      plotdatainterpol3.cov$category<-icat
      plotdatainterpol3<-rbind(plotdatainterpol3, plotdatainterpol3.cov)
    }
    colnames(plotdatainterpol1) = c("x_area_0.25","y_area_0.25","X0.025.1","Y0.025.1","X0.025","Y0.025", "category")
    colnames(plotdatainterpol2) = c("x_area_0.5","y_area_0.5","X0.5.1","Y0.5.1", "X0.5","Y0.5", "category")
    colnames(plotdatainterpol3) = c("x_area_0.975","y_area_0.975","X0.975.1","Y0.975.1","X0.975", "Y0.975", "category")
    plotdatainterpol1$category <- factor( plotdatainterpol1$category, levels=namesCategories)
    plotdatainterpol2$category <- factor( plotdatainterpol2$category, levels=namesCategories)
    plotdatainterpol3$category <- factor( plotdatainterpol3$category, levels=namesCategories)
  }

    if (plot.opt$plot.box==TRUE) {

      box.plot.width = 25
      x.limits = x.limits + c( -box.plot.width , box.plot.width  )*0.5

      p <- ggplot( obsmat, aes( x=.data$x, y=.data$y, fill = factor( .data$grp, ordered=TRUE) ) ) +
        # Title and layout
        theme(plot.title = element_text(hjust = 0.5, size = plot.opt$size.sub),
              axis.title.y = element_text(size = plot.opt$size.ylab),
              axis.title.x = element_text(size = plot.opt$size.xlab),
              axis.text.x = element_text(size=plot.opt$size.text.x),
              axis.text.y = element_text(size=plot.opt$size.text.y),
              axis.line.x = element_line(color=ifelse(plot.opt$xaxt==TRUE,"black","white")),
              axis.line.y = element_line(color=ifelse(plot.opt$yaxt==TRUE,"black","white")),
              panel.background=element_rect("white"),
              panel.grid.major.x = element_line(ifelse(plot.opt$grid==TRUE,"grey80","white"),linetype = plot.opt$lty.grid),
              panel.grid.minor.x = element_line(ifelse(plot.opt$grid==TRUE,"grey80","white"),linetype = plot.opt$lty.grid),
              panel.grid.major.y = element_line(ifelse(plot.opt$grid==TRUE,"grey80","white"),linetype = plot.opt$lty.grid),
              panel.grid.minor.y = element_line(ifelse(plot.opt$grid==TRUE,"grey80","white"),linetype = plot.opt$lty.grid))+
        expand_limits(y = 0) +  # Eco=>Romain: not sure we want this !!! we need some kind of test (some responses may be very far from 0!!!)
        guides( fill = "none" ) +
        
        # Model predicted and observed percentiles
        geom_line(data = pimat, mapping = aes(x = .data$xcent, y = .data$obs.median), inherit.aes = FALSE,
                  linetype = plot.opt$lty.lobs, colour = plot.opt$col.lobs, linewidth = plot.opt$lwd.lobs) +
        geom_line(data = pimat, mapping = aes(x = .data$xcent, y = .data$obs.sup), inherit.aes = FALSE,
                  linetype = plot.opt$lty.lobs, colour = plot.opt$col.lobs, linewidth = plot.opt$lwd.lobs) +
        geom_line(data = pimat, mapping = aes(x = .data$xcent, y = .data$obs.inf), inherit.aes = FALSE,
                  linetype = plot.opt$lty.lobs, colour = plot.opt$col.lobs, linewidth = plot.opt$lwd.lobs) +
        
        geom_line(data = pimat, mapping = aes(x = .data$xcent, y = .data$pinf.median), inherit.aes = FALSE,
                  linetype = plot.opt$lty.ther, colour = plot.opt$col.ther, alpha = plot.opt$alpha.ther, linewidth = plot.opt$lwd.ther) +
        geom_line(data = pimat, mapping = aes(x = .data$xcent, y = .data$pmid.median), inherit.aes = FALSE,
                  linetype = plot.opt$lty.ther, colour = plot.opt$col.ther, alpha = plot.opt$alpha.ther, linewidth = plot.opt$lwd.ther) +
        geom_line(data = pimat, mapping = aes(x = .data$xcent, y = .data$psup.median), inherit.aes = FALSE,
                  linetype = plot.opt$lty.ther, colour = plot.opt$col.ther, alpha = plot.opt$alpha.ther, linewidth = plot.opt$lwd.ther) +
        
        # Prediction bands
        { if ( plot.opt$bands == TRUE )
          geom_ribbon(data = pimat, mapping = aes(x = .data$xcent, y = .data$pmid.lower, ymin = .data$pmid.lower, ymax = .data$pmid.upper),
                      fill = plot.opt$fill.med, alpha = plot.opt$alpha.med)}  +
        { if ( plot.opt$bands == TRUE )
          geom_ribbon(data = pimat, mapping = aes(x = .data$xcent, y = .data$pinf.lower, ymin = .data$pinf.lower, ymax = .data$pinf.upper),
                      fill = plot.opt$fill.bands, alpha = plot.opt$alpha.bands)}  +
        { if ( plot.opt$bands == TRUE )
          geom_ribbon(data = pimat, mapping = aes(x = .data$xcent, y = .data$psup.lower, ymin = .data$psup.lower, ymax = .data$psup.upper),
                      fill = plot.opt$fill.bands, alpha = plot.opt$alpha.bands)}  +
        # Boundaries for prediction bands
        { if ( plot.opt$bands == TRUE )
        geom_line(data = pimat, mapping = aes(x = .data$xcent, y = .data$pinf.lower), inherit.aes = FALSE,
                  linetype = plot.opt$lty.bands, colour = plot.opt$col.bands, linewidth = plot.opt$lwd.band) }+
        { if ( plot.opt$bands == TRUE )
        geom_line(data = pimat, mapping = aes(x = .data$xcent, y = .data$pinf.upper), inherit.aes = FALSE,
                  linetype = plot.opt$lty.bands, colour = plot.opt$col.bands, linewidth = plot.opt$lwd.band)}+
        { if ( plot.opt$bands == TRUE )
        geom_line(data = pimat, mapping = aes(x = .data$xcent, y = .data$pmid.lower), inherit.aes = FALSE,
                  linetype = plot.opt$lty.med, colour = plot.opt$col.med, linewidth = plot.opt$lwd.med)}+
        { if ( plot.opt$bands == TRUE )
        geom_line(data = pimat, mapping = aes(x = .data$xcent, y = .data$pmid.upper), inherit.aes = FALSE,
                  linetype = plot.opt$lty.med, colour = plot.opt$col.med, linewidth = plot.opt$lwd.med)}+
        { if ( plot.opt$bands == TRUE )
        geom_line(data = pimat, mapping = aes(x = .data$xcent, y = .data$psup.lower), inherit.aes = FALSE,
                  linetype = plot.opt$lty.bands, colour = plot.opt$col.bands, linewidth = plot.opt$lwd.band)}+
        { if ( plot.opt$bands == TRUE )
        geom_line(data = pimat, mapping = aes(x = .data$xcent, y = .data$psup.upper), inherit.aes = FALSE,
                  linetype = plot.opt$lty.bands, colour = plot.opt$col.bands, linewidth = plot.opt$lwd.band)}+

        # plot of data as boxplot
        geom_boxplot(data=obsmat,
                     aes( x=.data$x, y=.data$y,  fill= factor( .data$grp, ordered=TRUE)), varwidth=plot.opt$varwidth, width=plot.opt$boxwidth)+

        scale_fill_manual( values = rep( plot.opt$col.pobs,
                                         length(unique(obsmat$grp)) ) ) +

        scale_y_continuous( plot.opt$ylab, limits = y.limits, scales::pretty_breaks(n = plot.opt$breaks.y) ) +

        scale_x_continuous( plot.opt$xlab, limits = x.limits,scales::pretty_breaks(n = plot.opt$breaks.x ) ) +

        # facet wrap over covariate categories
        facet_wrap(.~factor(.data$category, levels=namesCategories), nrow=1, scales = plot.opt$scales) +

        {if(numberCategories==1)
          theme(strip.background = element_blank(), strip.text.x = element_blank())
        } +
        {if (plot.opt$main!="") ggtitle(plot.opt$main)}

     # list_plot[[1]] <- p

    } else { # else plot.box=FALSE

      # ggplot template
      p <- ggplot(pimat, aes(x = .data$xcent)) +

        # theme of the ggplot template
        theme(plot.title = element_text(hjust = 0.5, size = plot.opt$size.sub),
              axis.title.x = element_text(size = plot.opt$size.xlab),
              axis.title.y = element_text(size = plot.opt$size.ylab),
              axis.text.x = element_text(size=plot.opt$size.text.x),
              axis.text.y = element_text(size=plot.opt$size.text.y),
              axis.line.x = element_line(color=ifelse(plot.opt$xaxt==TRUE,"black","white")),
              axis.line.y = element_line(color=ifelse(plot.opt$yaxt==TRUE,"black","white")),
              panel.background=element_rect("white"),
              panel.grid.major.x = element_line(ifelse(plot.opt$grid==TRUE,"grey80","white"),linetype = plot.opt$lty.grid),
              panel.grid.minor.x = element_line(ifelse(plot.opt$grid==TRUE,"grey80","white"),linetype = plot.opt$lty.grid),
              panel.grid.major.y = element_line(ifelse(plot.opt$grid==TRUE,"grey80","white"),linetype = plot.opt$lty.grid),
              panel.grid.minor.y = element_line(ifelse(plot.opt$grid==TRUE,"grey80","white"),linetype = plot.opt$lty.grid))  +

        # coordinates x-y
        coord_cartesian(xlim=x.limits, ylim=y.limits) +
        expand_limits(x = 0, y=0) + # Eco=>Romain: not sure we want this !!! we need some kind of test

        # bands
        { if ( plot.opt$bands == TRUE )
          geom_ribbon(aes(ymin = .data$pinf.lower, ymax = .data$pinf.upper), fill = plot.opt$fill.bands, alpha = plot.opt$alpha.bands) } +
        { if ( plot.opt$bands == TRUE )
          geom_ribbon(aes(ymin = .data$pmid.lower,   ymax = .data$pmid.upper),   fill = plot.opt$fill.med, alpha = plot.opt$alpha.med) } +
        { if ( plot.opt$bands == TRUE )
          geom_ribbon(aes(ymin = .data$psup.lower, ymax = .data$psup.upper), fill = plot.opt$fill.bands, alpha = plot.opt$alpha.bands) } +

        # fill intersection area as outliers
        { if ( plot.opt$bands == TRUE  & needinterpol)
          geom_ribbon(plotdatainterpol1,
                      mapping = aes(x = .data$x_area_0.25, ymin = .data$y_area_0.25, ymax = pmin(.data$Y0.025.1, .data$y_area_0.25)),
                      fill = plot.opt$fill.outliers.bands, alpha = plot.opt$alpha.outliers.bands) } +

        { if ( plot.opt$bands == TRUE  & needinterpol)
          geom_ribbon(plotdatainterpol1,
                      mapping = aes(x = .data$x_area_0.25, ymin = .data$y_area_0.25, ymax = pmax(.data$Y0.025, .data$y_area_0.25)),
                      fill = plot.opt$fill.outliers.bands, alpha = plot.opt$alpha.outliers.bands) } +

        { if ( plot.opt$bands == TRUE  & needinterpol)
          geom_ribbon(plotdatainterpol2,
                      mapping = aes(x = .data$x_area_0.5, ymin = .data$y_area_0.5,ymax = pmin(.data$Y0.5.1, .data$y_area_0.5)),
                      fill = plot.opt$fill.outliers.med, alpha = plot.opt$alpha.outliers.med) } +

        { if ( plot.opt$bands == TRUE  & needinterpol)
          geom_ribbon(plotdatainterpol2,
                      mapping = aes(x = .data$x_area_0.5,ymin = .data$y_area_0.5,ymax = pmax(.data$Y0.5, .data$y_area_0.5)),
                      fill = plot.opt$fill.outliers.med, alpha = plot.opt$alpha.outliers.med) } +

        { if ( plot.opt$bands == TRUE  & needinterpol)
          geom_ribbon(plotdatainterpol3,
                      mapping = aes(x = .data$x_area_0.975, ymin = .data$y_area_0.975, ymax = pmin(.data$Y0.975.1, .data$y_area_0.975)),
                      fill = plot.opt$fill.outliers.bands, alpha = plot.opt$alpha.outliers.bands) } +

        { if ( plot.opt$bands == TRUE  & needinterpol)
          geom_ribbon(plotdatainterpol3,
                      mapping = aes(x = .data$x_area_0.975,ymin = .data$y_area_0.975,ymax = pmax(.data$Y0.975, .data$y_area_0.975)),
                      fill = plot.opt$fill.outliers.bands, alpha = plot.opt$alpha.outliers.bands) } +

        # plot observed and model predicted percentiles
        #{if (plot.opt$type=="l" || plot.opt$type=="b")
        geom_line(aes(y = .data$obs.inf), linetype = plot.opt$lty.lobs,colour = plot.opt$col.lobs,linewidth = plot.opt$lwd.lobs)+
        #  {if (plot.opt$type=="l" || plot.opt$type=="b")
        geom_line(aes(y = .data$obs.median),  linetype = plot.opt$lty.lobs,colour = plot.opt$col.lobs,linewidth = plot.opt$lwd.lobs)+
        # {if (plot.opt$type=="l" || plot.opt$type=="b")
        geom_line(aes(y = .data$obs.sup),  linetype = plot.opt$lty.lobs,colour = plot.opt$col.lobs,linewidth = plot.opt$lwd.lobs)+
        geom_line(pimat, mapping = aes(y = .data$pinf.median), linetype = plot.opt$lty.ther, colour = plot.opt$col.ther, alpha = plot.opt$alpha.ther, linewidth = plot.opt$lwd.ther)+
        geom_line(pimat, mapping = aes(y = .data$pmid.median), linetype = plot.opt$lty.ther, colour = plot.opt$col.ther, alpha = plot.opt$alpha.ther, linewidth = plot.opt$lwd.ther)+
        geom_line(pimat, mapping = aes(y = .data$psup.median), linetype = plot.opt$lty.ther, colour = plot.opt$col.ther, alpha = plot.opt$alpha.ther, linewidth = plot.opt$lwd.ther)+
        
        # Prediction bands
        #{if (plot.opt$type=="l" || plot.opt$type=="b")
        { if ( plot.opt$bands == TRUE ) geom_line(aes(y = .data$pinf.lower), linetype = plot.opt$lty.bands,colour = plot.opt$col.bands,linewidth = plot.opt$lwd.band) } +#} +
        #{if (plot.opt$type=="l" || plot.opt$type=="b")
        { if ( plot.opt$bands == TRUE ) geom_line(aes(y = .data$pinf.upper), linetype = plot.opt$lty.bands,colour = plot.opt$col.bands,linewidth = plot.opt$lwd.band)} +#} +
        #{if (plot.opt$type=="l" || plot.opt$type=="b")
        { if ( plot.opt$bands == TRUE ) geom_line(aes(y = .data$pmid.lower), linetype = plot.opt$lty.med,colour = plot.opt$col.med,linewidth = plot.opt$lwd.med)} +#} +
        #{if (plot.opt$type=="l" || plot.opt$type=="b")
        { if ( plot.opt$bands == TRUE ) geom_line(aes(y = .data$pmid.upper), linetype = plot.opt$lty.med,colour = plot.opt$col.med,linewidth = plot.opt$lwd.med)} +#} +
        #{if (plot.opt$type=="l" || plot.opt$type=="b")
        { if ( plot.opt$bands == TRUE ) geom_line(aes(y = .data$psup.lower), linetype = plot.opt$lty.bands,colour = plot.opt$col.bands,linewidth = plot.opt$lwd.band) } +#} +
        #{if (plot.opt$type=="l" || plot.opt$type=="b")
        { if ( plot.opt$bands == TRUE ) geom_line(aes(y = .data$psup.upper), linetype = plot.opt$lty.bands,colour = plot.opt$col.bands,linewidth = plot.opt$lwd.band)} +#} +

        # plot loq
        { if (plot.opt$line.loq & !is.na(loq)) geom_line(aes(y = loq),  
                colour = plot.opt$col.line.loq,  linewidth = plot.opt$lwd.line.loq, linetype = plot.opt$lty.line.loq)} +
        
        # plot non censored data
        { if ( plot.opt$plot.obs == TRUE )
          geom_point( plotdatapoint, mapping = aes( x = .data$x1, y = .data$y1 ),
                color = plot.opt$col.pobs, shape = plot.opt$pch.pobs, size = plot.opt$size.pobs ) } +
        # plot censored data
        { if ( plot.opt$plot.obs == TRUE & dim(plotdatapoint2)[1]>0)
          geom_point( plotdatapoint2, mapping = aes( x = .data$x2, y = .data$y2 ),
                      color = plot.opt$col.pcens, shape =  plot.opt$pch.pcens, size = plot.opt$size.pcens ) } +

        # x-y log-scales
        { if (plot.opt$xlog == FALSE)  scale_x_continuous(plot.opt$xlab, scales::pretty_breaks(n = plot.opt$breaks.x))
        } +
        { if (plot.opt$ylog == FALSE)  scale_y_continuous(plot.opt$ylab, scales::pretty_breaks(n = plot.opt$breaks.y))
        } +
        { if (plot.opt$xlog == TRUE)
          scale_x_log10(plot.opt$xlab,breaks = scales::trans_breaks("log10", function(x) 10 ^ x),
                        labels = scales::trans_format("log10", scales::math_format(10 ^ .x)))
        } +
        { if (plot.opt$ylog == TRUE)
          scale_y_log10(plot.opt$ylab, breaks = scales::trans_breaks("log10", function(x) 10 ^ x),
                        labels = scales::trans_format("log10", scales::math_format(10 ^ .x)))
        } +
        # if log scales plot logticks
        { if (plot.opt$xlog == TRUE) annotation_logticks(sides = "b")} +
        { if (plot.opt$ylog == TRUE) annotation_logticks(sides = "l")} +

        {if (plot.opt$main!="") ggtitle(plot.opt$main)} +

        # facet wrap over covariate categories
        facet_wrap(.~factor(.data$category, levels=namesCategories), nrow=1, scales=plot.opt$scales) +

        {if(numberCategories==1)
          theme(strip.background = element_blank(), strip.text.x = element_blank())
        }
    #  list_plot[[1]] <- p

    }

  # to plot in the waffle plot
  #if (plot.opt$plot.default==TRUE){
   # print(p)
    return(p)
  #} else{
   # print(p)

  #}
} #END FUNCTION

####################################################################################################################################
