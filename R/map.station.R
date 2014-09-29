## Author 	 Rasmus E. Bnestad
## Updated 	 by Abdelkader Mezghani
## Last update   26.07.2013
## Includes	 map.station() ; test.map.station()
## Require 	 geoborders.rda

## Perform a series of tests that produce and save maps of the different data sources into the local directory.
test.map.station <- function(save=FALSE) {

map.station(src="NACD",col="darkgreen",bg="green")
if (save) dev.copy2pdf(file="NACD-Network.pdf")
if (save) dev.off()
map.station(src="ECAD",col="darkgreen",bg="green")
if (save) dev.copy2pdf(file="ECAD-Network.pdf")
if (save) dev.off()
map.station(src="GHCNM",col="darkgreen",bg="green")
if (save) dev.copy2pdf(file="GHCNM-Network.pdf")
if (save) dev.off()
map.station(src="GHCND",col="darkgreen",bg="green")
if (save) dev.copy2pdf(file="GHCND-Network.pdf")
if (save) dev.off()
map.station(src="NORDKLIM",col="darkgreen",bg="green")
if (save) dev.copy2pdf(file="NORDKLIM-Network.pdf")
if (save) dev.off()
map.station(src="NARP",col="darkgreen",bg="green")
if (save) dev.copy2pdf(file="NARP-Network.pdf")
if (save) dev.off()

locs <- map.station(ele=101)
locs <- map.station(cntr="NORWAY",ele=101,src="ECAD")
}


## The main function to produce map of subseted stations
map.stationmeta <- function(...)
  map.station(...)

map.station <- function (x = NULL,col = "darkgreen",bg="green",cex=1, zexpr = "alt",
                         subset=list(x=NULL,stid = NULL, param = NULL, lon = NULL, lat = NULL, 
                             alt = NULL, cntr = NULL, src = NULL, nmin = NULL , it = NULL),
                         col.subset="darkred",bg.subset="red",cex.subset=1,add.text.subset=FALSE,
                         colbar=list(breaks=NULL,n.breaks=10,type="p",cex=2,h=0.6,cex.lab=0.6,v=1),
                         showall = FALSE, verbose = FALSE , add.text=FALSE,
                         height=NULL,width=NULL,cex.axis=1,pch=21,
                         FUN=NULL,from=NULL,to=NULL,showaxis=FALSE,xlim = NULL, ylim = NULL,border=FALSE,
                         full.names=FALSE,full.names.subset=FALSE,new=TRUE,text=FALSE,
                         fancy=FALSE,gridlines=TRUE,...) 
{ 
    ## library(fields)
    ## Extract colbar parameters
    if (!is.null(colbar))
        cex.lab <- colbar$label

    ## setting default values for the color bar if not specified
    if (!is.null(FUN)) {
        if (is.null(colbar$type)) colbar$type <- "p"  
        if (is.null(colbar$cex)) colbar$cex <- 2
        if (is.null(colbar$h)) colbar$h <- 0.6
        if (is.null(colbar$v)) colbar$v <- 1
        if (is.null(colbar$cex.lab)) colbar$cex.lab <- 0.6
        if (is.null(colbar$n.breaks)) colbar$n.breaks <- 20
    }
    
    
    ##load("~/esd/data/geoborders.rda")
    data("geoborders", envir = environment())
    ##load("station.meta.rda") # data("station.meta", envir = environment())
    ##load("t2m.NORDKLIM.rda") # data("t2m.NORDKLIM", envir = environment())
    ##data("precip.NORDKLIM", envir = environment())
    ##browser()
    if (zexpr == "alt") 
        zexpr <- "sqrt( station.meta$alt/max(station.meta$alt,na.rm=TRUE) )"
    n <- 100
   
    ## if (!is.null(subset)) {col = "white" ; bg="white"} 
       
    ## if (!is.null(unlist(subset)) & !showall) {col = "white" ; bg="white"} 

    ## browser()
    if (!is.null(x)) { 
        if (inherits(x,"stationmeta")) {      
            ss <- x
            ss$variable <-apply(as.matrix(ss$element),1,esd2ele)
        }
        else if (inherits(x,"station")) {
            if (is.null(dim(x))) dim(x) <- c(length(x),1)
            ss <- list(station_id=attr(x,"station_id"),location=attr(x,'location'),country=attr(x,'country'),longitude=attr(x,"longitude"),latitude=attr(x,'latitude'),altitude=attr(x,'altitude'),variable=attr(x,"variable"),longname=attr(x,"longname"),start=rep(start(x),dim(x)[2]),end=rep(end(x),dim(x)[2]),source=attr(x,"source"))
        }
    } else
        ss <- select.station() 
    ## browser()
    if (is.null(attr(ss,"element")))
        ss$element <-apply(as.matrix(ss$variable),1,esd2ele)   

    if (!is.null(unlist(subset))) { ## highlight a subset of station
        
        if (is.null(subset$x)) {
            highlight <- select.station(x=subset$x,loc=subset$loc,stid = subset$stid, param = subset$param, lon = subset$lon, lat = subset$lat, alt = subset$alt, cntr = subset$cntr, src = subset$src, nmin = subset$nmin , it = subset$it)
            highlight$variable <-apply(as.matrix(highlight$element),1,esd2ele)
        } else if (inherits(subset$x,"station")) {
            if (is.null(dim(subset$x)))
                dim(subset$x) <- c(length(subset$x),1)
            highlight <- list(station_id=attr(subset$x,"station_id"),location=attr(subset$x,'location'),country=attr(subset$x,'country'),longitude=attr(subset$x,"longitude"),latitude=attr(subset$x,'latitude'),altitude=attr(subset$x,'altitude'),variable=attr(subset$x,"variable"),longname=attr(subset$x,"longname"), start=rep(start(subset$x),dim(subset$x)[2]),end=rep(end(subset$x),dim(subset$x)[2]),source=attr(subset$x,"source"))
        }
        highlight$element <- apply(as.matrix(highlight$variable),1,esd2ele)
    }  else highlight <-  NULL
    
    ## if (is.null(x) & is.null(subset)) ## Use select.station()
    ##    ss <- select.station(stid = subset$stid, param = subset$param, lon = subset$lon, lat = subset$lat, alt = subset$alt, cntr = subset$cntr, src = subset$src , nmin = subset$nmin , it = subset$it , verbose = verbose)
    
    ## Set negative altitude to NA
    ss$altitude[ss$altitude < 0] <- NA
    
    tte <- "rwb"
    
    cols <- rgb(seq(0, 0.5, length = 100)^2, seq(0.5, 1, length = 100), 
                seq(0, 0.5, length = 100)^2)

    ## if (new) dev.new(height=height,width=width)
                                        #par(bty = "n", xaxt = "n", yaxt = "n", xpd = FALSE)

    ## Select a subdomain in the x-axis
    if (is.null(xlim))
        if (is.null(highlight) | showall)
            if (length(subset$lon) > 1)
                xlim <- floor(range(ss$longitude, na.rm = TRUE))
            else
                xlim <-floor(range(ss$longitude, na.rm = TRUE) + c(-5,5))  # +/- 5 degrees
        else
            if (length(subset$lon) > 1)
                xlim <- floor(range(highlight$longitude, na.rm = TRUE))
            else
                xlim <-floor(range(highlight$longitude, na.rm = TRUE) + c(-5,5))  # +/- 5 degrees
    ## Select a subdomain in the y-axis
    if (is.null(ylim))
        if (is.null(highlight) | showall)
            if (length(subset$lat) > 1)
                ylim <- floor(range(ss$latitude, na.rm = TRUE))
            else
                ylim <- floor(range(ss$latitude, na.rm = TRUE) + c(-5,5))
        else
            if (length(subset$lat) > 1)
                ylim <- floor(range(highlight$latitude, na.rm = TRUE))
            else
                ylim <-floor(range(highlight$latitude, na.rm = TRUE) + c(-5,5))  # +/- 5 degrees

    if (!is.null(highlight))
        plot(highlight$longitude, highlight$latitude, pch = pch, col = col, bg = bg.all,cex = cex, xlab = "", ylab = "", xlim = xlim, ylim = ylim , axes =FALSE , frame.plot = FALSE)
    else if (!is.null(ss))
        plot(ss$longitude, ss$latitude, pch = pch, col = col, bg = bg,cex = cex, xlab = "", ylab = "", xlim = xlim, ylim = ylim , axes = FALSE , frame.plot = FALSE)

    ## browser()
    if (showall) {
        ss.all <- select.station(param=subset$param)
        points(ss.all$longitude,ss.all$latitude,pch=pch,col="grey50",bg="grey",cex=cex/2)
    }
    
    lines(geoborders$x, geoborders$y, col = "black")
    lines(attr(geoborders, "borders")$x, attr(geoborders, "borders")$y, col = "grey90")

    
  ## add title
    
    ## add search info to plot
    
    ## browser()   
    if (text) {
        if (!is.null(highlight)) {
            title(main=paste("SOURCE(S) : ", paste(levels(factor(highlight$source)),collapse="/" )),line=3,cex.main=.8)
            title(main=paste(length(levels(factor(highlight$location)))),line=2,cex.main=.8)
            title(main=paste(min(highlight$start,na.rm=TRUE),"/",max(highlight$end,na.rm=TRUE)),line=2,cex.main=.8,adj=1)
            if (!is.null(FUN))
                title(main=paste(paste(toupper(apply(as.matrix(levels(factor(highlight$variable))),1,esd2ele)),collapse="/"),toupper(FUN),sep="/"),line=2,cex.main=.8 , adj = 0)
            else
                title(main=paste(toupper(apply(as.matrix(levels(factor(highlight$variable))),1,esd2ele)),collapse="/"),line=2,cex.main=.8 , adj = 0)
        }
        else {
            title(main=paste("SOURCE(S) : ", paste(levels(factor(ss$source)),collapse="/" )),line=3,cex.main=.8)
            title(main=paste(length(ss$location)),line=2,cex.main=.8)
            title(main=paste(max(ss$start,na.rm=TRUE),"/",min(ss$end,na.rm=TRUE)),line=2,cex.main=.8,adj=1)
            title(main=paste(paste(toupper(levels(factor(ss$variable))),collapse="/"),toupper(FUN),sep="/"),line=2,cex.main=.8 , adj = 0)
        }
    }
    ## title(main=attr(z,"title"),line=2.2,cex.main=0.7)
    ## add margin text
    if (text) mtext(paste(("ESD package - map.station() - MET Norway 2014"),"(www.met.no)",sep=" "),side=1,line=4,cex=0.6)
    
    ## add grid
    grid()
    
    ## insert color bar
    if (TRUE) {  
        if (!is.null(FUN)) {
          if (is.element(FUN,c('lon','lat','alt')))
              eval(parse(text=paste('y <-',FUN,'(x)',sep="")))
          else {
              if (is.element("na.rm",names(formals(FUN))) | is.element("...",names(formals(FUN))) | (is.element(FUN,c("max","min"))))
                  y <- apply(coredata(x),2,FUN=FUN,na.rm=TRUE)
              else if (FUN=="trend")
                  y <- apply(coredata(annual(x)),2,FUN=FUN,ns.omit=FALSE)[1,]
              else
                  y <- apply(coredata(x),2,FUN=FUN) ## ,na.rm=TRUE)
          }
          ## browser()
          y.rng <- floor(range(y,na.rm=TRUE))
          
          if (is.null(colbar$col) & is.null(colbar$breaks)) {
              colbar$breaks <- pretty(y,n=colbar$n.breaks)
              COL <- colscal(n=colbar$n.breaks,col="bwr",test=FALSE) #rev(rainbow(n=length(colbar$breaks)))
              colbar$n.breaks <- length(COL)
          }
          if (!is.null(colbar$breaks)) {
              ## stopifnot(length(colbar$n.breaks)==length(colbar$breaks))
               COL <- colscal(n=length(colbar$breaks),col="bwr",test=FALSE) #COL <- rev(rainbow(n=length(colbar$breaks)))
              colbar$n.breaks <- length(colbar$breaks)
          }
          if (!is.null(colbar$breaks) & is.null(colbar$col)) {
              colbar$n.breaks <- length(colbar$breaks)
               COL <- colscal(n=length(colbar$breaks),col="bwr",test=FALSE) # COL <- rev(rainbow(n=length(colbar$breaks)))
          } else if (!is.null(colbar$col)) {
              COL <- colbar$col
              colbar$n.breaks <- length(COL)
              colbar$breaks <- pretty(y,colbar$n.breaks) ## seq(y.rng[1],y.rng[2],by=diff(y.rng)/colbar$n.breaks)                        
          }
          
          if (fancy==TRUE)
              col.bar(breaks=colbar$breaks,col=COL,pch=pch,type=colbar$type,v=colbar$v,h=colbar$h,cex=colbar$cex,cex.lab=colbar$cex.lab,border=border,...)
          else 
              image.plot(horizontal=TRUE,legend.only=T,zlim=range(y),col=COL,legend.width=1,axis.args=list(cex.axis=0.8),border=FALSE,add=TRUE,graphics.reset=TRUE)

          if (is.null(dim(x))) ns <- 1 else ns <- dim(x)[2]
          col <- rep(NA,ns)
          if (ns==1)
              k <- 1
          else {
              icol <- apply(as.matrix(y),2,findInterval,colbar$breaks)
              col <- "black"
              bg <- COL[icol]
          }
      } else {
          col=col;bg=bg     
      }
    }
    ## 
    ##scale <- apply(y,2,function(x) sum(!is.na(x))/length(x))
    if (!inherits(x,"stationmeta") & !is.null(attr(x,'na')))
        scale <- attr(x,'na')
    else
        scale <- 1
    if (is.null(highlight) | showall) 
        points(ss$longitude, ss$latitude, pch = pch, col = col, bg=bg ,cex = cex*scale, xlab = "", ylab = "", xlim = xlim, ylim = ylim,new=new,...)
    ## 
    if (!is.null(highlight)) {
        points(highlight$longitude, highlight$latitude, pch = 21 , col = col.subset,bg=bg.subset, cex = cex.subset,...)
    }
    ## add text if TRUE
    
    if (!is.null(unlist(subset)))
        if (add.text.subset)
            if (full.names.subset)
                text(highlight$longitude, highlight$latitude,highlight$location,pos=3,cex=cex.subset/2)
            else
                text(highlight$longitude, highlight$latitude,substr(toupper(highlight$location),1,3),pos=3,cex=cex.subset/2)
    
    if (add.text & showall)
         if (full.names)
             text(ss$longitude, ss$latitude,ss$location,pos=3,cex=cex/2)
         else
             text(ss$longitude, ss$latitude,substr(toupper(ss$location),1,3),pos=3,cex=cex/2)
    ##add label text
    
    if (showaxis) title(xlab = "Longitude",line=2.2 , cex.lab = cex.lab) 
    if (showaxis) title(ylab = "Latitude",line=2.2 , cex.lab = cex.lab) 
                                        # format axes
    if (showaxis) axis(1,seq(xlim[1],xlim[2],by=10),cex.axis=cex.axis) # 0.7
    if (showaxis) axis(2,seq(ylim[1],ylim[2],by=10),cex.axis=cex.axis)
    if (showaxis) axis(4,seq(ylim[1],ylim[2],by=10),cex.axis=cex.axis)
    if (showaxis) axis(3,seq(xlim[1],xlim[2],by=10),cex.axis=cex.axis)
}


col.bar <- function(breaks,horiz=TRUE,pch=21,v=1,h=1,col=col,cex=2,cex.lab=0.81,type="r",verbose=FALSE,vl=0.5,border=FALSE,...) {
    
    xleft <- par()$usr[1] 
    xright <- par()$usr[2]
    ybottom <- par()$usr[4] - 1 - h
    ytop <-  par()$usr[4] - 1 
    
    ## 
    by <- (xright - xleft - v * (length(col)))/(length(breaks))
    steps <-   seq(0, (xright -xleft - v * (length(col))) ,by=by ) # 
    nsteps <- length(steps) 
    
    if (verbose) print(steps)
    if (verbose) print(breaks)
    if (verbose) print(nsteps)
    
    ## if (max(abs(breaks))<=1) breaks <- round(breaks,digits=2)
    
    k <- 1/2
    for (i in 1 :(nsteps-2)) {  
        if (!is.null(v)) 
            if (i == 1) k <- k + v/2 else k <- k + v  
        if (type == "r") { ## "r" for rectangle
            rect(xleft= k  + xleft + steps[i] ,xright= k + xleft + steps[i+1],ybottom=ybottom,ytop=ytop,col=col[i],border=border)
            
            ## text(x = k + xleft + steps[i], y = ybottom - 1,labels=sprintf("%.1f",icn[i]),cex=cex)
        }
        else if (type == "p") { ## "p" points
            points(x= k + xleft + (steps[i]+ steps[i+1])/2, y=(ybottom + ytop)/2,pch=pch, bg=col[i],cex=cex,...)
            
        }
        
        text(x = k + xleft + (steps[i]+ steps[i+1])/2,  y = ybottom - vl, labels=levels(cut(breaks,breaks))[i],col="grey50",cex=cex.lab)
    } 
}

trend <- function(x,ns.omit=TRUE,alpha=0.1) {
    t <- 1:length(x)
    model <- lm(x ~ t)
    y <- c(model$coefficients[2] * 10 *100, anova(model)$Pr[1])
    if (ns.omit) if (y[2] > alpha) y[1] <- NA
    names(y) <- c("coefficients","P-value")
    return(y)
}

colbar2 <- function(x,col) {
    par0 <- par()
    par(mar=c(1,0,0,0),fig=c(0.5,1,0.665,0.695),new=TRUE,cex.axis=0.6)
    nl <- pretty(x)
    n <- length(nl)
    image(cbind(1:n,1:n),col=col) 
    par(xaxt="s")
    axis(1,at=seq(0,1,length=length(nl)),label=nl)
    par(fig=par0$fig)
}
