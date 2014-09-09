## Author 	 Rasmus E. Bnestad
## Updated 	 by Abdelkader Mezghani
## Last update   26.07.2013
## Includes	 map.station() ; test.map.station()
## Require 	 geoborders.rda

## A test function that performs a series of tests that produce and save maps of the different data sources into the local directory.
test.map.station <- function() {

map.station(src="NACD",col="darkgreen",bg="green")
dev.copy2pdf(file="NACD-Network.pdf")
dev.off()
map.station(src="ECAD",col="darkgreen",bg="green")
dev.copy2pdf(file="ECAD-Network.pdf")
dev.off()
map.station(src="GHCNM",col="darkgreen",bg="green")
dev.copy2pdf(file="GHCNM-Network.pdf")
dev.off()
map.station(src="GHCND",col="darkgreen",bg="green")
dev.copy2pdf(file="GHCND-Network.pdf")
dev.off()
map.station(src="NORDKLIM",col="darkgreen",bg="green")
dev.copy2pdf(file="NORDKLIM-Network.pdf")
dev.off()
map.station(src="NARP",col="darkgreen",bg="green")
dev.copy2pdf(file="NARP-Network.pdf")
dev.off()

locs <- map.station(ele=101)
locs <- map.station(cntr="NORWAY",ele=101,src="ECAD")
}


## The main function to produce map of selected stations
map.stationmeta <- function(...)
  map.station(...)

map.station <- function (x = NULL, stid = NULL, param = NULL, lon = NULL, lat = NULL, 
                         alt = NULL, cntr = NULL, src = NULL, xlim = NULL, ylim = NULL,
                         nmin = NULL , it = NULL ,
                         col = "darkgreen",bg="green",cex=2,breaks = NULL, zexpr = "alt",
                         select = NULL , col.select = "darkred" , bg.select = "red", cex.select=2, showall = FALSE,
                         verbose = FALSE , add.text=TRUE,height=NULL,width=NULL,
                         FUN=NULL,from=NULL,to=NULL,cex.colbar=1,type.colbar="p",cex.lab=1,...) 
{ 

  ##browser()
                                        #load("~/esd/data/geoborders.rda")
  data("geoborders", envir = environment())
                                        #load("station.meta.rda") # data("station.meta", envir = environment())
                                        #load("t2m.NORDKLIM.rda") # data("t2m.NORDKLIM", envir = environment())
                                        #data("precip.NORDKLIM", envir = environment())
  ##browser()
  if (zexpr == "alt") 
    zexpr <- "sqrt( station.meta$alt/max(station.meta$alt,na.rm=TRUE) )"
  n <- 100

  ## if (!is.null(select)) {col = "white" ; bg="white"} 

  if (showall==FALSE & !is.null(select)) {col = "white" ; bg="white"} 
 
  ## browser()
  if (!is.null(x)) { 
    if (inherits(x,"stationmeta")) {      
      ss <- x
      ss$variable <-apply(as.matrix(ss$element),1,esd2ele)
    }
    else if (inherits(x,"station")) {
      if (is.null(dim(x))) dim(x) <- c(length(x),1)
      ss <- list(station_id=attr(x,"station_id"),location=attr(x,'location'),country=attr(x,'country'),longitude=attr(x,"longitude"),latitude=attr(x,'latitude'),altitude=attr(x,'altitude'),element=attr(x,"element"),longname=attr(x,"longname"),start=rep(start(x),dim(x)[2]),end=rep(end(x),dim(x)[2]),source=attr(x,"source"))
    }
  }
  if (!is.null(select)) { ## highlight a subset of station
    ## browser()
    if (is.data.frame(select)){
      highlight <- select
      highlight$variable <-apply(as.matrix(highlight$element),1,esd2ele)
    }
    else if (inherits(select,"station")) {
      if (is.null(dim(select))) dim(select) <- c(length(select),1)
      highlight <- list(station_id=attr(select,"station_id"),location=attr(select,'location'),country=attr(select,'country'),longitude=attr(select,"longitude"),latitude=attr(select,'latitude'),altitude=attr(select,'altitude'),element=attr(select,"element"),longname=attr(select,"longname"), start=rep(start(select),dim(select)[2]),end=rep(end(select),dim(select)[2]),source=attr(select,"source"))
    }
  } else highlight <-  NULL
  
  if (is.null(x) & is.null(select)) ## Use select.station()
   ss <- select.station(stid = stid, param = param, lon = lon, lat = lat, alt = alt, cntr = cntr, src = src , nmin = nmin , it = it , verbose = verbose)

  ## Set negative altitude to NA
  ss$altitude[ss$altitude < 0] <- NA

   ##if (!is.null(highlight)) {
  ##    station.meta$lon[!highlight] <- NA
  ##    station.meta$lat[!highlight] <- NA
  ##    station.meta$alt[!highlight] <- NA
  ##}

  tte <- "rwb"

  cols <- rgb(seq(0, 0.5, length = 100)^2, seq(0.5, 1, length = 100), 
              seq(0, 0.5, length = 100)^2)

  dev.new(height=height,width=width)
                                        #par(bty = "n", xaxt = "n", yaxt = "n", xpd = FALSE)

   ## Select a subdomain in the x-axis
  if (is.null(xlim))
    if (length(lon) > 1)
      xlim <- floor(range(ss$longitude, na.rm = TRUE))
    else
      xlim <-floor(range(ss$longitude, na.rm = TRUE) + c(-5,5))  # +/- 5 degrees

  ## Select a subdomain in the y-axis
  if (is.null(ylim))
      if (length(lat) > 1) ylim <- floor(range(ss$latitude, na.rm = TRUE))
      else ylim <- floor(range(ss$latitude, na.rm = TRUE) + c(-5,5))
  
  plot(ss$longitude, ss$latitude, pch = 21, col = col, bg=bg ,cex = cex, xlab = "", ylab = "", xlim = xlim, ylim = ylim , axes = FALSE , frame.plot = FALSE,...)
  
  
  
  lines(geoborders$x, geoborders$y, col = "black")
  lines(attr(geoborders, "borders")$x, attr(geoborders, "borders")$y, col = "grey90")
  
  ## add title
  title(main=paste("SOURCE(S) : ", paste(levels(factor(ss$source)),collapse="/" )),line=3,cex.main=.8)
  ## add search info to plot
  title(main=paste(paste(levels(factor(ss$longname)),collapse="/")),line=2,cex.main=.8 , adj = 0)
  
  if (!is.null(highlight) & !showall) {
      title(main=paste(length(levels(factor(highlight$location))),"/",length(levels(factor(ss$location)))),line=2,cex.main=.8)
      title(main=paste(min(highlight$start),"/",max(highlight$end)),line=2,cex.main=.8,adj=1)
  }
  else {
    title(main=paste(length(ss$location)),line=2,cex.main=.8)
    title(main=paste(max(ss$start),"/",min(ss$end)),line=2,cex.main=.8,adj=1)
}
  
  ## title(main=attr(z,"title"),line=2.2,cex.main=0.7)
  ## add margin text
  mtext(paste(("ESD package - map.station() - MET Norway 2013"),"(www.met.no)",sep=" "),side=1,line=4,cex=0.6)
  
  ## add grid
  grid()
  
  ## insert color bar
  ## browser()
  if (TRUE) {  
      if (!is.null(FUN)) {
          if (is.element(FUN,c('lon','lat','alt')))
              eval(parse(text=paste('y <-',FUN,'(x)',sep="")))
          else {
              if (is.element("na.rm",names(formals(FUN))) | is.element("...",names(formals(FUN))))
                  y <- apply(coredata(x),2,FUN=FUN,na.rm=TRUE)
              else
                  y <- apply(coredata(x),2,FUN=FUN)
          }
          if (is.null(breaks)) {
              y.rng <- range(y,na.rm=TRUE)
              ## breaks <- seq(y.rng[1],y.rng[2],by=diff(y.rng)/10)
              breaks <- pretty(y)
          }
          COL <- rev(terrain.colors(n=length(breaks)))
          COL <- rainbow(n=length(breaks))
          col.bar(breaks,col=COL,type=type.colbar,v=cex,h=1,cex=cex)
          
          if (is.null(dim(x))) ns <- 1 else ns <- dim(x)[2]
          col <- rep(NA,ns)
          if (ns==1)
              k <- 1
          else {
              icol <- apply(as.matrix(y),2,findInterval,breaks)
              col <- "black"
              bg <- COL[icol]
          }
      } else {
          col=col;bg=bg     
      }
  }
  points(ss$longitude, ss$latitude, pch = 21, col = col, bg=bg ,cex = cex, xlab = "", ylab = "", xlim = xlim, ylim = ylim , ...)
  
  if (!is.null(select)) {
    points(highlight$longitude, highlight$latitude, pch = 21 , col = col.select,bg=bg.select, cex = cex.select,...)
  }
  ## add text if TRUE
  if ((add.text) & (!is.null(select))) text(highlight$longitude, highlight$latitude,highlight$location,pos=3,cex=cex.select/2)
  if (add.text & showall) text(ss$longitude, ss$latitude,substr(toupper(ss$location),1,3),pos=3,cex=cex/2)
  ##add label text
  
  title(xlab = "Longitude (Degrees-East)",line=2.2 , cex.lab = cex.lab) 
  title(ylab = "Latitude (Degrees-North)",line=2.2 , cex.lab = cex.lab) 
                                        # format axes
  axis(1,seq(xlim[1],xlim[2],by=10),cex.axis=cex) # 0.7
  axis(2,seq(ylim[1],ylim[2],by=10),cex.axis=cex)
  axis(4,seq(ylim[1],ylim[2],by=10),cex.axis=cex)
  axis(3,seq(xlim[1],xlim[2],by=10),cex.axis=cex)
  
  ## points(attr(t2m.NORDKLIM, "longitude"), attr(t2m.NORDKLIM,"latitude"), cex = 0.5)
  ## points(attr(precip.NORDKLIM, "longitude"), attr(precip.NORDKLIM,"latitude"), cex = 0.5)
  ## Add geo-border lines
  
                             
                                        #z <- round(eval(parse(text = zexpr)) * 100)
                                        #z[!is.finite(z)] <- 1
                                        #z[z < 1] <- 1
                                        #z[z > 100] <- 100
    
                                        #points(station.meta$lon, station.meta$lat, pch = 19, col = cols[z], cex = 0.5)
    #if (!is.null(stid)) {
    #    if (is.numeric(stid)) {
    #        iv <- is.element(station.meta$stid, stid)
    #    }
    #    else if (is.character(stid)) {
    #        nc <- nchar(stid)
    #        iv <- is.element(tolower(substr(station.meta$location, 
    #            1, nc)), tolower(stid))
    #    }
    #    points(station.meta$lon[iv], station.meta$lat[iv], pch = 19, 
    #        col = "white")
    #    points(station.meta$lon[iv], station.meta$lat[iv], lwd = 2)
    #}
    #par(fig = c(0.85, 0.99, 0.2, 0.4), new = TRUE, mar = rep(0,4), xaxt = "n", yaxt = "n", bty = "n")
    #plot(c(0, 1), c(0, 1), type = "n", xlab = "", ylab = "")
    #rect(0, 0, 1, 1, col = "white", border = "white")
    #zlim <- seq(0, max(z, na.rm = TRUE), by = 1)
    #for (i in zlim) {
    #    lines(c(0.7, 1), rep(i/max(zlim), 2), col = cols[zlim[i]],lwd = 2)
    #    if ((i%%10) == 0) 
    #        text(0.7, i/max(zlim), paste(round(sqrt(zlim[i])/100 * 
    #            max(station.meta$alt, na.rm = TRUE)), "m"), pos = 2,cex = 0.5, col = "grey40")
    #}
    #par(fig = c(0.03, 0.3, 0.1, 0.3), new = TRUE, mar = rep(0,4), xaxt = "s", yaxt = "s", bty = "n")
    #alt.rng <- range(highlight$altitude)
    #h <- hist(highlight$altitude, plot = FALSE, breaks = 50)
    #plot(range(h$mids), c(0, max(h$count)), type = "n", xlab = "", ylab = "")
    #lines(h$mids, h$count, lwd = 3, type = "s")
    #invisible(highlight)
    #par(bty = "n", xaxt = "n", yaxt = "n", xpd = FALSE, fig = c(0, 1, 0, 1), new = TRUE)
    #results <- station.meta$location[iv]
    #attr(result, "longitude") <- station.meta$lon[iv]
    #attr(result, "latitude") <- station.meta$lat[iv]
    #attr(result, "altitude") <- station.meta$alt[iv]
    #attr(result, "station_id") <- station.meta$stid[iv]
}


col.bar <- function(breaks,horiz=TRUE,v=1,h=1,col=col,cex=2,type="r",verbose=FALSE,vl=0.5) {
     
  xleft <- par()$usr[1] 
  xright <- par()$usr[2]
  ybottom <- par()$usr[4] - 1 - h
  ytop <-  par()$usr[4] - 1 
  ## browser()
  steps <-   seq(0, (xright -xleft - v * (length(col))) , (xright - xleft - v * (length(col)))/(length(breaks))) # 
  nsteps <- length(steps) 

  if (verbose) print(steps)
  if (verbose) print(breaks)
  if (verbose) print(nsteps)

  ## if (max(abs(breaks))<=1) breaks <- round(breaks,digits=2)
  
  k <- 0
  for (i in 1 :(nsteps-2)) {  
      if (!is.null(v)) 
          if (i == 1) k <- v/2 else k <- k + v  
      if (type == "r") { ## "r" for rectangle
          rect(xleft= k  + xleft + steps[i] ,xright= k + xleft + steps[i+1],ybottom=ybottom,ytop=ytop,col=col[i])
          
          ## text(x = k + xleft + steps[i], y = ybottom - 1,labels=sprintf("%.1f",icn[i]),cex=cex)
      }
      else if (type == "p") { ## "p" points
          points(x= k + xleft + (steps[i]+ steps[i+1])/2, y=(ybottom + ytop)/2,pch=21, bg=col[i],cex=cex)

      }
      
      text(x = k + xleft + (steps[i]+ steps[i+1])/2,  y = ybottom - vl, labels=levels(cut(breaks,breaks))[i],col="grey50",cex=cex)
  }
  
}

