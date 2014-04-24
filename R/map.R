# R.E. Benestad
# Plot a map of the station locations, fields, EOFs, CCA results, correlation, composites, ...

require(zoo)

map <- function(x,it=NULL,is=NULL,...) UseMethod("map")

map.default <- function(x,it=NULL,is=NULL,projection="lonlat",
                        xlim=NULL,ylim=NULL,n=15,
                        cols=NULL,breaks=NULL,
                        what=NULL,gridlines=FALSE,
                        lonR=NULL,latR=-90,axiR=NULL,...) {

  # default with no arguments will produce a map showing the station data in the esd package.

  x <- subset(x,it=it,is=is)
  X <- attr(x,'pattern')
  attr(X,'longitude') <- lon(x)
  attr(X,'latitude') <- lat(x)
  attr(X,'variable') <- attr(x,'variable')
  attr(X,'unit') <- attr(x,'unit')
  attr(X,'source') <- attr(x,'source')
  if (inherits(X,'zoo')) attr(X,'time') <- range(index(x)) else
  if (!is.null(attr(x,'time'))) attr(X,'time') <- attr(x,'time')
  if (projection=="lonlat") lonlatprojection(x=X,xlim=xlim,ylim=ylim,n=n,
                                             cols=cols,breaks=breaks,
                                             what=what,
                                             gridlines=gridlines,...) else
  if (projection=="sphere") map2sphere(x=X,lonR=lonR,latR=latR,axiR=axiR,
                                       what=what,gridlines=gridlines,
                                       cols=cols,...) else
  if (projection=="np") map2sphere(X,lonR=lonR,latR=latR,axiR=axiR,
                                   what=what,gridlines=gridlines,
                                   cols=cols,...) else
  if (projection=="sp") map2sphere(X,lonR=lonR,latR=latR,axiR=axiR,
                                   what=what,gridlines=gridlines,cols=cols,...)
  invisible(X)
  
  #map.station(NULL,...)
}

# If x is provided, map only x...
map.matrix <- function(x,projection="lonlat",...) {

  # default with no arguments will produce a map showing the station data in the esd package.

#  image(lon(x),lat(x),x)
  if (inherits(x,'zoo')) attr(x,'time') <- range(index(x)) 
  if (projection=="lonlat") lonlatprojection(x=x,...)  else
  if (projection=="sphere") map2sphere(x=x,...) else
  if (projection=="np") map2sphere(x,...) else
  if (projection=="sp") map2sphere(x,...)
  invisible(x)
  
  #map.station(NULL,...)
}


map.comb <- function(x,it=NULL,is=NULL,xlim=NULL,ylim=NULL,pattern=1,n=15,
                     projection="lonlat",cols=NULL,breaks=NULL,
                     lonR=NULL,latR=NULL,axiR=0,what=c("fill","contour"),
                     gridlines=TRUE,...) {
  stopifnot(inherits(x,'eof'))
  x <- subset(x,it=it,is=is)
  projection <- tolower(projection)
  if (is.null(cols)) cols <- colscal(n=n-1) else
  if (length(cols)==1) {
     palette <- cols
     cols <- colscal(col=palette,n=n-1)
  }
  map.eof(x=x,xlim=xlim,ylim=ylim,pattern=pattern,
          n=n,projection=projection,cols=cols,
          breaks=breaks,lonR=lonR,latR=latR,axiR=axiR,what=what,
          gridlines=gridlines,...) -> result
  invisible(result)
 }

map.eof <- function(x,it=NULL,is=NULL,pattern=1,xlim=NULL,ylim=NULL,n=15,
                    projection="lonlat",cols=NULL,
                    breaks=NULL,lonR=NULL,latR=NULL,axiR=0,
                    what=c("fill","contour"),gridlines=TRUE,...) {
  print('map.eof')
  stopifnot(inherits(x,'eof'))
  #x <- subset(x,it=it,is=is)
  projection <- tolower(projection)
  tot.var <- attr(x,'tot.var')
  D <- attr(x,'eigenvalues')
  var.eof <- 100* D^2/tot.var
  X <- attr(x,'pattern')[,,pattern]
  #str(x)
  attr(X,'longitude') <- attr(x,'longitude')
  attr(X,'latitude') <- attr(x,'latitude')
  attr(X,'variable') <- attr(x,'variable')
  attr(X,'unit') <- attr(x,'unit')
  attr(X,'source') <- attr(x,'source')
  attr(X,'time') <- range(index(x))
  if (projection=="lonlat") lonlatprojection(x=X,xlim=xlim,ylim=ylim,
                             n=n,cols=cols,breaks=breaks,
                             what=what,gridlines=gridlines,...) else
  if (projection=="sphere") map2sphere(x=X,lonR=lonR,latR=latR,axiR=axiR,
                                       what=what,gridlines=gridlines,
                                       cols=cols,...) else
  if (projection=="np") map2sphere(X,lonR=lonR,latR=90,axiR=axiR,
                                       what=what,gridlines=gridlines,
                                       cols=cols,...) else
  if (projection=="sp") map2sphere(X,lonR=lonR,latR=-90,axiR=axiR,
                                       what=what,gridlines=gridlines,
                                       cols=cols,...)
  invisible(X)
}


map.ds <- function(x,it=NULL,is=NULL,xlim=xlim,ylim=ylim,
                   what=c("fill","contour"),
                   n=15,projection="lonlat",
                   lonR=NULL,latR=NULL,axiR=0,gridlines=TRUE,
                   cols=NULL,breaks=NULL,...) {
  stopifnot(inherits(x,'ds'))
  x <- subset(x,it=it,is=is)
  projection <- tolower(projection)
  X <- attr(x,'pattern')
#  attr(X,'longitude') <- attr(X,'longitude')
#  attr(X,'latitude') <- attr(x,'latitude')
  attr(X,'variable') <- attr(x,'variable')

  unit <- attr(x,'unit')
  if ( (is.na(unit) | is.null(unit)) ) unit <- " "
  for (i in 1:length(unit)) {
    if ((unit[i]=='degree Celsius') | (unit[i]=='deg C') | (unit[i]=='degC'))
         unit[i] <- 'degree*C'
  }
  
  attr(X,'unit') <- unit
  attr(X,'source') <- attr(x,'source')
  #dim(X) <- attr(x,'dimensions')[1:2]
  #print(c(dim(X),length(attr(X,'longitude')),length(attr(X,'longitude'))))

  if (projection=="lonlat") lonlatprojection(x=X,n=n,cols=cols,breaks=breaks,
                             what=what,gridlines=gridlines,...) else
  if (projection=="sphere") map2sphere(x=X,lonR=lonR,latR=latR,axiR=axiR,
                                       what=what,gridlines=gridlines,
                                       cols=cols,...) else
  if (projection=="np") map2sphere(X,lonR=lonR,latR=90,axiR=axiR,
                                       what=what,gridlines=gridlines,
                                       cols=cols,...) else
  if (projection=="sp") map2sphere(X,lonR=lonR,latR=-90,axiR=axiR,
                                   what=what,gridlines=gridlines,
                                   cols=cols,...)
  invisible(X)
}


map.field <- function(x,it=NULL,is=NULL,xlim=NULL,ylim=NULL,
                      what=c("fill","contour"),
                      FUN='mean',n=15,projection="lonlat",
                      lonR=NULL,latR=NULL,na.rm=TRUE,
                      axiR=0,gridlines=FALSE,cols=NULL,breaks=NULL,...) {
  stopifnot(inherits(x,'field'))
  x <- subset(x,it=it,is=is)
  #print(length(x)); print(attr(x,'dimensions')[1:2])
  projection <- tolower(projection)

  if (!is.null(xlim)) {
    if (xlim[1] < 0) x <- g2dl(x,greenwich=FALSE)
  }
  X <- coredata(x)
  #str(X)
  if (inherits(X,"matrix")) X <- apply(x,2,FUN=FUN,na.rm=na.rm)
  #print(length(X))
  attr(X,'longitude') <- attr(x,'longitude')
  attr(X,'latitude') <- attr(x,'latitude')
  attr(X,'variable') <- attr(x,'variable')
#  if (attr(x,'unit')=="deg C") attr(X,'unit') <- expression(degree*C) else
  attr(X,'unit') <- attr(x,'unit')
  attr(X,'source') <- attr(x,'source')
  attr(X,'time') <- range(index(x))
  attr(X,'method') <- FUN
  attr(X,'timescale') <- class(x)[2]
  #print(length(X)); print(attr(x,'dimensions'))
  dim(X) <- attr(x,'dimensions')[1:2]
  #class(X) <- class(x)
  #str(X)
  
  if (projection=="lonlat") lonlatprojection(x=X,xlim=xlim,ylim=ylim,n=n,
                                             cols=cols,breaks=breaks,
                                             what=what,
                                             gridlines=gridlines,...) else
  if (projection=="sphere") map2sphere(x=X,lonR=lonR,latR=latR,axiR=axiR,
                                       what=what,gridlines=gridlines,
                                       cols=cols,...) else
  if (projection=="np") map2sphere(X,lonR=lonR,latR=90,axiR=axiR,
                                   what=what,gridlines=gridlines,
                                   cols=cols,...) else
  if (projection=="sp") map2sphere(X,lonR=lonR,latR=-90,axiR=axiR,
                                   what=what,gridlines=gridlines,
                                   cols=cols,...)
  invisible(X)
}


map.corfield <- function(x,xlim=NULL,ylim=NULL,n=15,projection="lonlat",
                         cols=NULL,breaks=NULL,
                         lonR=NULL,latR=NULL,axiR=0,what=c("fill","contour"),
                         gridlines=TRUE,...) {
  #print("map.corfield")
  stopifnot(inherits(x,'corfield'))
  x <- subset(x,it=it,is=is)
  projection <- tolower(projection)
  dim(x) <- attr(x,'dimensions')[1:2]

  if (projection=="lonlat") lonlatprojection(x=x,n=n,cols=cols,breaks=breaks,
                             what=what,gridlines=gridlines,...) else
  if (projection=="sphere") map2sphere(x=x,lonR=lonR,latR=latR,axiR=axiR,
                                       what=what,gridlines=gridlines,
                                       cols=cols,...) else
  if (projection=="np") map2sphere(x,lonR=lonR,latR=90,axiR=axiR,
                                   what=what,gridlines=gridlines,
                                   cols=cols,...) else
  if (projection=="sp") map2sphere(x,lonR=lonR,latR=-90,axiR=axiR,
                                   what=what,gridlines=gridlines,
                                   cols=cols,...)
  if (!is.null(attr(x,'x.longitude')) & !is.null(attr(x,'x.latitude')))
      points(attr(x,'x.longitude'),attr(x,'x.latitude'),lwd=2,cex=1.2)
  invisible(x)
}


map.trend <- function(x,it=NULL,is=NULL,xlim=NULL,ylim=NULL,n=15,
                      projection="lonlat",
                      cols=NULL,breaks=NULL,
                     lonR=NULL,latR=NULL,axiR=0,what=c("fill","contour"),
                     gridlines=TRUE,...) {
  stopifnot(inherits(x,'field'),inherits(x,'trend'))
  x <- subset(x,it=it,is=is)
  projection <- tolower(projection)
  X <- attr(x,'pattern')
  attr(X,'longitude') <- attr(x,'longitude')
  attr(X,'latitude') <- attr(x,'latitude')
  attr(X,'variable') <- paste(attr(x,'variable'),'trend')
  attr(X,'time') <- range(index(x))
  attr(X,'unit') <- paste('d',attr(x,'unit'),'/decade')
  attr(X,'source') <- attr(x,'source')
  dim(X) <- attr(x,'dimension')[1:2]
  #str(X)
  if (projection=="lonlat") lonlatprojection(x=X,n=n,cols=cols,breaks=breaks,
                             what=what,gridlines=gridlines,...) else
  if (projection=="sphere") map2sphere(x=X,lonR=lonR,latR=latR,axiR=axiR,
                                       what=what,gridlines=gridlines,
                                       cols=cols,...) else
  if (projection=="np") map2sphere(X,lonR=lonR,latR=90,axiR=axiR,
                                   what=what,gridlines=gridlines,
                                   cols=cols,...) else
  if (projection=="sp") map2sphere(X,lonR=lonR,latR=-90,axiR=axiR,
                                   what=what,gridlines=gridlines,
                                   cols=cols,...)
  invisible(X)
}


lonlatprojection <- function(x,it=NULL,is=NULL,xlim=NULL,ylim=NULL,
                             n=15,cols=NULL,breaks=NULL,geography=TRUE,
                             what=c("fill","contour"),gridlines=TRUE,new=TRUE,...) {
  #print('lonlatprojection')
  #print(dim(x)); print(c(length(attr(x,'longitude')),length(attr(x,'latitude'))))
  data("geoborders",envir=environment())
  # To deal with grid-conventions going from north-to-south or east-to-west:
  srtx <- order(attr(x,'longitude')); lon <- attr(x,'longitude')[srtx]
  srty <- order(attr(x,'latitude')); lat <- attr(x,'latitude')[srty]
  #print('meta-stuff')
  unit <- attr(x,'unit'); variable <- attr(x,'variable')
  if ( (unit=="degC") | (unit=="deg C") | (unit=="degree C") )
    unit <- "degree*C"
  if (unit=="%") unit <- "'%'"
  if ( (tolower(variable)=="t(2m)") | (tolower(variable)=="t2m") )
    variable <- "T[2*m]"
#  if (inherits(x,'corfield'))
#    main=eval(parse(text=paste('expression(paste("correlation: ",',
#                               variable," *(",unit,")))",sep=""))) else
  main=eval(parse(text=paste('expression(',variable," *(",unit,"))",sep="")))
  sub <- attr(x,'source')
  #print('time')
  if (!is.null(attr(x,'timescale'))) {
    #print(attr(x,'timescale'))
    timescale <- attr(x,'timescale')
    if (timescale == 'annual') {
      t1 <- year(attr(x,'time'))[1]
      t2 <- year(attr(x,'time'))[2]
    } else
    if (sum(is.element(c('month','season'),timescale))>0) {
#      t1 <- format(attr(x,'time')[1],'%Y-%m')
#      t2 <- format(attr(x,'time')[2],'%Y-%m')
      t1 <- paste(year(attr(x,'time'))[1],month(attr(x,'time'))[1])
      t2 <- paste(year(attr(x,'time'))[2],month(attr(x,'time'))[2])
    } else {
      t1 <- attr(x,'time')[1]  
      t2 <- attr(x,'time')[2]
    }
    period <- paste('[',t1,', ',t2,']',sep='')
  } else period <- NULL
  #print(period)
  method <- attr(x,'method')
  x <- x[srtx,srty]
  #print("HERE")
  if (!is.null(xlim)) {
    outside <- (lon < xlim[1]) | (lon > xlim[2])
    x[outside,] <- NA
  } else xlim <- range(lon)
  
  if (!is.null(ylim)) {
    outside <- (lat < ylim[1]) | (lat > ylim[2])
    x[,outside] <- NA
  } else ylim=range(lat)

  #print(n); print(summary(c(x)))
  if (is.null(breaks))
    breaks <- pretty(c(x),n=n)
  #str(breaks)
  
  if (is.null(cols)) cols <- colscal(n=length(breaks)-1) else
  if (length(cols)==1) {
     palette <- cols
     cols <- colscal(col=palette,n=length(breaks)-1)
  }
  
  #print(c(length(breaks),length(cols)))
  #if (is.Date(what))

  if ( (par()$mfcol[1]> 1) | (par()$mfcol[2]> 1) ) new <- FALSE
      
  if (new) {
    par(bty="n",xaxt="n",yaxt="n",xpd=FALSE,
        fig=c(0.05,0.95,0.12,0.95),mar=rep(1,4))
  } else {
    par(bty="n",xaxt="n",yaxt="n",xpd=FALSE,
        mar=rep(1,4))
  }
  plot(range(lon),range(lat),type="n",xlab="",ylab="", # REB 10.03
         xlim=xlim,ylim=ylim)                # to sumerimpose.
  par0 <- par()
  if (sum(is.element(tolower(what),'fill'))>0)   
    image(lon,lat,x,xlab="",ylab="",add=TRUE,
          col=cols,breaks=breaks,xlim=xlim,ylim=ylim,...) 
    
  if (geography) {
    lines(geoborders$x,geoborders$y,col="darkblue")
    lines(attr(geoborders,'borders')$x,attr(geoborders,'borders')$y,col="pink")
    lines(geoborders$x+360,geoborders$y,col="darkblue")
  }
  if (sum(is.element(tolower(what),'contour'))>0)
     contour(lon,lat,x,lwd=1,col="grey70",add=TRUE)
  if (gridlines) grid()
  par(xpd=TRUE)
  dlat <- diff(range(lat))/60
  #print(dlat)
  text(lon[1],lat[length(lat)] + dlat,main,pos=4,font=2)
  text(lon[1],lat[1] - dlat,sub,col="grey30",pos=4,cex=0.7)

  if (!is.null(period))
    text(lon[length(lon)],lat[length(lat)] + dlat,period,pos=2,cex=0.7,col="grey30")
  if (!is.null(method))
    text(lon[length(lon)],lat[1] - dlat,method,col="grey30",pos=2,cex=0.7)
  if (new) {
    par(fig = c(0.3, 0.7, 0.05, 0.10),mar=rep(0,4),cex=0.8,
        new = TRUE, mar=c(1,0,0,0), xaxt = "s",yaxt = "n",bty = "n")
  #print("colourbar")
    bar <- cbind(breaks,breaks)
    image(breaks,c(1,2),bar,col=cols,breaks=breaks)
  
#    par(bty="n",xaxt="n",yaxt="n",xpd=FALSE,
#        fig=c(0.05,0.95,0.12,0.95),new=TRUE)
#    plot(range(lon),range(lat),type="n",xlab="",ylab="")
    par(fig=par0$fig,new = TRUE, mar=par0$mar, xaxt = "n",yaxt = "n",bty = 'n')
    plot(range(lon),range(lat),type="n",xlab="",ylab="",
         xlim=xlim,ylim=ylim)
  }
  
  result <- list(x=lon,y=lat,z=x,breaks=breaks)
  invisible(result)
}


#map.pca <- function(x,it=NULL,is=NULL,ipca=1,cex=1.5,xlim=NULL,ylim=NULL,
#                    n=100,projection="lonlat",
#                    FUN=NULL,cols=NULL,breaks=NULL,
#                    lonR=NULL,latR=NULL,axiR=0,what=c("fill","contour"),
#                    gridlines=TRUE) {
#  print('map.pca')
#  x <- subset(x,it=it,is=is)
#  lon <- attr(x,'longitude')
#  lat <- attr(x,'latitude')
#
#  U <- attr(x,'pattern')
#  L <- attr(x,'eigenvalues')
#  V <- coredata(x)
#  #print(dim(U)); print(length(L)); print(dim(V))
  #X <- U %*% diag(L) %*% t(V)
#  X <- U %*% diag(L)
  #print(dim(X))
#  X <- X[,ipca]
#  X <- apply(X,1,FUN,na.rm=TRUE);
#  N <- length(X)
#  col <- colscal(n=n)
#  ax <- quantile(abs(X),0.95,na.rm=TRUE)
#  scale0 <- seq(-ax,ax,length=n)
  #print(scale0); print(n)
#  a.T <- rep(NA,N)
#  for (i in 1:N) a.T[i] <-  sum(X[i] > scale0)
#  a.T[a.T < 1] <- 1; a.T[a.T > 100] <- 100
#
#  par(bty="n",xaxt="n",yaxt="n",xpd=FALSE,
#      fig=c(0.05,0.95,0.1,0.95),mar=rep(1,4))
  #print(rbind(lon,lat,a.T))
#  plot(lon,lat,col=col[a.T],pch=19,xlab="",ylab="",cex=cex)
#  data(geoborders,envir=environment())
#  lines(geoborders)
#  lines(geoborders$x - 360,geoborders$y)
#  colbar(scale0,col,fig=c(0.90,0.95,0.05,0.25))
#}

map.pca <- function(x,pattern=1,...) {
  X <- attr(x,'pattern')[,pattern]
  #str(x)
  X <- attrcp(x,X)
  attr(X,'longitude') <- lon(x)
  attr(X,'latitude') <- lat(x)
  class(X) <- 'station'
  map.station(X,...)
}

map.mvr <- function(x,it=NULL,is=NULL,xlim=NULL,ylim=NULL,
                    n=15,projection="lonlat",
                    cols=NULL,breaks=NULL,
                    lonR=NULL,latR=NULL,axiR=0,what=c("fill","contour"),
                    gridlines=TRUE) {
  x <- subset(x,it=it,is=is)
  
}

map.cca <- function(x,it=NULL,is=NULL,icca=1,xlim=NULL,ylim=NULL,
                    what=c("fill","contour"),
                    n=15,projection="lonlat",
                    lonR=NULL,latR=NULL,
                    axiR=0,gridlines=FALSE,cols=NULL,breaks=NULL) {
  print('map.cca')
  #x <- subset(x,it=it,is=is)

  # For plotting, keep the same kind of object, but replace the patterns in
  # the eof/pca with the CCA patterns
  Y <- x$Y
  #print(dim(attr(Y,'pattern'))); print(dim(U))
  #attr(Y,'pattern') <- U
  U <- x$B.m
  dim(U) <- c(dim(attr(Y,'pattern'))[-length(dim(attr(Y,'pattern')))],
                                     length(x$i.eofs))
  attr(Y,'pattern') <- U
  attr(Y,'eigenvalues') <- rep(1,length(x$i.eofs))
  attr(Y,'time') <- range(index(x))
  X <- x$X
  #print(dim(attr(X,'pattern'))); print(dim(V))
  #attr(X,'pattern') <- V
  V <- x$A.m
  dim(V) <- c(dim(attr(X,'pattern'))[-length(dim(attr(X,'pattern')))],
                                     length(x$i.eofs))
  attr(X,'pattern') <- V
  attr(X,'eigenvalues') <- rep(1,length(x$i.eofs))
  attr(X,'time') <- range(index(x))
  
  map(Y,icca,xlim=xlim,ylim=ylim,what=what,
      projection=projection,lonR=lonR,latR=latR,axiR=axiR,
      gridlines=gridlines,cols=cols,breaks=breaks)
  map(X,icca,xlim=xlim,ylim=ylim,what=what,
      projection=projection,lonR=lonR,latR=latR,axiR=axiR,
      gridlines=gridlines,cols=cols,breaks=breaks)
  invisible(list(U=U,V=V))
}


# Produce a KMZ-file to show the data in GoogleEarth.
map.googleearth <- function(x) {
}
