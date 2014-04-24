
# Used to estimate annual statistics
## class creation
#annual <- function(x) structure(floor(x + .0001), class = "annual")

annual <- function(x, ...) UseMethod("annual")


annual.zoo <- function(x,FUN='mean',na.rm=TRUE,nmin=NULL, ...) {
  #print("annual.zoo")
  attr(x,'names') <- NULL
  yr <- year(x)
  class(x) <- 'zoo'
#  y <- aggregate(x,yr,FUN=match.fun(FUN),...,na.rm=na.rm)
  y <- aggregate(x,yr,FUN=FUN,...,na.rm=na.rm)
  invisible(y)
}

#annual.station.day <- function(x,FUN=mean,na.rm=TRUE,nmin=350, ...) {
#  attr(x,'names') <- NULL
  #print(class(index(x)))
#  yr <- year(x)
#  years <- as.numeric(rownames(table(yr)))
  #print(table(years))
#  ndyr <- as.numeric(table(yr))
  #print(table(ndyr))
#  ok <- is.element(yr,years[ndyr > nmin]) 
  #print(sum(ok))
#  X <- zoo(x[ok,],order.by=index(x)[ok])
#  yr <- yr[ok]
#  attr(X,'unit') <- attr(x,'unit')
  #print(ndyr)
#  cls <- class(x)
#  class(x) <- "zoo"
#  y <- aggregate.station(X,yr,match.fun(FUN),...,na.rm=na.rm)
#  unit <- attr(y,'unit')
#  y <- attrcp(x,y,ignore="unit")
#  unit -> attr(y,'unit')
#  #str(y); print(unit)
#  attr(y,'history') <- history.stamp(x)
#  class(y) <- cls
#  class(y)[2] <- "annual"
#  invisible(y)
#}


annual.default <- function(x,FUN='mean',na.rm=TRUE, nmin=NULL,...) {

  print('annual.default')
  nv <- function(x) sum(is.finite(x))

  #browser()
  if (inherits(FUN,'function')) FUN <- deparse(substitute(FUN)) # REB110314
  attr(x,'names') <- NULL
  yr <- year(x)
  d <- dim(x)
  #print(table(yr))
  YR <- as.numeric(rownames(table(yr)))
  nyr <- as.numeric(table(yr))
  nval <- aggregate(zoo(x,order.by=index(x)),year,FUN=nv)
  #print(c(length(nval),length(YR))); plot(nval)
  #print(YR)
  #print(class(x))
  # Need to accomodate for the possibility of more than one station series.
  if (inherits(x,'day')) {
    if (is.null(nmin)) nmin <- 365
    #fewd <- coredata(nval) < nmin
    #x[fewd] <- NA
    ok <- is.element(yr,YR[nyr >= nmin])  # REB quick fix.
  } else   if ( (inherits(x,'mon')) & is.null(nmin) ) {
      iy <- year(x)
      nmy <- as.numeric(table(iy))
      full <- nmy[nmy==12]
      x[is.element(iy,!full),] <- NA
      na.rm=FALSE
  } else
  if (inherits(x,'month')) {
    OK <- nyr == 12
    ok <- is.element(yr,YR[OK])
  } else if (inherits(x,'season')) {
    OK <- nyr == 4
    ok <- is.element(yr,YR[OK])
  } else ok <- is.finite(yr)
  #print(c(sum(ok),length(ok))); print(YR[nyr < nmin])
  #print(nyr); print(YR[OK]); print(yr[ok]); print(sum(ok)); print(length(ok))
  if (length(d)==2) X <- zoo(coredata(x[ok,]),order.by=index(x)[ok]) else
                    X <- zoo(coredata(x[ok]),order.by=index(x)[ok])
  if (FUN == 'sum') na.rm <- FALSE ## AM
  #y <- aggregate(X,yr[ok],FUN=FUN,...,na.rm=na.rm) ## AM
  #browser()
  #y <- aggregate(X,year,FUN==FUN,...,na.rm=na.rm) ## AM
  y <- aggregate(X,year,FUN=match.fun(FUN),...,na.rm=na.rm) # REB.
  y[!is.finite(y)] <- NA ## AM
  y <- attrcp(x,y,ignore="names")
  attr(y,'history') <- history.stamp(x)
  class(y) <- class(x)
  class(y)[length(class(y))-1] <- "annual"
  if (class(y)[1]=="spell") class(y) <- class(y)[-1]
  return(y)
}


annual.station <- function(x,FUN='mean',nmin=NULL,...) {
  #print('annual.station')
  attr(x,'names') <- NULL
#  if (inherits(x,'day')) {
#    y <- annual.station.day(x,FUN=match.fun(FUN),...)
#  } else {
#  ns <- length(x[1,])
#  for (i in 1:ns) {
#    y <- annual.default(x,FUN=match.fun(FUN),nmin=nmin,...)
    y <- annual.default(x,FUN=FUN,nmin=nmin,...)
#    if (i==1) y <- z else y <- c(y,z)
#  }
  return(y)
}

annual.spell <- function(x,FUN='mean',nmin=0,...) {
  attr(x,'names') <- NULL
  if ( (inherits(x,'mon'))  & is.null(nmin) ) {
    iy <- year(x)
    nmy <- as.numeric(table(iy))
    full <- nmy[nmy==12]
    x[is.element(iy,!full),] <- NA
    na.rm=FALSE
  }
  #annual.default(x,FUN=match.fun(FUN),...)
  annual.default(x,FUN=FUN,nmin=nmin,...)
}

annual.dsensemble <- function(x,FUN='mean') {
  #print("annual.dsensemble")
  y <- subset(x,it=0)
  return(y)
}



annual.field <- function(x,FUN='mean',na.rm=TRUE,nmin=NULL, ...) {
  attr(x,'names') <- NULL
  if (inherits(FUN,'function')) FUN <- deparse(substitute(FUN)) # REB110314
  yr <- year(x)
  cls <- class(x)
  class(x) <- "zoo"
  if ( (inherits(x,'mon'))  & is.null(nmin) ) {
    iy <- year(x)
    nmy <- as.numeric(table(iy))
    full <- nmy[nmy==12]
    x[is.element(iy,!full),] <- NA
    na.rm=FALSE
  }
#  y <- aggregate(x,yr,FUN=match.fun(FUN),...,na.rm=na.rm)
  y <- aggregate(x,yr,FUN=FUN,...,na.rm=na.rm)
  y <- attrcp(x,y)
  attr(y,'history') <- history.stamp(x)
  attr(y,'dimensions') <- c(attr(x,'dimensions')[1:2],length(index(y)))
  class(y) <- cls
  class(y)[2] <- "annual"
  invisible(y)
}

year <- function(x) {
  #str(x); print(class(x)); print(index(x))
  if (inherits(x,'integer')) x <- as.numeric(x)
  
  if ( (inherits(x,'numeric')) & (min(x,na.rm=TRUE) > 0) & (max(x,na.rm=TRUE) < 3000) )
    return(x)
  
  if (inherits(x,c('station','field','zoo'))) {
    y <- year(index(x))
    return(y)
  }
  if ( (class(x)[1]=="character") & (nchar(x[1])==10) ) {
    y <- year(as.Date(x))
    return(y)
  }
  if (class(index(x))=="numeric") {
    y <- trunc(index(x))
    return(y)
  }
  #print("here"); print(index(x))
  if (class(x)[1]=="Date")
    y <- as.numeric(format(x, '%Y')) else
  if (class(x)[1]=="yearmon") y <- trunc(as.numeric(x)) else
  if (class(x)[1]=="yearqtr") y <- trunc(as.numeric(x)) else
  if (class(x)[1]=="character") y <- trunc(as.numeric(x)) else
  if (class(x)[1]=="season") {
    # If season, then the first month is really the December month of the previous year
     month <- round(12*(as.numeric(index(x)) - trunc(as.numeric(index(x)))) + 1)
     y[is.element(month,1)] <- y[is.element(month,1)] - 1
   } else print('confused...')
  return(y)
}




month <- function(x) {

  if (inherits(x,'integer')) x <- as.numeric(x)
  if ( (inherits(x,'numeric')) & (min(x,na.rm=TRUE) > 0) & (max(x,na.rm=TRUE) < 13) )
    return(x)
  if (inherits(x,c('station','field','zoo'))) {
    y <- month(index(x))
    return(y)
  }
  if ( (class(x)[1]=="character") & (nchar(x[1])==10) ) {
    y <- month(as.Date(x))
    return(y)
  }
  #print(class(index(x)))
  if (class(x)[1]=="Date")
    month <- as.numeric(format(x, '%m')) else
  if (class(x)[1]=="yearmon")
    month <- round(12*(as.numeric(x) - trunc(as.numeric(x))) + 1) else
  if (class(x)[1]=="yearqtr") month <- round(12*(as.numeric(x) - trunc(as.numeric(x))) + 1)
  if (class(x)[1]=="season") {
    # If season, then the first month is really the months are DJF, MAM, JJA, and OND:
    month <- month - 1
    month[month==-1] <- 12
  }
  return(month)
}

day <- function(x) {
  if (inherits(x,'integer')) x <- as.numeric(x)
  if ( (inherits(x,'numeric')) & (min(x,na.rm=TRUE) > 0) & (max(x,na.rm=TRUE) < 32) )
    return(x)
    
  if (inherits(x,c('station','field','zoo'))) {
    y <- day(index(x))
    return(y)
  }
  if ( (class(x)[1]=="character") & (nchar(x[1])==10) ) {
    y <- day(as.Date(x))
    return(y)
  }
  if (class(x)[1]=="Date") day <- as.numeric(format(x, '%d'))
  return(day)
}

# Used to estimate Dec-Feb, Mar-May, Jun-Aug, and Sep-Nov statistics
# Manipulate the zoo-object by shifting the year/chonology so that
# zoo thinks the year defined as December-November is January-December.

season <- function(x,format="numeric", ...) {
  if (inherits(x,'integer')) x <- as.numeric(x)
  if ( (inherits(x,'numeric')) & (min(x,na.rm=TRUE) > 0) & (max(x,na.rm=TRUE) < 5) )
    return(x)
  
  if (inherits(x,c('station','field','zoo'))) {
    y <- season(index(x))
    return(y)
  }
  if ( (class(x)[1]=="character") & (nchar(x[1])==10) ) {
    y <- season(as.Date(x))
    return(y)
  }  
  if (class(x)[1]=="Date") {
    xl <- x
    n <- length(x)
    xl[2:n] <- x[1:n-1]
    xl[1] <- NA
    y <- yearqtr(xl)    
    y <- as.numeric(format(x, '%q'))
  }
  if (format=="character") y <- season.name[y+1]
  return(y)
}



seasonal.yearmon <- function(x) {

  attr(season,'history') <- history.stamp(x)
  season
}

season.name <- function() {
  season.c<-c("annual","DJF","MAM","JJA","SON")
  season<-cbind(c(1,12,12),c(12,1,2),c(3,4,5),c(6,7,8),c(9,10,11))
  colnames(season) <- season.c
  season
}



pentad <- function(x) {
  if (inherits(x,'station','field')) {
    y <- pentad(index(x))
    return(y)
  }  
  pentad <- 5*trunc(year(x)/5)
  pentad
}

