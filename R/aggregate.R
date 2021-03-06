# R.E .Benestad

# I'm unsure about the use of 'as' functions - perhaps just use 'monthly.station' or 'aggregate'?
# as.Date(,format='%m/%d/%Y'), weekdays()


# Time conversion tools:



aggregate.station <- function(x,by,FUN = 'mean', na.rm=TRUE, ...,
                              regular = NULL, frequency = NULL) {
  #print('aggregate.station')
  class(x) -> cls
  #print(deparse(substitute(by)))
  class(x) <- "zoo"
  #print("-------"); str(x)
#  fun4exc <- as.character(match.call())
#  if (inherits(FUN,'function')) FUN <- deparse(substitute(FUN)) # REB140314
#  print(fun4exc)
  # This is a less elegant solution to ensure right units...
#  if (length(grep("exceedance",fun4exc))==1) {
#    if (length(grep("counts",fun4exc))==1) fun <- "counts" else
#    if (length(grep("freq",fun4exc))==1) fun <- "freq" else
#    if (length(grep("wetfreq",fun4exc))==1) fun <- "wetfreq" else
#    if (length(grep("wetmean",fun4exc))==1) fun <- "wetmean" else
#                                            fun <- "mean"
#  } else fun <- "mean"

  #print(names(list(...))); print(names(formals(FUN)))
  
 # if (length(grep("threshold",fun4exc))==0) threshold <- 1
#  y <- aggregate(x, by, match.fun(FUN),na.rm=TRUE, ...,
#                 regular = regular, frequency = frequency)
  #browser()
  if ( (sum(is.element(names(formals(FUN)),'na.rm')==1)) |
       (sum(is.element(FUN,c('mean','min','max','sum','quantile')))>0 ) )
    y <- aggregate(x, by, FUN, na.rm=TRUE, ...,
                   regular = regular, frequency = frequency) else
    y <- aggregate(x, by, FUN, ..., regular = regular, frequency = frequency)
  #str(y); print("-------")

  if (class(index(y))=="Date") {
  dy <- day(y); mo <- month(y); yr <- year(y)
    if (dy[2] - dy[1] > 0) cls[length(cls) - 1] <- "day" else
    if (mo[2] - mo[1] == 1) cls[length(cls) - 1] <- "month" else
    if (mo[2] - mo[1] == 3) cls[length(cls) - 1] <- "season" else
    if (yr[2] - yr[1] > 0) cls[length(cls) - 1] <- "annual"
   } else
  if (class(index(y))=="yearmon") cls[length(cls) - 1] <- "month" else
  if (class(index(y))=="yearqtr") cls[length(cls) - 1] <- "qtr" else
  if (class(index(y))=="numeric") cls[length(cls) - 1] <- "annual" else
  if (class(index(y))=="character") cls[length(cls) - 1] <- "annual"
  class(y) <- cls
  y <- attrcp(x,y)

  args <- list(...)
  #print(names(args))
  ix0 <- grep('threshold',names(args))
  if (length(ix0)>0) threshold <- args[[ix0]] else threshold <- 1
  #print(threshold)
 
   #print(FUN)
  if (FUN=="counts")  {
    #print("Count")
    attr(y,'unit') <- paste("counts | X >",threshold," * ",attr(x,'unit'))
  } else if (FUN=="freq") {
    #print("Frequency")
    attr(y,'variable') <- 'f'
    attr(y,'unit') <- paste("frequency | X >",threshold," * ",attr(x,'unit'))
  } else if (FUN=="wetfreq") {
    #print("Wet-day frequency")
    attr(y,'variable') <- 'f[w]'
    attr(y,'unit') <- paste("frequency | X >",threshold," * ",attr(x,'unit'))
  } else if (FUN=="wetmean") {
    #print("Wet-day mean")
    attr(y,'variable') <- 'mu'
    attr(y,'unit') <- 'mm/day'
  } else if (FUN=="HDD") {
    attr(y,'variable') <- 'HDD'
    attr(y,'unit') <- 'degree-days'
  } else if (FUN=="CDD") {
    attr(y,'variable') <- 'CDD'
    attr(y,'unit') <- 'degree-days'
  } else if (FUN=="GDD") {
    attr(y,'variable') <- 'GDD'
    attr(y,'unit') <- 'degree-days'
  } else attr(y,'unit') <- attr(x,'unit')

  attr(y,'history') <- history.stamp(y)
  return(y)
}

aggregate.comb <- function(x,by,FUN = 'mean', ...,
                              regular = NULL, frequency = NULL) {
  # Also need to apply the aggregation to the appended fields
  print("aggregate.comb")
  #print(class(x))

  if (inherits(FUN,'function')) FUN <- deparse(substitute(FUN)) # REB140314
  if (deparse(substitute(by))=="year")
    by <- as.Date(strptime(paste(year(x),1,1,sep='-'),'%Y-%m-%d'))
  
  x <- aggregate.field(x,by=by,FUN=FUN, ...,
                       regular = regular, frequency = frequency)
  n <- attr(x,'n.apps')

  print("appended fields...")
  for (i in 1:n) {
    eval(parse(text=paste("z <- attr(x,'appendix.",i,"')",sep="")))
    #print(class(z)); print(by); print(index(z))
    z <- aggregate(z,by=by,FUN=FUN, ...,
                   regular = regular, frequency = frequency)
    print("update appended field")
    eval(parse(text=paste("z -> attr(x,'appendix.",i,"')",sep="")))
  }
  return(z)
}

aggregate.field <- function(x,by,FUN = 'mean', ...,
                              regular = NULL, frequency = NULL) {

  class(x) -> cls
  print(class(index(x)))
  class(x) <- "zoo"
  if (inherits(FUN,'function')) FUN <- deparse(substitute(FUN)) # REB140314
  #str(X); plot(X)
  #print("here...")
  #print(class(by))
  
  if (!is.list(by)) {
  # Temporal aggregation:
    #print("HERE")
    clsy2 <- switch(deparse(substitute(by)),
                         "as.yearmon"="month",
                         "as.yearqtr"="quarter",
                         "as.annual"="annual",
                         "year"="annual",
                         "by" = "by")
    if (is.null(clsy2)) clsy2 <- deparse(substitute(by))
    #print(clsy2)
    if (deparse(substitute(by))=="year") 
      by <- as.Date(strptime(paste(year(x),1,1,sep='-'),'%Y-%m-%d'))

    #print(deparse(substitute(by)))
    #print(class(x))
    #print(class(index(x)))
    #print('aggregate')
    y <- aggregate(x, by, match.fun(FUN), ...)
    class(x) <- cls; class(y) <- cls
    class(y)[2] <- clsy2
    
    y <- attrcp(x,y)
    #nattr <- softattr(x)
    #for (i in 1:length(nattr))
    #  attr(y,nattr[i]) <- attr(x,nattr[i])
    attr(y,'dimensions') <- c(attr(x,'dimensions')[-3],length(index(y)))
    #attr(y,'history') <- c(attr(x,'history'),'aggregate')
    #attr(y,'date-stamp') <- date()
    #attr(y,'call') <- match.call()
    attr(y,'history') <- history.stamp(x)
    return(y)
  } else {
    #print("there")
  # spatial aggregation
    dx <- by[[1]]; dy=by[[2]]
    clsx <- class(x)
    t <- index(x)
    d <- attr(x,'dimensions')
    lon <- dx*floor(attr(x,'longitude')/dx)
    lat <- dy*floor(attr(x,'latitude')/dy)
    xy <- paste(rep(lon,d[2]),sort(rep(lat,d[1])),sep="/")
#    xy <- paste(sort(rep(lon,d[2])),rep(lat,d[1]),sep="/")
    lon <- as.numeric(rownames(table(lon)))
    lat <- as.numeric(rownames(table(lat)))
    D <-  c(length(lon),length(lat),length(t))
    print(paste("spatial aggregation:",d[1],"x",d[2]," ->",D[1],"x",D[2]))
    Z <- t(coredata(x))
    rownames(Z) <- xy   
    z <- aggregate(Z,by=list(xy), match.fun(FUN),simplify=TRUE)
    z <- as.matrix(z); z <- z[,-1]
    z <- as.numeric(z)
    dim(z) <- c(D[1]*D[2],D[3])
    y <- zoo(t(as.matrix(z)),order.by=t)
    y <- attrcp(x,y)
    #nattr <- softattr(x)
    #for (i in 1:length(nattr))
    #  attr(y,nattr[i]) <- attr(x,nattr[i])
    attr(y,'dimensions') <- D
    attr(y,'latitude') <- lat
    attr(y,'longitude') <-lon
    #attr(y,'history') <- c(attr(x,'history'),'aggrgate')
    #attr(y,'date-stamp') <- date()
    #attr(y,'call') <- match.call()
    attr(y,'history') <- history.stamp(x)
    class(y) <- clsx
    return(y)
    }
}


aggregate.area <- function(x,is=NULL,it=NULL,FUN='sum',
                           na.rm=TRUE,smallx=FALSE) {
  # Estimate the area-aggregated values, e.g. the global mean (default)
    x <- subset(x,is=is,it=it)
   if (inherits(FUN,'function')) FUN <- deparse(substitute(FUN)) # REB140314
  d <- attr(x,'dimensions')
  #image(attr(x,'longitude'),attr(x,'latitude'),area)
  #print(c(length(colSums(area)),length(attr(x,'latitude')),sum(colSums(area))))
  lon <- rep(attr(x,'longitude'),d[2])
  lat <- sort(rep(attr(x,'latitude'),d[1]))
  aweights <- cos(pi*lat/180)
  aweights <- aweights/mean(aweights) 
#  area <- cos(pi*lat/180); dim(area) <- d[1:2]
#  area <- area/sum(area)
  if (smallx) {
    X <- coredata(x)%*%diag(aweights)
    y <- zoo(apply(X,1,FUN,na.rm=na.rm),order.by=index(x))
  } else {
    X <-coredata(x) 
    for (i in 1:d[3]) X[i,] <- X[i,]*aweights
    y <- zoo(apply(X,1,FUN,na.rm=na.rm),order.by=index(x))
  }

  Y <- as.station(y,loc=attr(x,'source'),param=attr(x,'variable'),
                  unit=attr(x,'unit'),
                  lon=range(lon(x)),lat=range(lat(x)),alt=NA,cntr=NA,
                  longname=paste(FUN,attr(x,'longname')),stid=NA,quality=NA,
                  src=attr(x,'source'),url=attr(x,'url'),
                  reference=attr(x,'reference'),info=attr(x,'info'),
                  method=paste(FUN,attr(x,'method')),type='area aggregate',
                  aspect=attr(x,'aspect'))
  attr(Y,'history') <- history.stamp(x)
  return(Y)
}

