# Rasmus Benestad
# Estimate the naomaly and climatology
# Store the monthly climatology as an attribute (12,np)

anomaly <-function(x,...) UseMethod("anomaly")

anomaly.default <- function(x,...) {
  if (inherits(x,'annual')) y <- anomaly.annual(x,...) else
  if (inherits(x,'month')) y <- anomaly.month(x,...) else
  if (inherits(x,'day')) y <- anomaly.day(x,...) else
  if (inherits(x,'season')) y <- anomaly.season(x,...) else
  y <- as.annual(x,...)
  return(y)
}

anomaly.field <- function(x,...) {
  stopifnot(inherits(x,"field"))
  x <- as.anomaly(x)
  return(x)
}

anomaly.comb <- function(x,...) {
  stopifnot(inherits(x,"field"),inherits(x,"comb"))
  y <- anomaly(x)
  n.apps <- attr(x,'n.apps')
  for (i in 1:n.apps) {
    eval(parse(text=paste("z <- attr(x,'appendix.",i,"')",sep="")))
    Z <- anomaly(z)
    eval(parse(text=paste("Z -> attr(x,'appendix.",i,"')",sep="")))
  }
  y <- attrcp(x,y)
  n.apps -> attr(x,'n.apps')
  attr(x,'history') <- history.stamp(x)
  return(y)
}


anomaly.station <- function(x,...) {
#  t <- index(X)[1:2]
#  datetype <- class(t)
#  if (!is.null(attr(x,'anomaly'))) {
#    orig <- coredata(X)
#    x <- zoo(attr(X,'anomaly'),order.by=index(X))
#    nattr <- softattr(X)
#    for (i in 1:length(nattr))
#      attr(x,nattr[i]) <- attr(X,nattr[i])
#    eval(parse(text=paste("attr(x,'",attr(X,'aspect'),"') <- orig")))
#    attr(x,'aspect') <- 'anomaly'
#    return(x)
#  }
#
#  if (datetype=="Date") {
#    dy <- diff(as.numeric(format(t,'%Y')))
#    dm <- diff(as.numeric(format(t,'%m')))
#    dd <- diff(as.numeric(format(t,'%d')))
#  } else if (datetype=="numeric") {
#    dy <- 1; dm <- 0; dd <- 0
#  }
#  if ((dy==1) & (dy==0) & (dd==0))
#    x <- anomaly.yearly(X) else
#  if ((dy==0) & (dm==1) & (dd==0))
#    x <- anomaly.monthly(X) else 
#  if ((dy==0) & (dm==0) & (dd==1))
#    x <- anomaly.daily(X)
#  attr(x,'history') <- history.stamp(X)
  x <- as.anomaly(x)
  return(x)
}

anomaly.annual <- function(x,ref=1961:1990) {
  X <- x
  t <- index(X)
  x <- coredata(X)
  datetype <- class(t)
  if (datetype=="Date") years <- as.numeric(fomat(t),'%Y') else
  if (datetype=="numeric") years <- t
  clim <- mean(x[is.element(years,ref)],na.rm=TRUE)
  x <- x - clim
  x <- zoo(x,order.by=t)
  x <- attrcp(X,x)
  #nattr <- softattr(X)
  #for (i in 1:length(nattr))
  #  attr(x,nattr[i]) <- attr(X,nattr[i])
  attr(x,'climatology') <- clim
  attr(x,'aspect') <- 'anomaly'
  attr(x,'history') <- history.stamp(X)
  class(x) <- class(X)
  return(x)
}

anomaly.month <- function(x,ref=NULL) {

# This function computes the anomalies by removing the 12-month seasonal cycle
  X <- x
  t <- index(x)
  x <- coredata(x)
  l <- length(x); n <- ceiling(l/12)
  # If the record is not full years, pad the extra months of the last year
  pad <- l %% 12
  if (pad>0) x <- c(rep(NA,pad),x)
  #Fast way to compute the climatology: clim
  dim(x) <- c(12,n)
  clim <- rowMeans(x,na.rm=TRUE)
  x <- c(x - clim)
  if (pad>0) x <- x[-(1:pad)]
  x <- zoo(x,order.by=t)
  x <- attrcp(X,x)
  #nattr <- softattr(X)
  #for (i in 1:length(nattr))
  #  attr(x,nattr[i]) <- attr(X,nattr[i])
  attr(x,'climatology') <- clim
  attr(x,'aspect') <- 'anomaly'
  class(x) <- class(X)
  return(x)
}


anomaly.season <- function(x,ref=NULL) {

# This function computes the anomalies by removing the 12-month seasonal cycle
  X <- x
  t <- index(x)
  years <- year(t)
  x <- coredata(x)
  l <- length(x); n <- ceiling(l/4)
  # If the record is not full years, pad the extra months of the last year
  pad <- l %% 4
  if (pad>0) x <- c(rep(NA,pad),x)
  #Fast way to compute the climatology: clim
  dim(x) <- c(4,n)
  clim <- rowMeans(x,na.rm=TRUE)
  x <- c(x - clim)
  if (pad>0) x <- x[-(1:pad)]
  x <- zoo(x,order.by=t)

  x <- attrcp(X,x)
  #nattr <- softattr(X)
  #for (i in 1:length(nattr))
  #  attr(x,nattr[i]) <- attr(X,nattr[i])
  attr(x,'climatology') <- clim
  attr(x,'aspect') <- 'anomaly'
  class(x) <- class(X)
  return(x)
}


anomaly.day <- function(x,ref=NULL) {
  X <- x
# This function computes the anomalies by a best-fit regression to the first
# 4 hermonics of the 365.25-day cycle.
  t <- as.numeric(as.Date(index(X)))
  x <- coredata(X)
  l <- length(x)
  ndy <- switch(attr(x,'calendar'),
                'gregorian'=365.25)
  c1 <- cos(2*pi*t/ndy); s1 <- sin(2*pi*t/ndy)
  c2 <- cos(4*pi*t/ndy); s2 <- sin(4*pi*t/ndy)
  c3 <- cos(6*pi*t/ndy); s3 <- sin(6*pi*t/ndy)
  c4 <- cos(8*pi*t/ndy); s4 <- sin(8*pi*t/ndy)
  clim.fit <- lm(x ~ c1 + s1 + c2 + s2 + c3 + s3 + c4 + s4)
  clim <- predict(clim.fit)[1:366]
  attributes(clim) <- NULL
  x <- zoo(clim.fit$residual,order.by=index(X))
  
  x <- attrcp(X,x,ignore="names")
#  nattr <- softattr(X)
#  for (i in 1:length(nattr))
#    attr(x,nattr[i]) <- attr(X,nattr[i])
  attr(x,'climatology') <- clim
  attr(x,'aspect') <- 'anomaly'
  class(x) <- class(X)
  return(x)
}


climatology <- function(x,...) UseMethod("climatology")

climatology.default <- function(x) {
  x <- as.climatology(x)
  return(x)
}

climatology.field <- function(x) {
  x <- as.climatology(x)
  return(x)
}

climatology.station <- function(x) {
#  x <- X
#  orig <- coredata(X)
#  if (is.null(attr(x,'climatology'))) {
#    t <- index(X)[1:2]
#    dy <- diff(as.numeric(format(t,'%Y')))
#    dm <- diff(as.numeric(format(t,'%m')))
#    dd <- diff(as.numeric(format(t,'%d')))
#    if ((dy==1) & (dy==0) & (dd==0))
#      x <- anomaly.yearly(X) else
#    if ((dy==0) & (dm==1) & (dd==0)) 
#      y <- anomaly.monthly(X) 
#    if ((dy==0) & (dm==0) & (dd==1)) 
#      y <- anomaly.daily(X)
#    clim <- attr(y,'climatology')
#  } else clim <- attr(X,'climatology')
#
#  nc <- length(index(X))%/%length(clim)
#  pad <- length(index(X))%%length(clim)
#  clim <- rep(clim,nc)
#  if (pad>0) clim <- c(clim,clim[1:pad])
#    
#  x <- zoo(clim,order.by=index(X))
#  nattr <- softattr(X)
#  for (i in 1:length(nattr))
#      attr(x,nattr[i]) <- attr(X,nattr[i])
#  eval(parse(text=paste("attr(x,'",attr(X,'aspect'),"') <- orig")))
#  attr(x,'aspect') <- 'climatology'
  x <- as.climatology(x)
  return(x)
}





# Station data can be expressed as PCA where each of the EOFs represent one
# year. The PCs describe the seasonal variations

clim2pca <-function(x,...) UseMethod("clim2pca")

clim2pca.default <- function(x) {
}

clim2pca.month <- function(x) {
  X <- aggregate(x,year)
  ny <- length(x) %/% 12
  nm <- length(x) %% 12
  y <- coredata(x[1:(length(x)-nm)])
  dim(y) <- c(12,ny)
  ok <- is.finite(colMeans(y))
  pca <- svd(y[,ok])
  for (i in 1:12) {
    z <- zoo(pca$v[,i],order.by=index(X))
    if (i == 1) Z <- z else
                Z <- merge(Z,z)
  }
  season <- pca$u
  colnames(season) <- month.abb
  rownames(season) <- paste("pattern",1:12,sep=".")
  attr(Z,'season') <- season
  attr(Z,'d') <- pca$d
  return(Z)
}

clim2pca.day <- function(x) {
}


