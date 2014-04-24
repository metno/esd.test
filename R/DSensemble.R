# R.E. Benestad - replacement for old ds.one to downscale the CMIP 5 ensemble
# seasonal mean and standard deviation fortemperature
#

ar1 <- function(x,...) acf(x,plot=FALSE,na.action = na.pass)$acf[2]

ltp <- function(x,type='exponential',...) {
  # Rybski et al. (2006), i:10.1029/2005GL025591
  ar <- acf(x,plot=FALSE)$acf
  n <- min(11,(1:length(ar))[ar <= 0])-1
  gamma <- log(ar[1:n])
  l <- 1:length(gamma)
#  qfit <- lm(gamma ~ l + I(l^2))
  qfit <- lm(gamma ~ l)
  gamma <- qfit$coefficients[2]
  if (type=='exponential') y <- gamma else
  if (type=='hurst') y <- 1 - gamma/2
  return(y)
}


DSensemble<-function(y,...) UseMethod("DSensemble")

DSensemble.default <- function(y,path='CMIP5.monthly/',rcp='rcp45',...) {

  stopifnot(!missing(y),inherits(y,"station"),file.exists(paste(path,rcp,sep="")))
  
  if (is.null(attr(y,'aspect'))) attr(y,'aspect') <- "original"
  if (sum(is.element(attr(y,'variable'),c('t2m','tmax','tmin')))>0)
    z <- DSensemble.t2m(y,path=path,rcp=rcp,...) else
  if (attr(y,'variable')=='precip')
    z <- DSensemble.precip(y,path=path,rcp=rcp,threshold=1,...) else
    z <- NULL
  return(z)
}

DSensemble.t2m <- function(y,plot=TRUE,path="CMIP5.monthly/",
                           reanalysis="ERA40_t2m_mon.nc",
                           rcp="rcp45",biascorrect=FALSE,non.stationarity.check=FALSE,
                           eofs=1:6,lon=c(-10,10),lat=c(-10,10),
                           select=NULL,FUN="mean",FUNX="mean",
                           pattern="tas_Amon_ens_",verbose=FALSE) {

  #print("predictand")
  #if ( (deparse(substitute(FUN))=='sd') | (deparse(substitute(FUN))=='ar1') )
  if ((FUN=='sd') | (FUN =='ar1'))
    y <- anomaly(y)
  
  ya <- annual(y,FUN=FUN)
  y <- as.4seasons(y,FUN=FUN)
  

#  yr <- as.4seasons(ya,FUN=ltp)
  if (!is.na(attr(y,'longitude'))) lon <- round( attr(y,'longitude') + lon )
  if (!is.na(attr(y,'latitude'))) lat <- round( attr(y,'latitude') + lat )
  #lon <- round( attr(y,'longitude') + lon )
  #lat <- round( attr(y,'latitude') + lat )

  # The units
  if ( (attr(y,'unit') == "deg C") | (attr(y,'unit') == "degree Celsius") )
        unit <- expression(degree*C) else
        unit <- attr(y,'unit')

  #ylim <- switch(deparse(substitute(FUN)),
  ylim <- switch(FUN,
                 'mean'=c(-2,8),'sd'=c(-0.5,1),'ar1'=c(-0.5,0.7))
  if (plot) {
    par(bty="n")
    plot.zoo(ya,type="b",pch=19,main=attr(y,'location'),
             xlab="year",ylab=unit,
             sub=paste('Station: ',attr(y,'station_id'),'; coordinates: ',
             attr(y,'longitude'),'E/',attr(y,'latitude'),'N; ',
             attr(y,'altitude'),'m.a.s.l',sep=''),
             ylim=ylim + range(coredata(ya),na.rm=TRUE),xlim=c(1900,2100))
    grid()
  }

  # Get the predictor: ERA40
  #print("reanalysis")
  if (is.character(reanalysis))
    t2m <- retrieve.ncdf4(ncfile=reanalysis,lon=lon,lat=lat) else
  if (inherits(reanalysis,'field')) t2m <- subset(reanalysis,is=list(lon=lon,lat=lat))
 
  #rm("reanalysis"); gc(reset=TRUE)
  #t2m <- t2m.ERA40(lon=lon,lat=lat)
  T2M <- as.4seasons(t2m,FUN=FUNX)
  rm("t2m"); gc(reset=TRUE)

  # Ensemble GCMs
  path <- file.path(path,rcp,fsep = .Platform$file.sep)
  ncfiles <- list.files(path=path,pattern=pattern,full.name=TRUE)
  N <- length(ncfiles)

  if (is.null(select)) select <- 1:N else
                       N <- length(select)
  if (verbose) print(ncfiles[select])

  # set up results matrix and tables of diagnostics:
  years <- sort(rep(1900:2100,4))
  months <- rep(c(1,4,7,10),length(1900:2100))
  m <- length(years)
  X <- matrix(rep(NA,N*m),N,m)
  gcmnm <- rep("",N)
  scorestats <- matrix(rep(NA,N*8),N,8)
  colnames(scorestats) <- c("r.xval","mean.diff","sd.ratio","autocorr.ratio",
                            "res.trend","res.K-S","res.ar1",'amplitude.ration')

  t <- as.Date(paste(years,months,'01',sep='-'))

  cols <- rainbow(100)

  if (verbose) print("loop...") 
  for (i in 1:N) {
    gcm <- retrieve.ncdf4(ncfile = ncfiles[select[i]],
                          lon=range(lon(T2M)),lat=range(lat(T2M)))
    gcmnm[i] <- attr(gcm,'model_id')

    GCM <- as.4seasons(gcm,FUN=FUNX)
    rm("gcm"); gc(reset=TRUE)
    Z <- combine(T2M,GCM)
    rm("GCM"); gc(reset=TRUE)
     
    if (verbose) print("- - - > DS")
    ds <- DS(y,Z,biascorrect=biascorrect,eofs=eofs,verbose=verbose)
    if (verbose) print("post-processing")
    z <- attr(ds,'appendix.1')
    i1 <- is.element(paste(years,months,sep='-'),paste(year(z),month(z),sep='-'))
    i2 <- is.element(paste(year(z),month(z),sep='-'),paste(years,months,sep='-'))
    #browser()
    X[i,i1] <- z[i2]

    # Diagnose the residual: ACF, pdf, trend. These will together with the
    # cross-validation and the common EOF diagnostics provide a set of
    # quality indicators.
    cal <- coredata(attr(ds,"original_data"))
    fit <- coredata(attr(ds,"fitted_values"))
    res <- as.residual(ds)
    res.trend <- 10*diff(range(trend(res)))/diff(range(year(res)))
    ks <- round(ks.test(coredata(res),pnorm)$p.value,4)
    ar <- as.numeric(acf(trend(cal-fit,result="residual"),plot=FALSE)[[1]][2]) ##ar <- ar1(coredata(res))
    if (verbose) print(paste("Examine residuals: trend=",res.trend,'D/decade; K.S. p-val',
                             ks,'; AR(1)=',ar))

    # Evaluation: here are lots of different aspects...
    # Get the diagnostics: this is based on the analysis of common EOFs...

    xval <- attr(ds,'evaluation')
    r.xval <- round(cor(xval[,1],xval[,2]),3)
    if (verbose) print(paste("x-validation r=",r.xval))
    
    dsa <- annual(ds)                     # reanalysis
    xy <- merge.zoo(annual(z),ya)
    ds.ratio <- round(sd(xy[,1],na.rm=TRUE)/sd(xy[,2],na.rm=TRUE),4)
    if (verbose) print(paste("sd ratio=",ds.ratio))

    #print(names(attributes(ds)))
    if (biascorrect) {
      diag <- attr(ds,'diagnose')
      if ( (verbose) & !is.null(diag)) str(diag)
    } else diag <- NULL
    
    # diagnose for ds-objects

    if (verbose) print('...')
    if (is.null(diag)) {
      diag <- diagnose(z,plot=FALSE)
      scorestats[i,] <- c(1-r.xval,NA,NA,NA,res.trend,ks,ar,ds.ratio)
      mdiff <- (mean(subset(ya,it=range(year(dsa))),na.rm=TRUE)-
                mean(subset(dsa,it=range(year(ya))),na.rm=TRUE))/sd(ya,na.rm=TRUE)
      srati <- sd(subset(dsa,it=range(year(ya))),na.rm=TRUE)/
               sd(subset(ya,it=range(year(dsa))),na.rm=TRUE)
      arati <- ar1(dsa)/ar1(ya)
    } else {

    # Extract the mean score for leading EOF from the 4 seasons:
      mdiff <- mean(c(diag$s.1$mean.diff[1]/diag$s.1$sd0[1],
                      diag$s.2$mean.diff[1]/diag$s.2$sd0[1],
                      diag$s.3$mean.diff[1]/diag$s.3$sd0[1],
                      diag$s.4$mean.diff[1]/diag$s.4$sd0[1]))
      srati <- mean(1 - c(diag$s.1$sd.ratio[1],diag$s.2$sd.ratio[1],
                          diag$s.3$sd.ratio[1],diag$s.4$sd.ratio[1]))
      arati <- mean(1 - c(diag$s.1$autocorr.ratio[1],diag$s.2$autocorr.ratio[1],
                          diag$s.3$autocorr.ratio[1],diag$s.4$autocorr.ratio[1]))
    }
    scorestats[i,] <- c(1-r.xval,mdiff,srati,arati,res.trend,ks,ar,ds.ratio)
    if (verbose) print(scorestats[i,])

    quality <- round(50 + 5*sum(scorestats[i,],na.rm=TRUE),2)
    if (verbose) print(paste("quality =",quality))
    quality[quality < 1] <- 1;quality[quality > 100] <- 100

    if (plot) {
      lines(annual(z),lwd=2,col=cols[quality])
      lines(ya,type="b",pch=19)
      lines(dsa,lwd=2,col="grey")
    }
    print(paste("i=",i,"GCM=",gcmnm[i],' x-valid cor=',round(r.xval,2),
                "R2=",round(100*sd(xval[,2])/sd(xval[,1]),2),'% ',
                'Common EOF: bias=',round(mdiff,2),' 1- sd1/sd2=',round(srati,3),
                "mean=",round(mean(coredata(y),na.rm=TRUE),2),'quality=',quality))
    
  }

  X <- zoo(t(X),order.by=t)
  #X <- attrcp(y,X)
  attr(X,'station') <- y
  attr(X,'reanalysis') <- attr(T2M,'source')
  attr(X,'domain') <- list(lon=lon,lat=lat)
  attr(X,'scorestats') <- scorestats
  attr(X,'path') <- path
  attr(X,'scenario') <- rcp
  attr(X,'history') <- history.stamp(y)
  class(X) <- c("dsensemble","zoo")
  save(file="DSensemble.rda",X)
  print("---")
  invisible(X)
} 
#save(file=paste("dscmip5_",attr(y,'location'),"_",N,"_rcp4.5.rda",sep=""),rcp4.5)

DSensemble.precip <- function(y,plot=TRUE,path="CMIP5.monthly/",
                              rcp="rcp45",biascorrect=FALSE,
                              reanalysis="ERA40_pr_mon.nc",non.stationarity.check=FALSE,
                              eofs=1:6,lon=c(-10,10),lat=c(-10,10),
                              select=NULL,FUN="exceedance",
                              FUNX="sum",threshold=1,
                              pattern="pr_Amon_ens_",verbose=FALSE) {
  # FUN: exceedance, wetfreq, wet, dry

  if (verbose) print('DSensemble.precip')
#  if (deparse(substitute(FUN))=='spell') {
  if ( (FUN=='wet') | (FUN=='dry')) {
    y <- spell(y,threshold=threshold)
    y <- annual(y)
    #plot(y); browser()
    y <- switch(FUN,'wet'=subset(y,is=1),'dry'=subset(y,is=2))
  } else
#    y <- annual(y,FUN=FUN,threshold=threshold)
    y <- aggregate(y,year,FUN=FUN,threshold=1)
  #browser()
  index(y) <- year(y)
  
  if (!is.na(attr(y,'longitude'))) lon <- round( attr(y,'longitude') + lon )
  if (!is.na(attr(y,'latitude'))) lat <- round( attr(y,'latitude') + lat )
  
  # Get the predictor: ERA40
  if (verbose) print("reanalysis")
  if (is.character(reanalysis))
    pre <- retrieve.ncdf4(ncfile=reanalysis,param='TP',lon=lon,lat=lat) else
  if (inherits(reanalysis,'field')) pre <- reanalysis
  rm("reanalysis"); gc(reset=TRUE)
  attr(pre,"source") <- "ERA40"

  # Use proportional variaions
  if (verbose) print("Annual mean")
  PREX <- annual(pre,FUN=FUNX)
  #print("estimate %-changes")
  PRE.ref <- subset(PREX,it=1961:1990)
#  PRE <- zoo(100*coredata(PREX)/colMeans(coredata(PRE.ref)),order.by=year(PREX))
  PRE <- zoo(100*coredata(PREX)/mean(c(coredata(subset(PREX,it=1961:1990)))),order.by=year(PREX))
  PRE <- attrcp(PREX,PRE)
  attr(PRE, "unit" ) <- "%"
  attr(PRE, "dimensions" ) <- attr(PREX, "dimensions" )
  class(PRE) <- class(PREX)
  rm("PREX"); gc(reset=TRUE)
  
  if (verbose) print("graphics")
  cols <- rainbow(100)
  unit <- attr(y,'unit')
  #ylim <- switch(deparse(substitute(FUN)),
  ylim <- switch(FUN,
                 'exceedance'=c(-10,50),'wetfreq'=c(0,0),'spell'=c(0,0),
                 'mean'=c(-10,50),'sd'=c(-5,10),'ar1'=c(-0.5,0.7))
  if (plot) {
    par(bty="n")
    plot.zoo(y,type="b",pch=19,main=attr(y,'location'),
             xlab="year",ylab=unit,
             sub=paste('Station: ',attr(y,'station_id'),'; coordinates: ',
             attr(y,'longitude'),'E/',attr(y,'latitude'),'N; ',
             attr(y,'altitude'),'m.a.s.l',sep=''),
             ylim=ylim + range(coredata(y),na.rm=TRUE),xlim=c(1900,2100))
    grid()
  }

  # Ensemble GCMs
  path <- file.path(path,rcp,fsep = .Platform$file.sep)
  ncfiles <- list.files(path=path,pattern=pattern,full.name=TRUE)
  N <- length(ncfiles)

  if (is.null(select)) select <- 1:N else
                       N <- length(select)
  if (verbose) print(ncfiles[select])

  # set up results matrix and tables of diagnostics:
  years <- sort(1900:2100)
  m <- length(years)
  X <- matrix(rep(NA,N*m),N,m)
  gcmnm <- rep("",N)
  scorestats <- matrix(rep(NA,N*8),N,8)
  colnames(scorestats) <- c("r.xval","mean.diff","sd.ratio","autocorr.ratio",
                            "res.trend","res.K-S","res.ar1",'amplitude.ration')

  
  for (i in 1:N) {
    #browser()
    gcm <- retrieve.ncdf4(ncfile = ncfiles[select[i]],
                          lon=range(lon(PRE)),lat=range(lat(PRE)))
    gcmnm[i] <- attr(gcm,'model_id')

    GCMX <- annual(gcm,FUN=FUNX)
#    GCM <- zoo(100*coredata(GCMX)/colMeans(coredata(subset(GCMX,it=1961:1990))),order.by=year(GCMX))
    GCM <- zoo(100*coredata(GCMX)/mean(c(coredata(subset(GCMX,it=1961:1990)))),order.by=year(GCMX))
    GCM <- attrcp(GCMX,GCM)
    attr(GCM, "unit" ) <- "%"
    attr(GCM, "dimensions" ) <- attr(GCMX, "dimensions" )
    class(GCM) <- class(GCMX)
    #browser()
    #str(GCM)
    rm("gcm","GCMX"); gc(reset=TRUE)
    if (verbose) print("combine")
    #browser()
    PREGCM <- combine(PRE,GCM)
    if (verbose) print("EOF")
    Z <- EOF(PREGCM)
    rm("GCM"); gc(reset=TRUE)
    
    if (verbose) print("diagnose")
    diag <- diagnose(Z)
    if (biascorrect) Z <- biasfix(Z)
    if (verbose) print("- - - > DS")
    ds <- DS(y,Z,eofs=eofs,verbose=verbose)
    if (verbose) print("post-processing")
    z <- attr(ds,'appendix.1')
    i1 <- is.element(years,year(z))
    i2 <- is.element(year(z),years)
    #browser()
    X[i,i1] <- z[i2]

    # Diagnose the residual: ACF, pdf, trend. These will together with the
    # cross-validation and the common EOF diagnostics provide a set of
    # quality indicators.
    cal <- coredata(attr(ds,"original_data"))
    fit <- coredata(attr(ds,"fitted_values"))
    res <- as.residual(ds)
    res.trend <- 10*diff(range(trend(res)))/diff(range(year(res)))
    ks <- ks.test(coredata(res),pnorm)$p.value
    ar <- as.numeric(acf(trend(cal-fit,result="residual"),plot=FALSE)[[1]][2])
    ## ar <- ar1(coredata(res))

    # Evaluation: here are lots of different aspects...
    # Get the diagnostics: this is based on the analysis of common EOFs...

    xval <- attr(ds,'evaluation')
    r.xval <- cor(xval[,1],xval[,2])

    #browser()
    xy <- merge.zoo(z,y)
    ds.ratio <- sd(xy[,1],na.rm=TRUE)/sd(xy[,2],na.rm=TRUE)
    
    # Extract the mean score for leading EOF from the 4 seasons:
    mdiff <- diag$mean.diff[1]/diag$sd0[1]
    srati <- 1 - diag$sd.ratio[1]
    arati <- 1 - diag$autocorr.ratio[1]
    scorestats[i,] <- c(1-r.xval,mdiff,srati,arati,res.trend,ks,ar,ds.ratio)

    quality <- 50 + 5*sum(scorestats[i,])
    quality[quality < 1] <- 1;quality[quality > 100] <- 100

    index(z) <- year(z); index(ds) <- year(ds)
    if (plot) {
      lines(z,lwd=2,col=cols[quality])
      lines(y,type="b",pch=19)
      lines(ds,lwd=2,col="grey")
    }
    #browser()
    print(paste("i=",i,"GCM=",gcmnm[i],' x-valid cor=',round(r.xval,2),
                "R2=",round(100*sd(xval[,2])/sd(xval[,1]),2),'% ',
                'Common EOF: bias=',round(mdiff,2),' 1- sd1/sd2=',round(srati,3),
                "mean=",round(mean(coredata(y),na.rm=TRUE),2),'quality=',quality))
    
  }

  #browser()
  X <- zoo(t(X),order.by=years)
  #X <- attrcp(y,X)
  attr(X,'station') <- y
  attr(X,'reanalysis') <- attr(PRE,'source')
  attr(X,'domain') <- list(lon=lon,lat=lat)
  attr(X,'scorestats') <- scorestats
  attr(X,'path') <- path
  attr(X,'scenario') <- rcp
  attr(X,'history') <- history.stamp(y)
  class(X) <- c("dsensemble","zoo")
  save(file="DSensemble.rda",X)
  print("---")
  invisible(X)
}
           


