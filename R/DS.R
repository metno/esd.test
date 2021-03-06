# Empirical downscaling using EOFs of monthly values from eof.R
# Predictand is a time series of monthly values from NACD or climate station.
#
# Reference: R.E. Benestad et al. (2002),
#            Empirically downscaled temperature scenarios for Svalbard,
#            doi.10.1006/asle.2002.005, September 18.
#
#            R.E. Benestad (2001),
#            A comparison between two empirical downscaling strategies,
#            Int. J. Climatology, 1645-1668, vol. 21, DOI 10.1002/joc.703
#
# R.E. Benestad, met.no, Oslo, Norway 11.04.2013
# rasmus.benestad@met.no
#------------------------------------------------------------------------

# dat -> y; preds -> X
# Needs to work also for daily, annual and seasonal data
#

sametimescale <- function(y,X,FUN='mean') {
  # Function to ensure that station y has the same time scales as X
  tsx <- class(X)[length(class(X))-1]
  tsy <- class(y)[length(class(y))-1]
  #print(c(tsx,tsy))
  index(y) <- as.Date(index(y))
  if (tsx==tsy) return(y)

  if (tsx=="day") agrscly <- as.Date(index(y)) else
  if (tsx=="month") agrscly <- as.yearmon(index(y)) else
  if (tsx=="annual") agrscly <- year(y) else
  if (tsx=="year") agrscly <- year(y)
  #str(agrscly)
  if (tsx !="season")
     y <- aggregate(y, agrscly, match.fun(FUN)) else 
     y <- as.4seasons(y, FUN=match.fun(FUN),dateindex=TRUE)
  return(y)
}



DS<-function(y,X,verbose=TRUE,...) UseMethod("DS")

# The basic DS-function, used by other methods
# REB HERE!

DS.default <- function(y,X,mon=NULL,
                       method="lm",swsm="step",
                       rmtrend=TRUE,eofs=1:7,area.mean.expl=FALSE,
                       verbose=FALSE,...) {

  if (verbose) print("DS.default")
  swapped <- FALSE
  if ( inherits(y,c("eof")) & inherits(X,c("station"))) {
    yy <- X
    X <- y
    y <- yy
    swapped <- TRUE
  }
  stopifnot(!missing(y),!missing(X), is.matrix(X),
            inherits(X,"eof"),inherits(y,"station"))
  
  y0 <- y
  X0 <- X 
  W <- attr(X,'eigenvalues')
  cls <- class(X)
  #print("index(y):");print(index(y)[1:24])
  
  if ( (is.character(index(y))) & (nchar(index(y)[1])==4) )
    index(y) <- as.numeric(index(y))
  if ( (is.character(index(X))) & (nchar(index(X)[1])==4) )
    index(X) <- as.numeric(index(y))
  if ( (is.character(index(y))) & (nchar(index(y)[1])==10) )
    index(y) <- as.Date(index(y))
  if ( (is.character(index(X))) & (nchar(index(X)[1])==10) )
    index(X) <- as.Date(index(y))
  if (class(index(y))=="numeric")
    index(y) <- as.Date(paste(index(y),'-01-01',sep=''))
  if (class(index(X))=="numeric")
    index(X) <- as.Date(paste(index(X),'-01-01',sep=''))
  
  y <- sametimescale(y,X)
  #print("index(y):");print(index(y)[1:24])
  
  if (!is.null(mon)) y <- station.subset(y,it=mon)
  
# synchronise the series: use the 'zoo' merge function through combine:
  #print(index(y)[1:24]); print(index(X)[1:24]);
  yX <- combine.station.eof(y,X)
  y <- yX$y; X <- yX$X
  year <- as.numeric( format(index(y), '%Y') ) 
  month <- as.numeric( format(index(y), '%m') )
  #print(length(y)); print(table(year)); print(table(month))
  
# De-trend the data used for model calibration:
  if (rmtrend) {
    offset <- mean(y,na.rm=TRUE)
    y <- trend(y,result="residual")
    offset <- offset - mean(y,na.rm=TRUE)
    X <- trend(X,result="residual")
  } else offset <- 0

  #str(y); print(class(y))
  caldat <- data.frame(y=coredata(y),X=as.matrix(coredata(X)))
  predat <- data.frame(X=as.matrix(coredata(yX$X)))
  colnames(predat) <- paste("X",1:length(colnames(predat)),sep=".")
  
  Xnames <- paste("X.",1:length(names(X)),sep="")
  colnames(caldat) <- c("y",Xnames)
  Xnames <- Xnames[eofs]
  calstr <- paste(method,"(y ~ ",paste(Xnames,collapse=" + "),
                  ", data=caldat, ...)",sep="")
  MODEL <- eval(parse(text=calstr))
  FSUM <- summary(MODEL)
  if (verbose) print(FSUM)

# Stepwise regression
  if (!is.null(swsm)) {
    cline <- paste("model <- ",swsm,"(MODEL,trace=0)",sep="")
    eval(parse(text=cline))
  } else
    model <- MODEL
  terms1 <- attr(model$terms,'term.labels')

  if (verbose) print(summary(model))

  fsum <- summary(model)
  COEFS=FSUM$coefficients
  COEFS[,1] <- 0; 
  COEFS[,2:4] <- NA; 
  coefs=fsum$coefficients
  TERMS <- attr(FSUM$terms,'term.labels')
  terms <- attr(fsum$terms,'term.labels')
  ii <- is.element(attr(COEFS,"dimnames")[[1]],attr(coefs,"dimnames")[[1]])
  COEFS[ii,1] <- coefs[,1]
  COEFS[ii,2] <- coefs[,2]
  COEFS[ii,3] <- coefs[,3]
  COEFS[ii,4] <- coefs[,4]
  dc <- dim(COEFS)

  U <- attr(X,'pattern'); du <- dim(U); dim(U) <- c(du[1]*du[2],du[3])
  pattern <- t(COEFS[2:dc[1],1]) %*%
               diag(attr(X,'eigenvalues')[eofs]) %*% t(U[,eofs])
  dim(pattern) <- c(du[1],du[2])
  
#  ds <- zoo(predict(model),order.by=index(X)) + offset
#  ds <- zoo(predict(model,newdata=caldat),order.by=index(X)) + offset
  ds <- zoo(predict(model,newdata=predat),order.by=index(X)) + offset
  #plot(y,lwd=4); lines(ds,col="red",lwd=2)
  #lines(zoo(model$fitted.values,order.by=index(ds)),col="blue",lwd=2)
  #lines(yX$y,col="darkgreen",lwd=2)
  
  #nattry <- softattr(y0)
  #nattrX <- softattr(X0,ignore=c('longitude','latitude')) 
  #for (i in 1:length(nattry))
  #  attr(ds,nattry[i]) <- attr(y0,nattry[i])
  #for (i in 1:length(nattrX))
  #  attr(pattern,nattrX[i]) <- attr(X0,nattrX[i])
  ds <- attrcp(y0,ds,ignore='names')
  pattern <- attrcp(X0,pattern,ignore=c('longitude','latitude','names'))
  
  caldat <- zoo(as.matrix(caldat),order.by=index(X))
  attr(caldat,'calibration_expression') <- calstr
  attr(caldat,'stepwise_screening') <- swsm
  attr(ds,'calibration_data') <- caldat
  attr(ds,'fitted_values') <- zoo(model$fitted.values +
                                  offset,order.by=index(X))
  class(attr(ds,'fitted_values')) <- class(y0)
  attr(ds,'original_data') <- yX$y
  r2 <- var(coredata(model$fitted.values))/var(y,na.rm=TRUE)
  attr(r2,'description') <- ' var(fitted.values))/var(y)'
  attr(ds,'quality') <- r2
  attr(ds,'variable') <- attr(y0,'variable')
  attr(ds,'model') <- model
  attr(ds,'mean') <- offset
  attr(ds,'method') <- method
  attr(ds,'eof') <- X0
  attr(pattern,'longitude') <- attr(X0,'longitude')
  attr(pattern,'latitude') <- attr(X0,'latitude')
  attr(ds,'pattern') <- pattern
  attr(ds,'dimensions') <- c(du[1],du[2])
  attr(ds,'longitude') <- attr(y0,'longitude')
  attr(ds,'latitude') <- attr(y0,'latitude')
  #attr(ds,'source') <- paste(attr(y0,'source'),attr(X0,'source'),sep="-")
  attr(ds,'aspect') <- 'downscaled'
  attr(ds,'source') <- attr(X0,'source')
  attr(ds,'type') <- "downscaled results"
  attr(ds,'history.predictand') <- attr(y0,'history')
  attr(ds,'history') <- history.stamp(X0)
  #print("HERE"); print(cls)
  class(ds) <- c("ds",cls)
  rm("y0","X0")
  #print("Completed")
  #lines(ds,col="darkred",lwd=2,lty=2)
  #lines(attr(ds,'original_data'),col="green",lwd=2,lty=2)
  invisible(ds)
}


DS.eof <- function(X,y,mon=NULL,
                   method="lm",swsm="step",
                   rmtrend=TRUE,eofs=1:7,area.mean.expl=FALSE,
                   verbose=FALSE,pca=TRUE,...) {
  #if (verbose) print("DS.eof")
  ds <- DS.default(y,X,mon=mon,
                   method=method,swsm=swsm,
                   rmtrend=rmtrend,eofs=eofs,
                   area.mean.expl=area.mean.expl,
                   verbose=verbose,...)
  return(ds)
}



DS.station <- function(y,X,biascorrect=FALSE,mon=NULL,
                   method="lm",swsm="step",
                   rmtrend=TRUE,eofs=1:7,area.mean.expl=FALSE,
                   verbose=FALSE,pca=FALSE,npca=20,...) {
  
  stopifnot(!missing(y),!missing(X),inherits(y,"station"))
  if (verbose) print("DS.station")
  
  if ( (!inherits(X,'eof')) & (inherits(X,'field')) ) {
    #print("HERE")
    ds <- DS.field(y=y,X=X,biascorrect=biascorrect,mon=mon,
                  method=method,swsm=swsm,
                  rmtrend=rmtrend,eofs=eofs,
                  area.mean.expl=area.mean.expl,verbose=verbose,...) 
    return(ds)
  }

  # REB inserted lines to accomodate for multiple stations
  d <- dim(y)
  if (!is.null(d)) ns <- d[2] else ns <- 1
  if (!is.null(d)) {
    if (verbose) print(paste('predictand contains',ns,'stations'))
    # More than one station
    Y <- y
    if (pca) {
      if (verbose) print("PCA")
      # PCA is used when y represents a group of variables and when
      # their covariance is a concern: it preserves the covariance
      # as seen in the past, as the PCs and eigenvectors are orthogonal.
      # Useful for spatial response, wind vectors, temperature/precipitation
      Y <- PCA(y,npca)
    }
    ns <- dim(Y)[2]
  } else Y <- y
  
  #print(class(Y)); print(class(X)); print(paste(ns,"stations"))

  # Loop over the different PCs or stations
  for (i in 1:ns) {
    if (verbose) print(paste("i=",i))
    z <- subset(Y,is=i)
    #str(z)
    #if (verbose) {print(class(z)); print(names(attributes(z)))}
    if (inherits(X,'eof')) {
      if (verbose) print("The predictor is some kind of EOF-object")
      # Call different functions, depending on the class of X:
      #print("the predictor is an EOF-object")
      if (inherits(X,'comb')) {
        if (verbose) print("*** Comb ***")
          # X is combined EOFs
          ds <- DS.comb(y=z,X=X,biascorrect=biascorrect,mon=mon,
                        method=method,swsm=swsm,
                        rmtrend=rmtrend,eofs=eofs,
                        area.mean.expl=area.mean.expl,verbose=verbose,...)
          if (verbose) print("---")
      } else if (inherits(X,'eof')) {
        if (verbose) print("*** EOF ***")
          # X is ordinary EOF
          ds <- DS.default(y=z,X=X,mon=mon,
                           method=method,swsm=swsm,
                           rmtrend=rmtrend,eofs=eofs,
                           area.mean.expl=area.mean.expl,
                           verbose=verbose,...)
          #if (verbose) print("+++")
      }
      } else if (inherits(X,'field')) {
          if (verbose) print("the predictor is an field-object")
          # X is a field
          ds <- DS.field(y=z,X=X,biascorrect=biascorrect,mon=mon,
                   method=method,swsm=swsm,
                   rmtrend=rmtrend,eofs=eofs,
                   area.mean.expl=area.mean.expl,verbose=verbose,...)
    }
    # May need an option for coombined field: x is 'field' + 'comb'
    if (verbose) print("Cross-validation")
    xval <- crossval(ds)
    attr(ds,'evaluation') <- zoo(xval)

    #if (verbose) print(names(attributes(ds)))
    if (ns==1) dsall <- ds else {
      if (i==1) dsall <- list(ds.1=ds) else
                eval(parse(text=paste('dsall$ds.',i,' <- ds',sep='')))
    }

  }
  #str(dsall)
  
  #print("---")
  #str(dsall)

  # If PCA was used to transform the predictands to preserve the
  # spatial covariance, then do the inverse to recover the results
  # in a structure comparable to the original stations.
  if ( (ns>1) & pca ) {
    attr(dsall,'pattern') <- attr(Y,'pattern')
    attr(dsall,'eigenvalues') <- attr(Y,'eigenvalues')    
    attr(dsall,'mean') <- attr(Y,'mean')    
    ds.results <- pca2station(dsall)
  } else ds.results <- dsall
  invisible(ds.results)  
}


# DS for combined fields - to make predictions not based on the
# calibration data

DS.comb <- function(X,y,biascorrect=FALSE,mon=NULL,
                    method="lm",swsm="step",
                    rmtrend=TRUE,eofs=1:7,area.mean.expl=FALSE,
                    verbose=FALSE,...) {

  if (verbose) print("DS.comb")
  if ( inherits(y,c("eof")) & inherits(X,c("station"))) {
    yy <- X
    X <- y
    y <- yy
    swapped <- TRUE
    rm("yy")
  }
  X0 <- X
  
  stopifnot(!missing(y),!missing(X),is.matrix(X),
            inherits(X,"comb"),inherits(y,"station"))
  
  # For combined fields/common EOFs, do the DS-fitting once, and then
  # use the model n times to predict the values associated with the
  # appended fields:

  
  if (!inherits(X,"eof")) X <- EOF(X,mon=mon,area.mean.expl=area.mean.expl)
  
  
  if (biascorrect) {
    if (verbose) print("Bias correcion - bias-fix common EOF")
    X <- biasfix(X)
  }
  
  ds <- DS.eof(X,y,mon=mon,
               method=method,swsm=swsm,
               rmtrend=rmtrend,eofs=eofs,
               area.mean.expl=area.mean.expl,verbose=verbose,...)

  # For combined fields, make sure to add the appended PCs to
  # the results.
  #print(names(attributes(X)))
  model <- attr(ds,'model')
  n.app <- attr(X,'n.apps')
  attr(ds,'n.apps') <- n.app
  for (i in 1:n.app) {
    #print("HERE")
    Z <- eval(parse(text=paste("attr(X0,'appendix.",i,"')",sep="")))
    #browser()
    newdata <- data.frame(X=Z)
    colnames(newdata) <- paste("X",1:length(colnames(X)),sep=".")
    #print(summary(newdata))
    z <- predict(model,newdata=newdata) + attr(ds,'mean')
    Y <- zoo(z,order.by=index(Z))
    Y <- attrcp(Z,Y)

    # Store in a similar structure as in the common EOF:
    eval(parse(text=paste("attr(ds,'appendix.",i,"') <- Y",sep="")))
  }
  rm("X0"); gc(reset=TRUE)
  invisible(ds)
}


# This function takes care of downscaling based on a field-object X.
# X can be a combined field. This function calls more primitive DS methods,
# depending on the time scale represented in X (monthly or seasonal).

DS.field <- function(X,y,biascorrect=FALSE,mon=NULL,
                     method="lm",swsm="step",
                     rmtrend=TRUE,eofs=1:7,area.mean.expl=FALSE,
                     verbose=FALSE,...) {
  if (verbose) print("DS.field")
  # Keep track of which is an eof object and which is a station record:
  swapped <- FALSE
  if ( inherits(y,c("field")) & inherits(X,c("station"))) {
    yy <- X
    X <- y
    y <- yy
    swapped <- TRUE
  }
  
  #print(class(X)); print(attr(y,'variable'))
 
  if (sum(is.element(tolower(attr(y,'variable')),c('t2m','tmax','tmin'))) >0) {
    if (inherits(X,'month')) 
      ds <- DS.t2m.month.field(y=y,X=X,biascorrect=biascorrect,
                         mon=mon,method=method,swsm=swsm,
                         rmtrend=rmtrend,eofs=eofs,
                         area.mean.expl=area.mean.expl,
                         verbose=verbose,...) else
    if (inherits(X,'season')) 
      ds <- DS.t2m.season.field(y=y,X=X,biascorrect=biascorrect,
                         method=method,swsm=swsm,
                         rmtrend=rmtrend,eofs=eofs,
                         area.mean.expl=area.mean.expl,
                         verbose=verbose,...) else
    if (inherits(X,'annual')) 
      ds <- DS.t2m.annual.field(y=y,X=X,biascorrect=biascorrect,
                         method=method,swsm=swsm,
                         rmtrend=rmtrend,eofs=eofs,
                         area.mean.expl=area.mean.expl,
                         verbose=verbose,...)
  } else if (tolower(attr(y,'variable'))=='precip')
    ds <- DS.precip.season.field(y=y,X=X,biascorrect=biascorrect,
                       method=method,swsm=swsm,
                       rmtrend=rmtrend,eofs=eofs,
                       area.mean.expl=area.mean.expl,verbose=verbose,...)
  invisible(ds)
}



# Downscales all 12-calendar months for a field by stepping through the months
# and compute the EOFs before applying the default DS method.
DS.t2m.month.field <- function(y,X,biascorrect=FALSE,mon=NULL,
                          method="lm",swsm="step",
                          rmtrend=TRUE,eofs=1:7,area.mean.expl=FALSE,
                          verbose=FALSE,station=TRUE) {
  if (verbose) print("DS.t2m.month.field")
  if (inherits(X,'comb')) type <- 'eof.comb' else type <- "eof.field"
  cls <- class(y)

  ds <- list()
  if (is.null(mon)) mon <-  1:12
  
  for (i in mon) {
    #print(month.name[i])
    eof <- EOF(X,it=i,area.mean.expl=area.mean.expl)
    if (biascorrect) eof <- biasfix(eof)
    cline <- paste("ds$",month.abb[i],
                 "<- DS.station(y,eof,method=method,swsm=swsm,",
                 "rmtrend=rmtrend,eofs=eofs,",
                 "area.mean.expl=area.mean.expl,verbose=verbose)",
                 sep="")
    if (verbose) print(cline)
    eval(parse(text=cline))
  }
  #print(summary(ds))

  if (station) ds <- combine.ds(ds) else
               cls <- c("list","dsfield",cls)

  #ds <- attrcp(y,ds)
  #nattr <- softattr(y)
  #for (i in 1:length(nattr))
  #  attr(ds,nattr[i]) <- attr(y,nattr[i])

  attr(ds,'predictand.class') <- cls
  #class(ds) <- c("list","dsfield",cls)
  invisible(ds)
}


DS.t2m.season.field <- function(y,X,biascorrect=FALSE,
                          method="lm",swsm="step",
                          rmtrend=TRUE,eofs=1:7,area.mean.expl=FALSE,
                          verbose=FALSE,station=TRUE) {
  # Downscale seasonal mean and standard deviation
  if (verbose) print("DS.t2m.season.field")
  #ya <- as.anomaly(y)
  #yc <- as.climatology(y) 
  #y.mean <- aggregate(as.4season(ya,FUN="mean"))
  #y.sd <- aggregate(as.4season(ya,FUN="sd"))
  #X.4s <- aggregate(as.4season(X,FUN="mean"))
  #str(X); browser()
  Z1 <- EOF(subset(X,it='djf'),area.mean.expl=area.mean.expl)
  if (verbose) print("downscale DJF")
  ds1 <- DS(y,Z1,biascorrect=biascorrect,eofs=eofs)
  Z2 <- EOF(subset(X,it='mam'),area.mean.expl=area.mean.expl)
  if (verbose) print("downscale MAM")
  ds2 <- DS(y,Z2,biascorrect=biascorrect,eofs=eofs)
  Z3 <- EOF(subset(X,it='jja'),area.mean.expl=area.mean.expl)
  if (verbose) print("downscale JJA")
  ds3 <- DS(y,Z3,biascorrect=biascorrect,eofs=eofs)
  
#  load('control.ds.1.rda'); Z3x <- EOF(PREGCM,area.mean.expl=area.mean.expl)
#  ds3 <- DS(y,Z3x,biascorrect=biascorrect,eofs=eofs)
  
  Z4 <- EOF(subset(X,it='son'),area.mean.expl=area.mean.expl)
  if (verbose) print("downscale SON")
  ds4 <- DS(y,Z4,biascorrect=biascorrect,eofs=eofs)
  if (verbose) print("Combine the 4 seasons")
  ds <- combine(list(ds1,ds2,ds3,ds4))
  z <- c(crossval(ds1),crossval(ds2),crossval(ds3),crossval(ds4))
  attr(ds,'evaluation') <- z
  save(file='inside.ds.seas.f.1.rda',y,Z1,ds1,Z2,ds2,Z3,ds3,Z4,ds4,X)
  #browser()
  invisible(ds)
}

DS.t2m.annual.field <- function(y,X,biascorrect=FALSE,
                          method="lm",swsm="step",
                          rmtrend=TRUE,eofs=1:7,area.mean.expl=FALSE,
                          verbose=FALSE,station=TRUE) {
  # Downscale seasonal mean and standard deviation
  if (verbose) print("DS.t2m.annual.field")
  #ya <- as.anomaly(y)
  #yc <- as.climatology(y) 
  #y.mean <- aggregate(as.4season(ya,FUN="mean"))
  #y.sd <- aggregate(as.4season(ya,FUN="sd"))
  #X.4s <- aggregate(as.4season(X,FUN="mean"))
  
  Z <- EOF(X,area.mean.expl=area.mean.expl)
  ds <- DS(y,Z,biascorrect=biascorrect)
  invisible(ds)
}



DS.precip.season.field <- function(y,X,biascorrect=FALSE,threshold=1,
                                   method="lm",swsm="step",
                                   rmtrend=TRUE,eofs=1:7,area.mean.expl=FALSE,
                                   verbose=FALSE,...) {

  # Computes the annual mean values for wet-day mean mu, wet-day frequency, and spell.
  # Also computes seasonal variations from PCA X[year,calendar months].
  # One PC for each year.

  mu <- as.4seasons(y,FUN="exceedance",threshold=threshold)
  fw <- as.4seasons(y,FUN="exceedance",fun="freq")
  wL <- as.4seasons(spell(y))

  if (!inhetits(X,'season')) X <- as.4seasons(X)

  for (i in 1:4) {
    x <- EOF(X,it=i,area.mean.expl=area.mean.expl)
    if (biascorrect) x <- biasfix(x)
    ds.mu <- DS.default(mu,x,method=method,swsm=swsm,
                        rmtrend=rmtrend,eofs=eofs,
                        verbose=verbose,...)
    ds.fw <- DS.freq(fw,x,rmtrend=rmtrend,eofs=eofs,
                        verbose=verbose,...)
    ds.wL <- DS.spell(wL,x,rmtrend=rmtrend,eofs=eofs,
                        verbose=verbose,...)
  }
  
  # DS mu, fw, spell, total
  ds <- NULL
  invisible(ds)
}

# Use family='gaussian' for sample sizes gt 30 - > central limit theorem
DS.freq <- function(y,X,threshold=1,biascorrect=FALSE,method="glm",
                    family="gaussian",swsm="step",
                    rmtrend=TRUE,eofs=1:7,verbose=FALSE,...) {
  if (inherits(X,'month')) Z <- aggregate(y,as.yearmon,FUN=wetfreq,threshold=threshold) else
  if (inherits(X,'season')) Z <- as.4seasons(y,FUN=wetfreq,threshold=threshold) else
  if (inherits(X,'annual')) Z <- annual(y,FUN=wetfreq,threshold=threshold)
  
  ds <- DS.default(Z,X,biascorrect=biascorrect,method=method,swsm=swsm,
           rmtrend=rmtrend,eofs=eofs,
           verbose=verbose,...)
  return(ds)
}


DS.spell <- function(y,X,threshold=1,biascorrect=FALSE,
                     method="glm",family="gaussian",swsm="step",
                    rmtrend=TRUE,eofs=1:7,verbose=FALSE,...) {
  # Downscale the spell length using a GLM with poisson family.
  # Thate the mean spell length over a given interval:
  if (inherits(y,'spell')) z <- as.station(y) else
  if (inherits(y,'sstation')) {
    z <- as.station(spell(y))
  }
    
  if (inherits(X,'month')) Z <- aggregate(z,as.yearmon,FUN=mean) else
  if (inherits(X,'season')) Z <- as.4seasons(z,FUN=mean) else
  if (inherits(X,'annual')) Z <- annual(z,FUN=mean)

  ds <- DS(Z,X,biascorrect=biascorrect,method=method,swsm=swsm,
           rmtrend=rmtrend,eofs=eofs,
           verbose=verbose,...)
  return(ds)
}


# DS.pca
# This function applies to PCA results for a group of stations.
# Typically some annual statistics (e.g. mu), stored in compressed form
# as a PCA-object (similar to EOF, but not gridded and without the area
# weighting.
# The data may be pre-filtered using CCA.
# Rasmus Benestad, 19.08.2013


DS.pca <- function(y,X,biascorrect=FALSE,mon=NULL,
                   method="lm",swsm=NULL,eofs=1:10,
                   rmtrend=TRUE,verbose=FALSE,...) {
  print('DS.pca. v2')
  cls <- class(y)
  Z <- y; pca <- X
  nattr <- softattr(y)

  # synchronise the two zoo objects through 'merge' (zoo)
  dy <- dim(y)
  dx <- dim(X)

  # Use method for downscaling
  #str(y); str(X)
  if (verbose) print(method)
  if (toupper(method)=='mvr') {
    
    if (is.null(dy)) dy <- c(length(y),1)
    if (dy[2]>1) colnames(y) <- paste("y",1:dy[2],sep=".")
    if (is.null(dx)) dx <- c(length(x),1)
    if (dx[2]>1) colnames(X) <- paste("X",1:dx[2],sep=".") 
  #colnames(y) <- paste("y",1:dim(y)[2],sep=".")
  #colnames(X) <- paste("X",1:dim(y)[2],sep=".")
    yX <- merge(y,X,all=FALSE)

    vars <- names(yX)
    ys <- vars[grep('y',vars)]
    Xs <- vars[grep('X',vars)]
    ix <- is.element(vars,Xs)
    iy <- is.element(vars,ys)
    x <- zoo(coredata(yX[,ix]),order.by=index(yX))
    y <- zoo(coredata(yX[,iy]),order.by=index(yX))
    class(y) <- cls

    print('WARNING: does not operate on combined objects')
    model <- eval(parse(text=paste(method,"(y,x)",sep="")))
      # Reconstruct the entire data, and then apply the PCA to this
    V <- predict(model); W <- attr(Z,'eigenvalues')
    U <- attr(Z,'pattern')
    UWV <- U %*% diag(W) %*% t(V)
    pca <- svd(UWV)
    #str(pca)
    ds <- zoo(pca$v,order.by=index(X))
    model$fitted.values <- zoo(model$fitted.values,order.by=index(X)) +
#    attr(Z,'mean') + offset  # REB 04.12.13: included attr(Z,'mean') in
#                             # addition to offset
    model$calibration.data <- X
    class(model$fitted.values) <- class(Z)
    attr(ds,'model') <- model
    attr(ds,'quality') <- var(coredata(model$fitted.values))/var(y,na.rm=TRUE)
    attr(ds,'eigenvalues') <- pca$d
    attr(ds,'sum.eigenv') <- sum(pca$d)
    attr(ds,'pattern') <- pca$u
  } else {
      # treat each PC as a station
    if (verbose) print('Prepare output data')
    y.out <- matrix(rep(NA,dx[1]*dy[2]),dx[1],dy[2])
    # If common EOFs, then accomodate for the additional predictions
    if (!is.null(attr(Z,'appendix.1')))
      y.app <- attr(Z,'appendix.1')

    if (verbose) print(eofs)
    # For each PC
    
    for (i in 1:dy[2]) {
      ys <- as.station(zoo(y[,i]),loc=loc(y)[i],param=varid(y)[i],
                       unit=unit(y)[i],lon=lon(y)[i],lat=lat(y)[i],
                       alt=alt(y)[i],cntr=cntr(y)[i],stid=stid(y)[i])
      class(ys) <- class(y)[-1]
      z <- DS(ys,X,biascorrect=biascorrect,
              method=method,swsm=swsm,eofs=eofs,
              rmtrend=rmtrend,verbose=verbose,...)

      attr(z,'mean') <- 0
      zp <- predict(z,newdata=X)
      y.out[,i] <- coredata(zp)
      #plot(ys); lines(zoo(z,order.by=year(z)))
      #lines(zoo(y.out[,i],order.by=year(X)),col='blue',lty=2); browser()
      #print(c(i,dy[2])); print(summary(y.out[,i]))
      # If common EOFs, then also capture the predictions:
      if (!is.null(attr(z,'appendix.1'))) {
        y.app[,i] <- attr(zp,'appendix.1.')
      }
    }
    if (verbose) print('Transform back into a PCA-object')
    ds <- zoo(y.out,order.by=index(X))
    ds <- attrcp(y,ds)
    attr(ds,'eigenvalues') <- attr(Z,'eigenvalues')
    attr(ds,'sum.eigenv') <- attr(Z,'sum.eigenv')
    attr(ds,'pattern') <- attr(Z,'pattern')
 }
  #print(class(model)); str(model)
  attr(ds,'variable') <- varid(Z)
  attr(ds,'mean') <- attr(Z,'mean') # + offset
  attr(ds,'max.autocor') <- attr(Z,'max.autocor')
  attr(ds,'tot.var') <- attr(Z,'tot.var')
   #attr(ds,'dimensions') <- c(du[1],du[2])
  attr(ds,'longitude') <- lon(Z)
  attr(ds,'latitude') <- lat(Z)
#  attr(ds,'source') <- paste(attr(Z,'source'),attr(X,'source'),sep="-")
  attr(ds,'source') <- attr(X,'source')
  attr(ds,'history') <- history.stamp(y)
  attr(ds,'call') <- match.call()
  class(ds) <- c("ds",cls)
  
  #plot(zoo(y[,1],order.by=year(y)),lwd=3)
  #lines(zoo(y.out[,1],order.by=year(X)),col='blue',lwd=2)
  #lines(zoo(ds[,1],order.by=year(ds)),col='red',lty=2)
  
  invisible(ds)
}


biasfix <- function(x) {
  stopifnot(!missing(x), inherits(x,"eof"),inherits(x,"comb"))
  # Check if the results already have been bias-corrected
  if (!is.null(attr(x,'diagnose'))) return(x)
  diag <- diagnose(x)
  n <- attr(x,'n.apps')
  for ( i in 1:n ) {
    eval(parse(text=paste("z <- attr(x,'appendix.",i,"')",sep="")))
    Z <- coredata(z)
    #print(dim(Z)); print(length(diag$sd.ratio)); print(length(diag$mean.diff))
    # diagnose: (1 + sd(z))/(1 + sd(x))
    # x is reanalysis; z is gcm:
    for (j in 1:length(Z[1,]))
      Z[,j] <- Z[,j]/diag$sd.ratio[i,j] + diag$mean.diff[i,j]
    y <- zoo(Z,order.by=index(z))
    y <- attrcp(z,y)
    eval(parse(text=paste("y -> attr(x,'appendix.",i,"')",sep="")))
  }
  attr(x,'history') <- history.stamp(x)
  attr(x,'quality') <- "'bias' corrected -  ref (Imbert & Benestad (2005); Theor. Appl. Clim.; DOI: 10.1007/s00704-005-0133-4"
  attr(x,'diagnose') <- diag
  return(x)
}
