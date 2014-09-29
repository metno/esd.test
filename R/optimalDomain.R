## Author         A. Mezghani
## Create         11-12-2013
## Description    Performs a quick search of the optimal spatial domain that has to be used in the downscaling 

## y : station object as predictand ; X : field object as predictor

longitude <- function(x) return(attr(x,"longitude"))
latitude <- function(x) return(attr(x,"latitude"))
station.id <- function(x) return(attr(x,"station_id")) 
wmo.id <- function(x) return(attr(x,"wmo"))
quality <-  function(x) return(attr(x,"quality"))
altitude <- function(x) return(attr(x,"altitude"))
calendar <- function(x) return(attr(x,"calendar"))
country <- function(x) return(attr(x,"country"))
location <- function(x) return(attr(x,"location"))
variable <- function(x) return(attr(x,"variable"))
unit <- function(x) return(attr(x, "unit"))
src <-  function(x) return(attr(x,"source"))
aspect <- function(x) return(attr(x, "aspect"))
reference <-  function(x) (attr(x, "reference"))
info <- function(x) return(attr(x,"info"))
history.esd <- function(x) return(attr(x, "history"))
pattern <- function(x) return(attr(x,"pattern"))
element <- function(x) return(attr(x,"element"))

var2param <- function(x) {
    variable <-as.matrix(as.character(variable(x)))
    var2prm <- function(x) switch(x,"T[2 * m]"="t2m")
    apply(variable,1,var2prm)
    return(apply(variable,1,var2prm))
}
##finished <- FALSE
##if (finished) {
##    subset.station <- 
##        loc <- location(x)
##    cntr <- country(x)
##    lon <- longitude(x)
##    lat <- latitude(x)
##    alt <- altitude(x)
##    src <- srcesd(x)
##
##    if (inherits(x,"station")) 
##        ele <- as.character(variable((x)))
##}

var2param <- function(x) return(switch(x,expression("T[2 * m]"="t2m")))
param <- apply(ele, 2, FUN= )

meta <- data.frame(station_id = station.id(x),
                   location = location(x),
                   country = country(x),
                   longitude = longitude(x),
                   latitude = latitude(x),
                   altitude = altitude(x),
                   element = apply(as.matrix(var2param(x)),1,esd2ele),
                   start = rep(year(x)[1],length(station.id(x))),
                   end = rep(end(x)[1],length(station.id(x))),
                   source = src(x),
                   quality= quality(x))
class(meta) <- "stationmeta"

optimalDomain <- function(y=NULL,X=NULL,...) UseMethod("optimalDomain")


optimalDomain.default <- function(y,X,...) {

    lon <- attr(y,"longitude")
    lat <-  attr(y,"latitude")

    
    for (idx in 1:ndx)
        for (idy in 1:ndy) {
            x <- subset(X,lon=c(lon-dx,lon+dx), lat=c(lat-dy,lat+dy) , plot=FALSE)
            cyx[idx,idy] <- corfield(y,x)
            
        }
    

    if (inherits(y,"station") & inherits(X,"eof")) {
        X <- eof2field(X,...)
    }

    if (inherits(y,"station") & inherits(X,"field")) {
        cfs <- corfield.station(y,X)
    }

}


## e.g.
## X <- t2m.ERAINT(lon=c(-10,30),lat=c(40,80))
## y <- station.metno("oslo - blindern","t2m")
## y <- as.monthly(y,FUN="mean")
## cfs <- corfield(x=y,y=X)

as.monthly <- function (x, FUN = "mean", ...) 
{
    y <- aggregate(as.zoo(x), function(tt) as.Date(as.yearmon(tt)), 
                   FUN, ...)
    y <- attrcp(x, y)
    attr(y, "history") <- history.stamp(x)
    class(y)[2] <- "month"
    return(y)
}

corfield.station <- function (x, y, plot = TRUE) 
{
    print("corfield.station:")
    stopifnot(inherits(x, "station"), inherits(y, "field"))
    if ((inherits(x, "day")) & (inherits(y, "month"))) {
        x <- aggregate(x, as.yearmon, mean)
        print("Warning in corfield.station: aggregated y to monthly values.")
    }
    xy <- combine.station.field(x, y, all = FALSE)
    x <- xy$y
    y <- xy$X
    r <- apply(t(coredata(y)), 1, cor, coredata(x))
    print(length(r))
    r <- attrcp(y,r,ignore=c("longitude","latitude"))
    ## nattr <- softattr(y)
    ## for (i in 1:length(nattr)) attr(r, nattr[i]) <- attr(y, nattr[i])
    browser()
    attr(r, "dimensions") <- attr(x, "dimensions")[1:2]
    attr(r, "longitude") <- attr(y, "longitude")
    attr(r, "latitude") <- attr(y, "latitude")
    attr(r, "variable") <- paste(attr(x, "variable"), attr(y, 
                                                           "variable"), sep = "+")
    attr(r, "source") <- paste(attr(x, "source"), attr(y, "source"), 
                               sep = "+")
    attr(r, "location") <- attr(x, "location")
    class(r) <- "corfield"
    if (plot) 
        map(r)
    return(r)
}

combine.default <- function(x,y,all=FALSE,orig.format=TRUE) {
                                        # If either of the arguments is NULL, then return the x - useful for looping
    stopifnot(!missing(x))
    if (is.null(x)) return(y)
    if (is.null(y)) return(x)
    
                                        #print(class(x)); print(class(y))
                                        #print("dim(y)="); print(dim(y))
    ##browser()
    if ( !zeros(c(inherits(x,c("station","zoo"),which=TRUE),
                  inherits(y,c("station","zoo"),which=TRUE)) ) )
        X <- combine.station(x,y) else
    if ( !zeros( c(inherits(x,c("station","zoo"),which=TRUE)
                   ,inherits(y,c("eof","zoo"),which=TRUE)) ) |
        !zeros( c(inherits(y,c("station","zoo"),which=TRUE),
                  inherits(x,c("eof","zoo"),which=TRUE)) ) )
        X <- combine.station.eof(x,y,all=all,
                                 orig.format=orig.format) else
    if ( !zeros( c(inherits(x,c("station","zoo"),which=TRUE)
                   ,inherits(y,c("field","zoo"),which=TRUE)) ) |
        !zeros( c(inherits(y,c("station","zoo"),which=TRUE),
                  inherits(x,c("field","zoo"),which=TRUE)) ) )
        X <- combine.station.field(x,y,all=all,
                                   orig.format=orig.format) else { 
                                       print("combine.default - don't know what to do :-(")
                                       Z <- NULL
                                   }
    attr(X,'history') <- c('combine.station',attr(X,'history'))
    attr(X,'call') <- match.call()
    attr(X,'date') <- date()
    invisible(X)
}
combine.field.station <- function(x,y,all=FALSE,orig.format=TRUE) {
                                        #print("combine.field.station")
    ## browser()
                                        # If either of the arguments is NULL, then return the x -
                                        # useful for looping
    
    if (is.null(x)) return(y)
    if (is.null(y)) return(x)

                                        # Keep track of which is an eof object and which is a station record:
    swapped <- FALSE
    if ( inherits(x,c("station")) & inherits(y,c("field"))) {
        yy <- x
        x <- y
        y <- yy
        swapped <- TRUE
    } 
    clsx <- class(x)
    clsy <- class(y)
    index(y) <- as.Date(index(y))
                                        #print(class(X))
    index(x) <- as.Date(index(x))
                                        #print("HERE...")
                                        # REB 20.08.2013: added colnames to x & y before merge to keep track of
                                        # y & x.
    if (length(dim(y))==2) 
        colnames(y) <- paste("y",1:dim(y)[2],sep=".") 
    if (length(dim(x))==2)
        colnames(x) <- paste("x",1:dim(x)[2],sep=".")

    comb <- merge(x,y,all=all)
                                        #print(summary(comb))

    if (orig.format) {
        vars <- names(comb)
        ys <- vars[grep('y',vars)]
        Xs <- vars[grep('x',vars)]
        ix <- is.element(vars,Xs)
        iy <- is.element(vars,ys)
                                        #print(c(sum(ix),sum(iy)))
        browser()      
        XX <- zoo(coredata(comb[,ix]),order.by=index(comb))
        yy <- zoo(coredata(comb[,iy]),order.by=index(comb))
        names(XX) <- Xs
        names(yy) <- ys
        XX <- attrcp(x,XX,ignore="dimensions")          
        ##nattr1 <- softattr(x)
        ##for (i in 1:length(nattr1))
        ##  attr(XX,nattr1[i]) <- attr(x,nattr1[i])
        attr(XX,'dimensions') <- attr(x,'dimensions')
        yy <- attrcp(y,yy,ignore="dimensions")
                                        #nattr2 <- softattr(y)
                                        #for (i in 1:length(nattr2))
                                        #  attr(yy,nattr2[i]) <- attr(y,nattr2[i])
        ## attr(yy,'dimensions') <- attr(y,'dimensions')
        
                                        #    mostattributes(yy) <- attributes(x)
                                        #    mostattributes(XX) <- attributes(y)
        clsx -> class(XX)
        clsy -> class(yy)
        browser()
                                        #print(clsx); print(clsy); print(dim(XX)); print(length(yy))
        if (swapped) X <- list(y=XX,X=yy) else
        X <- list(y=yy,X=XX)
    } else {
        X <- comb
        nattr2 <- softattr(x)
        for (i in 1:length(nattr1))
            attr(X,nattr1[i]) <- attr(x,nattr1[i])
                                        #mostattributes(comb) <- attributes(x)
        attr(X,'X.attributes') <- attributes(y)
    }
                                        #print(summary(combined))
    attr(X,'history') <- c('combine.field.station',attr(X,'history'))
    attr(X,'call') <- match.call()
    attr(X,'date') <- date()
    invisible(X)
}
subset.eof <- function(x,it=NULL,is=NULL) {
    d <- dim(x)
    ## browser()
    if (is.null(is)) is <- 1:d[2] else
    if (is.list(is)) {
                                        # Select a subregion from the EOFs:
        lons <- attr(x,'longitude')
        lats <- attr(x,'latitude')
        X <- attr(x, "pattern")
        if ( (length(is[[1]])==2) & (length(is[[2]])==2) ) {
            lon.rng <- range(is[[1]]); lat.rng <- range(is[[2]])
            if ( (min(lon.rng) < 0) & (attr(x,'greenwich')) ) {
                lons[lons > 180] <- lons[lons > 180] - 360
                srt <- order(lons)
                X <- X[srt,,]
                lons <- lons[srt]
            }
            keepx <- (lons >= lon.rng[1]) & (lons <= lon.rng[2])
            keepy <- (lats >= lat.rng[1]) & (lats <= lat.rng[2])
            if ( (sum(keepx)==0) | (sum(keepy)==0) ) {
                print(is); print(lons); print(lats)
                stop('Check the coordinates')
            }
            X <- X[keepx,keepy,]
            attr(x, "pattern") <- X
            lons[keepx] -> attr(x,'longitude')
            lats[keepy] -> attr(x,'latitude')
            attr(x,'mean') <- attr(x,'mean')[keepx,keepy]
            attr(x,'dimensions') <- c(sum(keepx),sum(keepy),d[2])
            is <- 1:d[2]
        }
    } 
                                        #print(it)
    if (is.null(it)) {
        it <- 1:d[1] 
        dates <- index(x)
        keep <- 1 : d[1]
    } else {
        if (is.numeric(it)) {
            print("numeric it")
            if (max(nchar(as.character(it)))<=2)
                keep <- is.element(as.numeric(format(index(x),"%m")),it)
            else {
                it <- seq(it[1],it[2],1)
                keep <- is.element(as.numeric(format(index(x),"%Y")),it)      
            }
        }
        else if (is.character(it))
            keep <- is.element(index(x),as.Date(it))
                                        #print(c(sum(keep),length(keep)))
        dates <- index(x)[keep]
    }

    ## grep for appendices
    nm <- names(attributes(x))
    id <- grep("appendix",nm)
    if (length(id)>0) {
        nm <- nm[id]
        for (i in 1:length(nm)) {
            eval(parse(text=paste("a <- attr(x,'",nm,"')",sep="")))
            cls <- class(a)
            ais <- zoo(coredata(a)[keep,is],order.by=dates)
            ais <- attrcp(a,ais)
            eval(parse(text=paste("attr(x,'",nm,"') <- ais",sep="")))
            rm(a,ais,cls)
        }
    }
    
    class(x) -> cls
    ##keep <- is.element(index(x),it)
    y <- x[keep,is]
    
    class(x) <- cls; class(y) <- cls
    y <- attrcp(x,y)
    ## browser()
    attr(y,"mean") <- attr(x,"mean")
    attr(y,'pattern') <- attr(x,"pattern")[,,is]
    attr(y,'eigenvalues') <-attr(x,"eigenvalues")[is]
    attr(y,'date-stamp') <- date()
    attr(y,'call') <- match.call()
    attr(y, "dimensions") <- dim(attr(x,"pattern")[,,is])
    return(y)
}

t2m.ERAINT <- function (lon = NULL, lat = NULL, anomaly = FALSE) 
{
    data("eof.t2m.ERAINT", envir = environment())
    ## browser()
    t2m.ERAINT <- eof2field(eof.t2m.ERAINT, lon = lon, lat = lat, 
                            anomaly = anomaly)
    t2m.ERAINT
}

eof2field <- function (x, lon = NULL, lat = NULL, anomaly = FALSE) 
{
    x <- subset(x, is = list(lon = range(lon), lat = range(lat)))
    eof <- x
    U <- attr(eof, "pattern")
    d <- dim(U)
    dim(U) <- c(d[1] * d[2], d[3])
    W <- attr(eof, "eigenvalues")
    V <- coredata(eof)
    x <- U %*% diag(W) %*% t(V)
    if (!anomaly) 
        x <- x + c(attr(eof, "mean"))
    x <- t(x)
    x <- as.field(x, index(eof), lon = attr(eof, "longitude"), 
                  lat = attr(eof, "latitude"), param = attr(eof, "variable"), 
                  unit = attr(eof, "unit"), longname = attr(eof, "longname"), 
                  src = attr(eof, "source"), url = attr(eof, "url"), reference = attr(eof, 
                                                                         "reference"), info = attr(eof, "info"), calendar = attr(eof, 
                                                                                                                     "calendar"), greenwich = attr(eof, "greenwich"))
    if (!is.null(lon) | !is.null(lat)) {
        if (is.null(lon)) 
            lon <- c(-180, 180)
        if (is.null(lat)) 
            lat <- c(-90, 90)
    }
    attr(x, "history") <- c("eof2field", attr(eof, "history"))
    attr(x, "date-stamp") <- date()
    if (anomaly) 
        attr(x, "aspect") <- "anomaly"
    else attr(x, "aspect") <- "original"
    attr(x, "call") <- match.call()
    class(x) <- class(eof)[-1]
    invisible(x)
}
