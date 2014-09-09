
## Author Rasmus E. Benestad - was initially part of subset.R file
## Modified by A. Mezghani
## Last update 06.01.2014 ; 24-02-2014
 
subset.station <- function(x,it = NULL,is=NULL,loc=NULL , param = NULL,  stid = NULL ,lon = NULL, lat = NULL, 
                           alt = NULL, cntr = NULL, src = NULL , nmin = NULL,
                           verbose=FALSE) {
  ## browser()
  if (inherits(it,c('field','station'))) {
    ## Match the times of another esd-data object
    print('field/station')
    x2 <- matchdate(x,it)
    return(x2)
  }
  ##print("subset.station")
  if (is.null(dim(x))) {
      x2 <- station.subset(x,it=it,is=1)
  } else {
      ##print("here")
      x2 <- station.subset(x,it=it,is=is)
      ## browser()
      ## extra selection based on meta data
      ## ss <- select.station(x=x2,loc = loc , param = param,  stid = stid ,lon = lon, lat = lat, alt = alt, cntr = cntr, src = src , nmin = nmin)
      ## browser()
      ## if (!is.null(ss)) {
      ##    id <- is.element(attr(x2,'station_id'),ss$station_id)
      ## Keep selected stations only
      ##    x2 <- station.subset(x2,it=it,is=which(id),verbose=verbose)
      ##}
      ##if (!is.null(is)) x2 <- station.subset(x2,it=it,is=is,verbose=verbose)
  }
  return(x2)
}

station.subset <- function(x,it=NULL,is=NULL,verbose=FALSE) {
    ## REB: Use select.station to condition the selection index is...
    ## loc - selection by names
    ## lon/lat selection be geography or closest if one coordinate lon/lat
    ##         if two-element vectors, define a region
    ## alt - positive values: any above; negative any below height
    ## cntr - selection by country
    
    x0 <- x
    if (is.null(it) & is.null(is)) return(x)
    d <- dim(x)
    if (is.null(d)) d <- c(length(x),1)
    if (is.null(it)) it <- index(x)
    if (is.null(is)) is <- 1:d[2]

    ## browser()
    ##print("HERE")
    ## get time in t
    t <- index(x)
    if (class(it)!=class(t)) print("Index and it class do not match !")
    
    ##  if (datetype=="Date") {
    if (inherits(t,c("Date","yearmon"))) {
        ## REB: replaced by lines below:
        ##    year <- as.numeric( format(t, '%Y') ) 
        ##    month <- as.numeric( format(t, '%m') )
        yr <- year(x)
        mo <- month(x)
        dy <- day(x)
    } else print("Index of x should be a Date or yearmon object")
    
    ## Generate sequence of days, months or years if range of it value is given
    if ((length(it)>2) & (is.character(it)))
        it <- as.Date(it)
    else if ( length(it) == 2 ) {
        if (is.character(it)) {
            if (inherits(x,"month")) ## it is a month or season
                it <- seq(as.Date(it[1]),as.Date(it[2]),by='month')
            else if (inherits(x,"day")) ## it is a day
                it <- seq(as.Date(it[1]),as.Date(it[2]),by='day')
            else if (inherits(x,"annual")) ## it is a year
                it <- seq(as.Date(it[1]),as.Date(it[2]),by='year')
            else if ((class(it)=="numeric") | (class(it)=="integer")) {
                if (min(it) > 1500) ## it is a year
                    it <- seq(it[1],it[2],by=1)
            }
        }
    } 
    ## browser()
    ## get the subset indices in ii
    if ((class(it)=="numeric") | (class(it)=="integer")) {    
        if ( ((min(it,na.rm=TRUE) > 0) & (max(it,na.rm=TRUE) < 13)) ) {## it is a month or season
            it.mo <- it
            ii <- is.element(mo,it.mo)
        }
        else if ((min(it,na.rm=TRUE) > 0) & (max(it,na.rm=TRUE) < 31)) {## it is a day
            it.dy <- it
            ii <- is.element(dy,it.dy)
        }
        else if (min(it) > 1500) {## it is a year
            it.yr <- it
            ii <- is.element(yr,it.yr)
        }
    }
    else if (inherits(it,c("Date","yearmon"))) {       
        ii <- is.element(t,it)
    }
    else ## keep all values
        ii <- 1:length(t)
    
    class(x) -> cls
    ##print(cls)
    ## update the class of x
    class(x) <- "zoo" 
    
    
    #else if (inherits(x0,c("month"))) {
    #    ii <- is.element(mo,it.mo)
    #    ##print("here")
    #}
    #else if ((inherits(x0,c("day")))) {
        ##browser()
    #    if (verbose) print("Daily Object")
    #    ii <- is.element(dy,it.dy)
    #}
    #else if ((min(it) > 0) & (max(it) < 5) & (inherits(x0,"season"))) {
    #    if (verbose) print("Seasonal Object")
    #    ii <- is.element(mo,c(1,4,7,10)[it.mo]) 
    #}
    #else if ( (min(it) > 0) & (inherits(x0,"annual")) ) {
    #    if (verbose) print("Annual Object")
    #    ## browser()
    #    ii <- is.element(it,it.yr)
    #    ##if (sum(ii)>0) y <- x[ii,is] else stop(paste("it value(s) must be in the range of",paste(range(yr),collapse="-")))
    # }
    ## else if ((min(it) > 0) & (max(it) < 13) & (sum(is.element(it,1:12)) > 0)) {
        
    ## }
    #else if (sum(is.element(it,1600:2200)) > 0) {
    #    ##print(it)
    #    if (length(it)==2) it <- it[1]:it[2]
    #    ii <- is.element(yr,it.yr)
    #   
    #}
    #else if (is.character(it)) {
    #    dates <- as.Date(it)
    #    ii <- is.element(index(x),dates)
    #   
    #}
    #else
    #    ii <- 1:length(t)
    
    y <- x[ii,is]

    class(x) <- cls; class(y) <- cls
    y <- attrcp(x,y,ignore=c("names"))
    ## nattr <- softattr(x)
    ## for (i in 1:length(nattr))
    ##  attr(y,nattr[i]) <- attr(x,nattr[i])
    ## mostattributes(y) <- attributes(x)
    attr(y,'longitude') <- attr(x,'longitude')[is]
    attr(y,'latitude') <- attr(x,'latitude')[is]
    attr(y,'altitude') <- attr(x,'altitude')[is]
    attr(y,'country') <- attr(x,'country')[is]
    attr(y,'source') <- attr(x,'source')[is]
    attr(y,'station_id') <- attr(x,'station_id')[is]
    attr(y,'location') <- attr(x,'location')[is]
    attr(y,'quality') <- attr(x,'quality')[is]
    attr(y,'URL') <- attr(x,'URL')[is]
    attr(y,'history') <- attr(x,'history')[is]
    attr(y,'variable') <- attr(x,'variable')[is]
    attr(y,'element') <- attr(x,'element')[is]
    attr(y,'aspect') <- attr(x,'aspect')[is]
    attr(y,'unit') <- attr(x,'unit')[is]
    attr(y,'longname') <- attr(x,'longname')[is]
    attr(y,'reference') <- attr(x,'reference')[is]
    attr(y,'info') <- attr(x,'info')[is]
    attr(y,'method') <- attr(x,'method')[is]
    attr(y,'type') <- attr(x,'type')[is]
    ##attr(y,'date-stamp') <- date()
    ##attr(y,'call') <- match.call()
    attr(y,'history') <- history.stamp(y)
    return(y)
}
 
