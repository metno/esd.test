## Author Rasmus E. Benestad - was initially part of subset.R file
## Modified by A. Mezghani
## Last update 06.01.2014

subset.station <- function(x,loc=NULL , param = NULL,  stid = NULL ,lon = NULL, lat = NULL, 
                            alt = NULL, cntr = NULL, src = NULL , it = NULL , n.year = NULL,
                           is = NULL) {

  #print("subset.station")
  # REB: added a few lines so that station.subset will be used if the arguments 'is' or
  # 'it' are active...
  if ( !is.null(is) | !is.null(it) ) {
    #print("here")
    x2 <- station.subset(x,it=it,is=is)
    return(x2)
  }
  ## browser()
  ## select stations based on meta data
  ss <- select.station(x,loc = loc , param = param,  stid = stid ,lon = lon, lat = lat, alt = alt, cntr = cntr, src = src , n.year = n.year)
  if (!is.null(ss))
    id <- is.element(attr(x,'station_id'),ss$station_id)
  ## Keep selected stations only
  if (!is.null(ss)) x2 <- station.subset(x,it=it,is=which(id))
  if (!is.null(is)) x2 <- station.subset(x2,it=it,is=is)
  return(x2)
}

station.subset <- function(x,it=NULL,is=NULL) {
  # REB: Use select.station to condition the selection index is...
  # loc - selection by names
  # lon/lat selection be geography or closest if one coordinate lon/lat
  #         if two-element vectors, define a region
  # alt - positive values: any above; negative any below height
  # cntr - selection by country
  
  x0 <- x
  if (is.null(it) & is.null(is)) return(x)
  d <- dim(x)
  if (is.null(d)) d <- c(length(x),1)
  if (is.null(it)) it <- 1:d[1]
  if (is.null(is)) is <- 1:d[2]

  #print("HERE")
  ## browser()
  t <- index(x)
  datetype <- class(t)
  if (datetype=="Date") {
    year <- as.numeric( format(t, '%Y') ) 
    month <- as.numeric( format(t, '%m') )
  }
  else if (datetype=="numeric") year <- t
  class(x) -> cls
  ##print(cls)
  class(x) <- "zoo"
  if ( (class(it)=="logical") & (length(it)==length(x)) )
      y <- x[it,is] else
    if ( (min(it) > 0) & (max(it) < 13) & (inherits(x0,c("month"))) ) {
      ##keepm <- as.numeric(format(index(X),"%m"))==it
      ##print("Monthly aggregated field")
      ##keepm <- is.element(as.POSIXlt(dates)$mon+1,it)
      ii <- is.element(month,it)
      y <- x[ii,is]
    } else 
    if ( (min(it) > 0) & (max(it) < 5) & (inherits(x0,c("season"))) ) {
      print("Seasonally aggregated field")
      ##print(table(as.POSIXlt(dates)$mon+1))
      ##keepm <- is.element(as.POSIXlt(dates)$mon+1,c(1,4,7,10)[it])
      ##print(c(it,sum(keepm)))
      ii <- is.element(month,c(1,4,7,10)[it])
      y <- x[ii,is]
    } else
    if ( (min(it) > 0) & (max(it) < 13) & (sum(is.element(it,1:12)) > 0) ) {
      
    } else
    if (sum(is.element(it,1600:2200)) > 0) {
      #print(it)
      if (length(it)==2) it <- it[1]:it[2]
      ii <- is.element(year,it)
      y <- x[ii,is]
    } else
    if (is.character(it)) {
      dates <- as.Date(it)
      ii <- is.element(index(x),dates)
      y <- x[ii,is]
    } else y <- x[it,is]
   
  class(x) <- cls; class(y) <- cls
  y <- attrcp(x,y)
  #nattr <- softattr(x)
  #for (i in 1:length(nattr))
  #  attr(y,nattr[i]) <- attr(x,nattr[i])
#  mostattributes(y) <- attributes(x)
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
  attr(y,'date-stamp') <- date()
  attr(y,'call') <- match.call()
  attr(y,'history') <- history.stamp(y)
  return(y)
}
