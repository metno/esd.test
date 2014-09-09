as.4seasons.day <- function(x,FUN='mean',na.rm=TRUE,...) {
  t <- index(x)
  year <- as.numeric(format(t,'%Y'))
  month <- as.numeric(format(t,'%m'))
  day <- as.numeric(format(t,'%d'))
  #shift the time stamps by one month, sneaking December into the subsequent year
browser()
  month <- month + 1
  dec <- is.element(month,13)
  year[dec] <- year[dec] + 1
  month[dec] <- 1
  tshifted <- as.Date(paste(year,month,day,sep="-"))
  X <- zoo(coredata(x),order.by=tshifted)
  y <- aggregate(X,as.yearqtr,FUN=FUN,...,na.rm=na.rm)
  unit <- attr(y,'unit')
  y <- attrcp(x,y,ignore="unit")
  unit -> attr(y,'unit')
  #str(y); print(unit)
  attr(y,'history') <- history.stamp(x)
  class(y) <- class(x)
  class(y)[2] <- "season"
  invisible(y)
}
