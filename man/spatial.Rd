\name{spatial.avg.field}
\alias{spatial}
\title{Spatial Average of a Field Object.}
\description{Computes the spatial average of a field object and return a
zoo time series object.}
\usage{
spatial.avg.field(x,...)
}
\arguments{
 \item{x}{A zoo field object with two (longitude, latitude) or three dimensions 
   (longitude, latitude, time)}
}
\value{A "zoo" "time series" object}

\seealso{
  \code{\link{retrieve.ncdf4}}
}
\author{A. Mezghani, MET Norway}
\examples{
\dontrun{
# Consider the "gcm" object from the e.g. in \link{retrieve.ncdf4}
# Compute the spatial average along lon and lat in gcm2
gcm2 <- spatial.avg.field(gcm)
# keep all attributes in gcm2
gcm2 <- attrcp(gcm,gcm2)
# Compute the annual mean 
gcm.am <- as.annual(gcm2,FUN=mean,na.rm=TRUE)
# keep all attributes in gcm.am
gcm.am <- attrcp(gcm2,gcm.am)
year <- index(gcm.am)
# Compute the anomalies relative to the period 1986-2005
agcm.ave <- gcm.am-mean(gcm.am[is.element(as.numeric(year),c(1986:2005))])
agcm.ave <- attrcp(gcm.am,agcm.ave)
# plot anomaly time series of the global mean temperature 
frame.metno(agcm.ave,col="black",cex.lab=0.75,cex.axis=0.75) ! Should be
  updated !
# Add vertical margin text with y-label
mtext("Anomaly values relative to 1986-2005",side=2,line=2,cex=0.75,las=3)
}
}
\keyword{parameter , element}