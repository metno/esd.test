\name{spatial.avg.field}
\alias{spatial.avg.field}
\title{spatial.avg.field}
\description{
Perform spatial average of a zoo field (object) using different methods such as mean, sd, ... 
}
\usage{
spatial(x,method="mean",...)
spatial.avg.field(x,...) # name need to be updated such as spatial(x,method="average")
}
\arguments{
  \item{x}{A zoo field object with longitude and latitude dimensions}
 }
\value{A "zoo" "time series" object}

\seealso{
\code{\link{test.retrieve.ncdf4}}
}

\author{A. Mezghani, MET Norway}
\examples{
\dontrun{
# Example 1: use spatial.avg.field to compute annual mean temperature
over Scandinavia from Access-1-0 model run for CMIP5 RCP45 experiment
and show a plot:
gcm <- retrieve.ncdf4(ncfile="CMIP5.monthly/rcp45/tas_Amon_ens_rcp45_00.nc",param="Auto",lon=c(-20,20),lat=c(40,60), lev = NULL,time = c(1960,2001),ncdf.check=TRUE,miss2na = TRUE,verbose=TRUE,plot=TRUE) # to retrieve 2m-temperature data in gcm
gcm2 <- spatial(gcm,method="mean") # Computes the global mean temperature from gcm data into gcm2
# Example 2: use spatial.avg.field to compute the global mean anomaly temperature for all CMIP5 RCP45 scenarios located in "path" folder and save the results into "file".
test.retrieve.ncdf4(path="CMIP5.monthly/rcp45/",file="Norway_mean_t2m_anomaly_cmip3_1986-2005.rda") 
}
}
\keyword{spatial,average,field}