# Workaround from having R-scripts in the data folder:
# Rasmus Benestad

slp.MERRA <- function(lon=NULL,lat=NULL,anomaly=FALSE) {
  data("eof.slp.MERRA",envir=environment())
  slp.MERRA<- eof2field(eof.slp.MERRA,lon=lon,lat=lat,anomaly=anomaly)
  slp.MERRA
}

t2m.MERRA <- function(lon=NULL,lat=NULL,anomaly=FALSE) {
  data("eof.t2m.MERRA",envir=environment())
  t2m.MERRA<- eof2field(eof.t2m.MERRA,lon=lon,lat=lat,anomaly=anomaly)
  t2m.MERRA
}

t2m.NCEP <- function(lon=NULL,lat=NULL,anomaly=FALSE) {
  data("eof.t2m.NCEP",envir=environment())
  t2m.NCEP<- eof2field(eof.t2m.NCEP,lon=lon,lat=lat,anomaly=anomaly)
  t2m.NCEP
}

sst.NCEP <- function(lon=NULL,lat=NULL,anomaly=FALSE) {
  data("eof.sst.NCEP",envir=environment())
  sst.NCEP<- eof2field(eof.sst.NCEP,lon=lon,lat=lat,anomaly=anomaly)
  sst.NCEP
}

slp.NCEP <- function(lon=NULL,lat=NULL,anomaly=FALSE) {
  data("eof.slp.NCEP",envir=environment())
  slp.NCEP<- eof2field(eof.slp.NCEP,lon=lon,lat=lat,anomaly=anomaly)
  slp.NCEP
}

t2m.ERAINT <- function(lon=NULL,lat=NULL,anomaly=FALSE) {
  data("eof.t2m.ERAINT",envir=environment())
  t2m.ERAINT<- eof2field(eof.t2m.ERAINT,lon=lon,lat=lat,anomaly=anomaly)
  t2m.ERAINT
}

precip.ERAINT <- function(lon=NULL,lat=NULL,anomaly=FALSE) {
  data("eof.precip.ERAINT",envir=environment())
  precip.ERAINT<- eof2field(eof.precip.ERAINT,lon=lon,lat=lat,anomaly=anomaly)
  precip.ERAINT
}

slp.ERAINT <- function(lon=NULL,lat=NULL,anomaly=FALSE) {
  data("eof.slp.ERAINT",envir=environment())
  slp.ERAINT<- eof2field(eof.slp.ERAINT,lon=lon,lat=lat,anomaly=anomaly)
  slp.ERAINT
}

t2m.ERA40 <- function(lon=NULL,lat=NULL,anomaly=FALSE) {
  data("eof.t2m.ERA40",envir=environment())
  t2m.ERA40<- eof2field(eof.t2m.ERA40,lon=lon,lat=lat,anomaly=anomaly)
  t2m.ERA40
}

t2m.DNMI <- function(lon=NULL,lat=NULL,anomaly=FALSE) {
  data("eof.t2m.DNMI",envir=environment())
  t2m.DNMI<- eof2field(eof.t2m.DNMI,lon=lon,lat=lat,anomaly=anomaly)
  t2m.DNMI
}

slp.DNMI <- function(lon=NULL,lat=NULL,anomaly=FALSE) {
  data("eof.slp.DNMI",envir=environment())
  slp.DNMI<- eof2field(eof.slp.DNMI,lon=lon,lat=lat,anomaly=anomaly)
  slp.DNMI
}

sst.DNMI <- function(lon=NULL,lat=NULL,anomaly=FALSE) {
  data("eof.sst.DNMI",envir=environment())
  sst.DNMI<- eof2field(eof.sst.DNMI,lon=lon,lat=lat,anomaly=anomaly)
  sst.DNMI
}

t2m.NorESM.M <- function(lon=NULL,lat=NULL,anomaly=FALSE) {
  data("eof.t2m.NorESM.M",envir=environment())
  t2m.NorESM.M<- eof2field(eof.t2m.NorESM.M,lon=lon,lat=lat,anomaly=anomaly)
  t2m.NorESM.M
}




