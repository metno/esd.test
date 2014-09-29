
## author Rasmus E. Benestad
## Last-Update 2013-07-23 by Abdelkader Mezghani

## Formatted meta data for the esd R package 
## Abdelkader Mezghani
## Last update : 24-02-2014

## SCRIPTS TO CONVERT META DATA FROM ITS ORIGINAL SOURCE TO THE ESD FORMAT

## PREPARING THE STATION META DATA FILE - "station.meta.rda"
## and different meta data sources

## depends on 

## scripts needed to run meta2esd
source("esd/R/ghcnd.R") # already included in the esd package
source("esd/R/ghcnm.R") # already included in the esd package
source("R_scripts/nordklim.R") # not yet included in the clim.pact and esd package
source("R_scripts/metno.R") # not yet included in the esd package 
source("R_scripts/ecad.R") # not yet included in the esd package
##source("R_scripts/narp.R") # already included in the esd packag
##source("R_scripts/nacd.R") # already included in the clim.pact and moved to the esd package

meta2esd <- function(path="~",force=FALSE,save=FALSE,backup=FALSE,verbose=FALSE) {
  ## require(clim.pact)
  
  ## These data sets will be re-saved in esd and consolidated in terms of variable names
  if (file.exists(file.path(path,"station.meta.rda")) & backup)
    file.backup(file.path(path,"station.meta.rda"))
  
  ## browser()
  ## GHCND Rda META FILE
  if (!file.exists(file.path(path,"ghcnd.meta.rda")) | force) {
    if (backup) backup.file(file.path(path,"ghcnd.meta.rda"))
    ghcnd.meta <- ghcnd.meta(force=TRUE,save=TRUE)
  }
  else
    load(file.path(path,"ghcnd.meta.rda"),envir=environment())

  ## GHCNM Rda META FILE 
  if (!file.exists(file.path(path,"ghcnm.meta.rda")) | force) {
    if (backup) file.backup(file.path(path,"ghcnm.meta.rda"))
    ghcnm.meta <- ghcnm.meta(save=TRUE)
  }
  else
    load(file.path(path,"ghcnm.meta.rda"),envir=environment())

  ## METNO-Daily Rda META FILE 
  if (!file.exists(file.path(path,"metnod.meta.rda")) | force){
    if (backup) file.backup(file.path(path,"metnod.meta.rda"))
     metnod.meta <- metnod.meta(save=TRUE)
   }
  else
    load(file.path(path,"metnod.meta.rda"),envir=environment())

  ## METNO-Monthly Rda META FILE 
  if (!file.exists(file.path(path,"metnom.meta.rda")) | force){
    if (backup) file.backup(file.path(path,"metnom.meta.rda"))
     metnom.meta <- metnom.meta(save=TRUE)
   }
  else
    load("metnom.meta.rda")  

  ## NACD Rda META FILE 
  if (!file.exists(file.path(path,"nacd.meta.rda")) | force)    ## NACD Rda META FILE from clim.pact
    {data("NACD",envir=environment()) ; nacd.meta <- NACD} ## nacd.meta ! not yet created 
  else
    load(file.path(path,"nacd.meta.rda"),envir=environment())

  ## NARP Rda META FILE 
  if (!file.exists(file.path(path,"narp.meta.rda")) | force)    ## NARP rda META FILE from clim.pact
    data("narp.meta",envir=environment())
  else
    load(file.path(path,"narp.meta.rda"),envir=environment())
  
  ## NORDKLIM Rda META FILE 
  if (!file.exists(file.path(path,"nordklim.meta.rda")) | force) {    ## NARP Rda META FILE
    if (backup) file.backup(file.path(path,"nordklim.meta.rda"))
    nordklim.meta <- nordklim.meta(save=TRUE)
  }
  else
    load(file.path(path,"nordklim.meta.rda"),envir=environment())

  ## ECAD Or ECSN Rda META FILE 
  if (!file.exists(file.path(path,"ecad.meta.rda")) | force) {   ## ECAD Rda META FILE
      if (backup) file.backup(file.path(path,"ecad.meta.rda"))
      ecad.meta <- ecad.meta(save=TRUE)
  }
  else
      load(file.path(path,"ecad.meta.rda"))
  ecsn.meta <- ecad.meta
  
  ## Get meta data lengths
  n.narp <- length(narp.meta$stnr)
  n.nacd <- length(nacd.meta$station.number)
  n.nork <- length(nordklim.meta$station_id)
  
  n.nacd <- length(nacd.meta$station.number)
  n.ecad <- length(ecad.meta$station_id)
  n.ghcnm <- length(ghcnm.meta$station_id)
  n.metnom <- length(metnom.meta$station_id)
  n.metnod <- length(metnod.meta$station_id)
  
  n.ghcnd <- length(ghcnd.meta$station_id)
  nacd.lon <- (nacd.meta$degE + nacd.meta$minE/60)*ifelse(as.character(nacd.meta$E.W)==" E",1,-1)
  nacd.lat <- nacd.meta$degN + nacd.meta$minN/60 
  
  nacd.cn <- gsub(" ","",as.character(nacd.meta$country)) ## AM 17-09-2014 bug fixed - remove extra spaces ...
  nacd.cn[is.element(nacd.cn,'FR')] <- 'FRI'
  ##r.script <- readLines("meta2esd.R")
  ##  data("observation.meta",envir=environment()
  station.meta <- list(
                       station_id=as.character(c(nacd.meta$station.number,narp.meta$stnr,nordklim.meta$station_id,ecad.meta$station_id,ghcnm.meta$station_id,ghcnd.meta$station_id,metnom.meta$station_id,metnod.meta$station_id)),
                       location=c(nacd.meta$location,narp.meta$names,nordklim.meta$location,ecad.meta$location,ghcnm.meta$location,ghcnd.meta$location,metnom.meta$location,metnod.meta$location),
                       country=c(toupper(decryptcn(nacd.cn)),toupper(narp.meta$countries),nordklim.meta$country,ecad.meta$country,ghcnm.meta$country,ghcnd.meta$country,metnom.meta$country,metnod.meta$country),
                       longitude=c(nacd.lon,narp.meta$lons,nordklim.meta$longitude,ecad.meta$lon,ghcnm.meta$longitude,ghcnd.meta$longitude,metnom.meta$longitude,metnod.meta$longitude),
                       latitude=c(nacd.lat,narp.meta$lats,nordklim.meta$latitude,ecad.meta$lat,ghcnm.meta$latitude,ghcnd.meta$latitude,metnom.meta$latitude,metnod.meta$latitude),
                       altitude=c(nacd.meta$alt,rep(NA,n.narp),nordklim.meta$altitude,ecad.meta$alt,ghcnm.meta$altitude,ghcnd.meta$altitude,metnom.meta$altitude,metnod.meta$altitude),
                       element=c(nacd.meta$element,rep(NA,n.narp),nordklim.meta$element,ecad.meta$element,ghcnm.meta$element,ghcnd.meta$element,metnom.meta$element,metnod.meta$element),
                       start=c(nacd.meta$start,rep(NA,n.narp),nordklim.meta$start,ecad.meta$start,ghcnm.meta$start,ghcnd.meta$start,metnom.meta$start,metnod.meta$start),
                       end=c(rep(NA,n.nacd),rep(NA,n.narp),nordklim.meta$end,ecad.meta$end,ghcnm.meta$end,ghcnd.meta$end,metnom.meta$end,metnod.meta$end),    	
                       source=c(rep("NACD",n.nacd),rep("NARP",n.narp),rep("NORDKLIM",n.nork),rep("ECAD",n.ecad),rep("GHCNM",n.ghcnm),rep("GHCND",n.ghcnd),rep("METNOM",n.metnom),rep("METNOD",n.metnod)),
                       wmo=c(nacd.meta$wmo.number,narp.meta$WMO.number,rep(NA,n.nork),rep(NA,n.ecad),rep(NA,n.ghcnm),rep(NA,n.ghcnd),rep(NA,n.metnom),rep(NA,n.metnod)), 
                       quality=c(nacd.meta$quality,rep(NA,n.narp),rep(NA,n.nork),rep(NA,n.ecad),rep(NA,n.ghcnm),rep(NA,n.ghcnd),rep(NA,n.metnom),rep(NA,n.metnod))
                       )
  
  if (verbose) print(str(station.meta))
  if (verbose) print(table(station.meta$source))
  if (save) save(file=file.path(path,"station.meta.rda"),station.meta)
  attr(station.meta,'history') <- c('meta2esd.R - data taken from the clim.pact package and consolidated for NACD and NARP',"nordklim.meta.rda","ecad.meta.rda","ghcnd.meta.rda","ghcnm.meta.rda","metnom.meta.rda","metnod.meta.rda")
  ##attr(station.meta,'R-script') <- r.script
  attr(station.meta,'date') <- date
  attr(station.meta,'call') <- match.call()
  attr(station.meta,'author') <- 'R.E. Benestad & A. Mezghani'
  attr(station.meta,'metnoURLs') = c(
        "www.dmi.dk/dmi/sr96-1.pdf",
        "http://www.norden.org/en/publications/publikationer/2005-450",
        "http://www.smhi.se/hfa_coord/nordklim/",
        "http://eca.knmi.nl/",
        "https://dokit.met.no/klima/userservices/urlinterface/brukerdok",
        "https://dokit.met.no/klima/userservices/urlinterface/brukerdok")
  ## station.meta$call <- match.call()
  invisible(station.meta)
  save(station.meta,file=file.path("station.meta.rda"))
}

decryptcn <- function(codes,src="ECAD") {
  n <- length(codes)
  country <- rep("",n)
  for (i in 1:n) {
    country[i]= switch(codes[i],'SE'='Sweden','AT'=,'Austria','BE'='Belgium',
            'HR'='Croatia','CY'='Cypros','CZ'='Chzeck Republic',
            'FI'='Finland','FR'='France','DE'='Germany',
            'IS'='Iceland','RU'='Russia','DK'='Denmark',
            'IE'='Ireland','NL'='the Netherlands','IT'='Italy',
            'NO'='Norway','LV'='Latvia','LT'='Lituania',
            'PT'='Portugal','RO'='Romania','SK'='Slovakia',
            'SI'='Slovenia','ES'='Spain','CH'='Switzerland',
            'RS'='Serbia','EE'='Estonia','MK'='Makedonia',
            'GB'='Great Britain','BA'='Bosnia-Hertsogovina',
            'AL'='Albania','DZ'='Algeria','LU'='Luxemburg',
            'AM'='Armenia','GL'='Greenland','AZ'='Azerbaijan',
            'EG'='Egypt','GR'='Greece','PL'='Poland','IL'='Israel',
            'BY'='Belarus','GE'='Georgia','HU'='Hungary',
            'IQ'='Iraq','KZ'='Khazakstan','LY'='Libya',
            'MD'='Moldova','MO'='Morocco','MT'='Malta',
            'SA'='Saudi Arabia','SY'='Syria','TJ'='Tajikistan',
            'TR'="Turkey",'UA'='Ukraina','UZ'='Uzbekistan',
            'B'='Belgia','FIN'='Finland','FRI'='Faroe Islands',
            'G'='Greenland','IRL'='Ireland','IS'='Iceland',
            'N'='Norway','S'='Sweden')
  }
  invisible(country)
}

file.backup <- function(file) {
    stamp <- file.info(file)$mtime
    name <- substr(file,1,nchar(file)-4)
    file.rename(file,paste(name,"_",format(stamp,"%Y%m%d"),".rda",sep=""))
}
