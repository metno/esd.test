## Name		: Retrieve
## Description	: S3 method used to retrieve data from netcdf file. The default method was set to the old version of retrieve i.e. retrieve.nc in previous clim.pact package 
## Author 	: A. Mezghani, MET NORWAY
## contact 	: abdelkaderm@met.no
## Created      : 21-03-2013
## Last Update	: 25-04-2013
## require	: zoo, summary_ncdf4 , check_ncdf4
## input	: a zoo field object / 3 dimensional field with dimensions (time,lon,lat)

## Check if ncdf4 library exists and install it if not

if (library("ncdf4", logical.return = TRUE)) {
  library(ncdf4) 
} else install.packages("ncdf4")

## Define retrieve as method
retrieve <- function(ncfile=NULL,...) UseMethod("retrieve")
 
## Set retrieve for ncdf4 object
retrieve.ncdf4 <- function (ncfile = ncfile, path = path , param = "auto",
                            lon = NULL, lat = NULL, lev = NULL, time = NULL,
                            miss2na = TRUE, greenwich = FALSE , ncdf.check = TRUE ,
                            plot = FALSE , verbose = FALSE , ...) 
  
{ # Begin of function
  ## Update argument names for internal use only
  lon.rng  <- lon
  lat.rng  <- lat
  lev.rng  <- lev
  time.rng <- time
  ## check if file exists and type of ncfile object
  if (is.character(ncfile)) {
    if (!file.exists(ncfile)) {stop(paste("Sorry, the netcdf file '", ncfile, "' does not exist or the path has not been set correctly !",sep =""))}
    ncid <- nc_open(ncfile)     
  } else if (class(ncfile) == "ncdf4") ncid <- ncfile else stop("ncfile format should be a valid netcdf filename or a netcdf id of class 'ncdf4'")  
  ## Read and put attributes in model
  model <- ncatt_get(ncid,0)
  ## Get variable attributes in v1
  namevars <- names(ncid$var)
  if (tolower(param) == "auto") {
    if (ncid$nvars > 1) {
      i <- grep(param, names(ncid$var))
      if (length(i) == 0) i <- as.integer(readline(paste("Choose variable ",paste(namevars,collapse="/") ,"(from 1 - ",length(namevars), "): ",sep = "")))
      if (!is.integer(i)) stop("You should introduce an integer value and at least select one variable") 
    } else i <- 1
    param <- names(ncid$var)[i] # ; rm(i)
    v1 <- ncid$var[[i]] 
  } else {
    v1 <- NULL
    v1 <- eval(parse(text=paste("ncid$var$",param,sep="")))
    if (is.null(v1)) stop(paste("Variable ",param," could not be found !",sep=""))
  }
  ## Get dimensions
  ## Get dimension names
  dimnames <- rep(NA,v1$ndims)
  for (i in 1:v1$ndim) dimnames[i] <- tolower(v1$dim[[i]]$name)
  ## Get lon, lat, lev, time attr and values and update values if necessary
  ## Longitudes
  ilon <- grep("lon", dimnames) ; if (length(ilon) ==0) ilon <- NULL else if (length(ilon)>1) stop("Error in dim lon")
  if (!is.null(ilon)) lon <- eval(parse(text=paste("v1$dim[[",as.character(ilon),"]]",sep=""))) else lon <- NULL
  if (!is.null(ilon)) {
    ilonunit <- grep("unit",names(lon))
    if (length(ilonunit>1)) {
      if (verbose) print(paste("Longitude unit is :",lon$unit,sep=" "))
      lonunit <- eval(parse(text = paste("lon$",names(lon)[ilonunit],sep="")))
      if (length(grep("degrees_east",lonunit))<1) stop("'retrieve.ncdf4' is not suited to extract longitude units different from 'degrees_east'")
    }
  }
  ## browser()
  ## Update longitude values if greenwich is FALSE
  if (!greenwich) {
    id <- lon$vals > 180
    if (sum(id) > 0) {
      if (verbose) print("Convert to non-Greenwich")
      lon$vals[id] <- lon$vals[id] - 360
    }
  } else {
    id <- lon$vals < 0
    if (sum(id) > 0) {
      if (verbose) print("Convert to Greenwich")
      lon$vals[id] <- lon$vals[id] + 360
    }
  }##else if (!(sum(id) > 0)) lon$vals <- lon$vals + 180
  
  ## Latitudes
  ilat <- grep("lat", dimnames) ; if (length(ilat) ==0) ilat <- NULL else if (length(ilat) > 1) stop("Error in dim lat")
  if (!is.null(ilat)) lat <- eval(parse(text=paste("v1$dim[[",as.character(ilat),"]]",sep=""))) else lat <- NULL
  ## Pressure Level if pressure variable / not used for surface variables
  ilev <- grep("lev", dimnames) ; if (length(ilev) ==0) ilev <- NULL else if (length(ilev)>1) stop("Error in dim lev")
  if (!is.null(ilev)) lev <- eval(parse(text=paste("v1$dim[[",as.character(ilev),"]]",sep=""))) else lev <- NULL
  # browser()
  ## Time 
  itime <- grep("tim", dimnames) ; if (length(itime) ==0) itime <- NULL else if (length(itime)>1) stop("Error in dim time")
  if (!is.null(itime)) time <- eval(parse(text=paste("v1$dim[[",as.character(itime),"]]",sep=""))) else time <- NULL
  ## Check and update info 
  ## browser() 
  if (ncdf.check) { 
    ncid2 <- check.ncdf4(ncid,param=param,verbose=verbose) 
    if (length(grep("model",ls())) > 0) model <- ncid2$model 
    if (!is.null(itime)) time <- ncid2$time
    rm(ncid2)
  }
  ## browser()
  if (verbose) print(model$frequency)
  ## Subselect a spatial and a temporal domain
  ## longitude extract range
  if (!is.null(ilon)) {
    if (!is.null(lon.rng)) {
      if (length(lon.rng) > 2) stop("lon.rng should be in the form of c(x1,x2)")
      else if (length(lon.rng) == 1) {
        lon.w <- which((lon$vals-lon.rng) == min(abs(lon$vals-lon.rng)))
        if (verbose) print(paste("Single point extraction / Selected nearest grid cell lon :",as.character(lon$vals[lon.w]),lon$unit,sep=" "))
      }
      else if (length(lon.rng) == 2)  {
        lon.w <- which((lon$vals >= lon.rng[1]) & (lon$vals <= lon.rng[length(lon.rng)]))
        if (verbose) print(paste("Selected longitudes:",paste(as.character(sort(lon$vals[lon.w])),collapse="/"),lon$units,sep=" "))
      }
    } else lon.w <- seq(1,length(lon$vals),1)
    ## lon$vals <- as.vector(lon$vals[lon.w])
    lon$len <- length(lon.w)
  }
  
  ## latitude extract range
  if (!is.null(ilat)) {
    if (!is.null(lat.rng)) {
      if (length(lat.rng) > 2) stop("lat.rng should be in the form of c(y1,y2)")
      if (length(lat.rng) == 1) {
        lat.w <- which((lat$vals-lat.rng) == min(abs(lat$vals-lat.rng)))
        if (verbose) print(paste("Single point extraction / Selected nearest grid cell lat :",as.character(lat$vals[lat.w]),lat$unit,sep=" "))
      }
      if (length(lat.rng) == 2) { 
        lat.w <- which((lat$vals >= lat.rng[1]) & (lat$vals <= lat.rng[length(lat.rng)]))
        if (verbose) print(paste("Selected Latitudes:",paste(as.character(lat$vals[lat.w]),collapse="/"),lat$units,sep=" "))
      }
    } else lat.w <- seq(1,length(lat$vals),1)
    ## lat$vals <- as.vector(lat$vals[lat.w])
    lat$len <- length(lat.w)
  }
  
  ## time extract range
  if (!is.null(itime)) {
    if (!is.null(time.rng)) {
      if (length(time.rng) > 2) stop("time.rng should be in the form of c(year1,year2)")
      if (length(time.rng) == 1) {
        time.w <- which((time$vals-time.rng) == min(abs(time$vals-time.rng)))
        if (verbose) print(paste("Single time extraction:",as.character(time$vals[time.w]),time$unit,sep=" "))
      }
      if (length(time.rng) == 2) {
        if (sum(is.element(time.rng,format.Date(time$vdate,"%Y"))) < 1) stop("Selected time interval is outside the range of the data") 
        time.w <- which((format.Date(time$vdate,"%Y") >= time.rng[1]) & (format.Date(time$vdate,"%Y") <= time.rng[length(time.rng)]))
        if (verbose) {
          if (model$frequency == "mon")
            print(paste("Selected time values:",paste(as.character(format.Date(time$vdate[time.w],"%Y-%m")),collapse="/"),model$frequency,sep=" "))
          else
            print(paste("Selected time values:",paste(as.character(time$vdate[time.w]),collapse="/"),model$frequency,sep=" "))
        }
        if ((length(grep("time.w",ls())) < 1) | (length(time.w)<1)) stop("No time overlapping with selected time interval")
      }
    } else time.w <- seq(1,length(time$vals),1)
    ## Updating time$vals and time$vdate
    time$vdate <- time$vdate[time.w]
    ## time$vals <- as.vector(time$vals[time.w])
    time$len <- length(time.w)
  } 
  ## browser()
  ## level extract range
  if (!is.null(ilev)) {
    if (!is.null(lev.rng)) {
      if (length(lev.rng) > 2) stop("lev.rng should be in the form of c(z1,z2)")
      if (length(lev.rng) == 1) {
        lev.w <- which((lev$vals-lev.rng) == min(abs(lev$vals-lev.rng)))
        if (verbose) print(paste("Single level extraction:",as.character(lev$vals[lev.w]),lev$unit,sep=" "))
      }
      if (length(lev.rng) == 2) { 
        lev.w <- which((lev$vals >= lev.rng[1]) & (lev$vals <= lev.rng[length(lev.rng)]))
        if (verbose) print(paste("Selected Levels:",paste(as.character(lev$vals[lev.w]),collapse="/"),lev$units,sep=" "))
      }
    } else lev.w <- rank(lev$vals)
    ## lev$vals <- as.vector(lev$vals[lev.w])
    lev$len <- length(lev.w)
  }
  ## browser()
  ## Extract values and add Scale Factor and offset if any
  if (verbose) print(paste("Reading data for ",v1$longname,sep=""))
  if ((!is.null(ilon)) & (!is.null(itime))) {  
    diff.lon.w <- diff(rank(lon$vals[lon.w]))
    id2 <- which(diff.lon.w!=1)
    if (!is.null(ilev)) {
      if ((sum(id) > 0) & (sum(id2)!=0)) { ## & !greenwich    
        count <- c(length(lon.w),length(lat.w),length(lev.w),length(time.w))
        lon.w1 <-lon.w[1:id2]
        lon.w2 <- lon.w[(id2+1):length(lon.w)]
        start1 <- c(lon.w1[1],lat.w[1],lev.w[1],time.w[1])
        count1 <- c(length(lon.w1),length(lat.w),length(lev.w),length(time.w))
        val1 <- ncvar_get(ncid,param,start1,count1,collapse_degen=FALSE)
        d1 <- dim(val1)
        dim(val1) <- c(d1[1],prod(d1[2:length(d1)]))
        start2 <- c(lon.w2[1],lat.w[1],lev.w[1],time.w[1])
        count2 <- c(length(lon.w2),length(lat.w),length(lev.w),length(time.w))
        val2 <- ncvar_get(ncid,param,start2,count2,collapse_degen=FALSE)
        d2 <- dim(val2)
        dim(val2) <- c(d2[1],prod(d2[2:length(d2)]))
        val <- rbind(val1,val2)
      } else {
        start <- c(lon.w[1],lat.w[1],lev.w[1],time.w[1])
        count <- c(length(lon.w),length(lat.w),length(lev.w),length(time.w))
        val <- ncvar_get(ncid,param,start,count,collapse_degen=FALSE)
      }
      dim(val) <- count
      ## sort longitudes ...
      lon$vals <- lon$vals[lon.w]
      lon.srt <- order(lon$vals)
      if (sum(diff(lon.srt)!=1)) {
        if (verbose) print("Sort Longitudes") 
        ## lon.srt <- order(lon$vals)
        lon$vals <- lon$vals[lon.srt]
      } ## else lon.srt <- seq(1,length(lon$vals),1)
      lat$vals <- lat$vals[lat.w]
      lat.srt <- order(lat$vals)
      if (sum(diff(lat.srt)!=1)) {
        if (verbose) print("Sort Latitudes") 
        ## lat.srt <- order(lat$vals)
        lat$vals <- lat$vals[lat.srt]
      } ## else lat.srt = seq(1,length(lat$vals),1)
      val <- val[lon.srt,lat.srt,,]
      dim(val) <- count
    }
    else {
      if ((sum(id) > 0) & (sum(id2)!=0)) { ## & !greenwich
        count <- c(length(lon.w),length(lat.w),length(time.w))
        lon.w1 <-lon.w[1:id2]
        lon.w2 <- lon.w[(id2+1):lon$len]
        start1 <- c(lon.w1[1],lat.w[1],time.w[1])
        count1 <- c(length(lon.w1),length(lat.w),length(time.w))
        val1 <- ncvar_get(ncid,param,start1,count1,collapse_degen=FALSE)
        d1 <- dim(val1)
        dim(val1) <- c(d1[1],prod(d1[2:length(d1)]))
        start2 <- c(lon.w2[1],lat.w[1],time.w[1])
        count2 <- c(length(lon.w2),length(lat.w),length(time.w))
        val2 <- ncvar_get(ncid,param,start2,count2,collapse_degen=FALSE)
        d2 <- dim(val2)
        dim(val2) <- c(d2[1],prod(d2[2:length(d2)]))
        val <- rbind(val1,val2)
        stopifnot((d1[2]==d2[2]) | (d1[3]==d2[3]))
        ##dim(val) <- c((d1[1]+d2[1]),d1[2],d2[3])
      } else {
        start <- c(lon.w[1],lat.w[1],time.w[1])
        count <- c(length(lon.w),length(lat.w),length(time.w))
        val <- ncvar_get(ncid,param,start,count)
      }
      dim(val) <- count   
      lon$vals <- lon$vals[lon.w]
      lon.srt <- order(lon$vals)
      if (sum(diff(lon.srt)!=1)!=0) {
        if (verbose) print("Sort Longitudes") 
        ##lon.srt <- order(lon$vals)
        lon$vals <- lon$vals[lon.srt]
      } ## else lon.srt <- seq(1,length(lon$vals),1)
      lat$vals <- lat$vals[lat.w]
      lat.srt <- order(lat$vals)
      if (sum(diff(lat.srt)!=1)!=0) {
        if (verbose) print("Sort Latitudes") 
        ## lat.srt <- order(lat$vals)
        lat$vals <- lat$vals[lat.srt]
      } ## else lat.srt = seq(1,length(lat$vals),1)
      val <- val[lon.srt,lat.srt,]
      ##dim(val) <- count
    }
  }
  ##browser()
  ## Convert units
  iunit <- grep("unit",names(v1))
  if (length(iunit)>0) {
    text=paste("v1$",names(v1)[iunit],sep="")
    units <- eval(parse(text=text))
    if ((units=="K")) {
      val <- val - 273 
      units <- "degC"
    }
    if ((length(grep("pa",tolower(units)))>0) | (length(grep("N",tolower(units)))>0)) {
      val <- val/100 
      units <- "hPa"
    }
    ## browser()
    if ((units=="Kg/m^2/s") | (units=="kg m-2 s-1")) {
      val <- val * (24*60*60) 
      units <- "mm/day"
    }
    if (verbose) print(paste("Data converted to unit:",units, sep= " "))
  }
  ## replace missval by NA
  if (miss2na) {
    imissval <- grep("miss",names(v1))
    if (length(imissval)>0) {
      text=paste("v1$",names(v1)[imissval],sep="")
      missval <- eval(parse(text=text))
      val[val == missval] <- NA
    }
    if (verbose) print(paste(as.character(sum(is.na(val))),"missing values have been replaced by NA" , sep = " "))
  }
  
  ## browser()
  if (verbose) print("Done !")
  
  ## Copy "filename" attribute to model attributes
  model$filename <- ncid$filename
  
  ## close netcdf4 file
  nc_close(ncid)
  
  ## Create output and save attributes to the results # 
  
  ##browser()
  d <- dim(val)
  if (length(ilev)==1)
    dim(val) <- c(d[1]*d[2],d[4])
  else
    dim(val) <- c(d[1]*d[2],d[3])
  ## d <- dim(val)
  ##create a zoo object z
  z <- zoo(x=t(val),order.by=time$vdate)
  ## Add attributes to z
  if (!is.null(v1)) {
    attr(z,"variable") <- param
    attr(z,"longname") <- v1$longname
    attr(z,"units") <- units
    attr(z,"dimensions") <- d
  }  
  if (!is.null(ilon)) {
    attr(z,"longitude") <- lon$vals
    attr(z,"greenwich") <- greenwich
  }
  if (!is.null(ilat)) {
    attr(z,"latitude") <- lat$vals
  }
  if (!is.null(ilev)) {
    attr(z,"level")        <- lev$vals
    attr(z,"levelUnit")    <- lev$units
  }
  if (!is.null(itime)) {
    attr(z,"calendar") <- time$calendar
  }
  ## Add attributes
  attr(z, "file")           <- ifelse(!is.null(model$filename),model$filename, NA)
  attr(z, "title")          <- ifelse(!is.null(model$title), model$title, NA)
  
  ##attr(z, "project_id")     <- ifelse(!is.null(model$project_id), model$project_id, NA)
  attr(z, "source")         <- ifelse(!is.null(model$project_id), model$project_id, NA)
  attr(z, "model_id")       <- ifelse(!is.null(model$model_id), model$model_id, NA)
  attr(z, "experiment_id")  <- ifelse(!is.null(model$experiment_id),model$experiment_id, NA)
  attr(z, "realization")    <- ifelse(!is.null(model$realization),model$realization,NA)
  attr(z, 'timeunit')       <- ifelse(!is.null(model$frequency),model$frequency, NA)
  attr(z, 'frequency')      <- 1
  attr(z, 'type')           <- ifelse(!is.null(model$type),model$type, 'field')
  ## attr(z, "timestamp")      <- date()
  ## attr(z, "anomaly")        <- FALSE
  ## attr(z, "time:method")    <- NA
  ## attr(z, "spatial:method") <- NA
  ##attr(z, "title")          <- model$title
  attr(z, "URL")            <- "http://climexp.knmi.nl/"
  attr(z, "call")           <- match.call()
  ## attr(z, "history")        <- NA
  attr(z, "institution")    <- NA 
  attr(z, "reference")      <- NA
  attr(z, "history")        <- history.stamp(z)
  class(z)                  <- c("field",model$frequency,"zoo")
  
  ## plot the results
  if (plot) map(z,...)
  
  invisible(z)
  
} # End of the function

## e.g. RUN !
## gcm <- retrieve.ncdf4(ncfile="CMIP5.monthly/rcp45/tas_Amon_ens_rcp45_00.nc",param="auto",lon=c(-20,20),lat=c(40,60), lev = NULL,time = c(1960,2001), ncdf.check=TRUE,miss2na = TRUE,verbose = TRUE)
