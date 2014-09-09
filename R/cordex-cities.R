## main script for cordex cities extraction

##

cordex.station <- function(param=NULL) {
  
  europe.cities <- toupper(c("Amsterdam",
                             "Andorra",
                             "Athens",
                             "Belgrade",
                             "Berlin",
                             "Bern",
                             "Bratislava",
                             "Brussels" ,
                             "Bucuresti", 
                             "Budapest", ## comment for t2m
                             "Chisinau",
                             "Copenhagen", 
                             "Dublin", 
                             "Helsinki", 
                             "Kiev",
                             "Lisbon", ## comment for t2m
                             "Ljubljana",
                             "London",
                             "Luxembourg",
                             "Madrid",
                             "Minsk",
                             "Monaco",
                             "Moscow",
                             "Nicosia",
                             "Nuuk",
                             "Oslo",
                             "Paris",
                             "Podgorica",
                             "Prague",
                             "Reykjavik",
                             "Riga",
                             "Rome", ## comment for t2m
                             "San Marino",
                             "Sarajevo",
                             "Skopje",
                             "Sofia",
                             "Stockholm",
                             "Tallinn",
                             "Tirana",
                             "Vaduz",
                             "Valletta", ## comment for t2m
                             "Vatican",
                             "Vienna",
                             "Vilnius",
                             "Warsaw", ## comment for t2m
                             "Zagreb"))
  x <- read.csv(file ="/home/abdelkaderm/SHARED/data/worldcapitals.txt",head=TRUE,sep="\t")
  
  lon <- lat <- rep(NA,length("europe.cities"))
  for (i in 1 : length(europe.cities)) {
    print(i)
    id <- grep(substr(tolower(europe.cities[i]),1,3),substr(tolower(as.character(x$Name)),1,3))
    lon[i] <- x$Longitude[id]
  lat[i] <- x$Latitude[id]
  }
  europe.country <- toupper(c("Netherlands",
                              "Andorra",
                              "Greece",
                              "Serbia",
                              "Germany",
                              "Switzerland",
                              "Slovakia",
                              "Belgium",
                              "Romania",
                              "Hungary", ## comment for t2m
                              "Moldova",
                              "Denmark",
                              "Ireland",
                              "Finland",
                              "Ukraine",
                              "Portugal", ## comment for t2m
                              "Slovenia",
                              "United Kingdom",
                              "Luxembourg",
                              "Spain",
                              "Belarus",
                              "Monaco",
                              "Russia",
                              "Cyprus",
                              "Greenland",
                              "Norway",
                              "France",
                              "Montenegro",
                              "Czech Republic",
                              "Iceland",
                              "Latvia",
                              "Italy", ## comment for t2m
                              "San Marino",
                              "Bosnia & Herzegovina",
                              "Macedonia",
                              "Bulgaria",
                              "Sweden",
                              "Estonia",
                              "Albania",
                              "Liechtenstein",
                              "Malta", ## comment for t2m
                              "Holy See",
                              "Austria",
                              "Lithuania",
                              "Poland", ## comment for t2m
                              "Croatia"))

  
  ## 1. Retrieving data into y and map the stations
                                        #browser()
  X <-  NULL ; ss <-  NULL
  for (i in 1:length(europe.cities)) {
    print(paste(i, europe.cities[i],europe.country[i],"ECAD"))
    ss1 <- select.station(loc = europe.cities[i] , cntr = europe.country[i] , src = "ECAD" , param = param, it = c(1982,2012)) 
    
    if (is.null(ss1)) {
      ## browser()
      ## dx <- 0.05 ; k <- 1
      ## while (is.null(ss1) & (k < 10)) {
      ss1 <- select.station(cntr = europe.country[i] , src = "ECAD" , param = param , it = c(1982,2012))
      
      if (!is.null(ss1)) {
        d <- distAB(lon=lon[i],lat=lat[i],lons=ss1$longitude,lats=ss1$latitude)
        id <- d ==min(d)
        class(ss1) <- "data.frame"
        ss1 <- ss1[id,]
      }
      else ss1 <- NULL
      ##dx <- dx+1
      ##k <- k+1
      ## }
    }
    ## if (i==10) browser()
    if (!is.null(ss1)) {
      class(ss1) <- "data.frame"
      if (dim(ss1)[1]>1) ss1 <- ss1[1,]
    }
    ## if (!is.null(ss1)) ss <- merge(ss,ss1,all=TRUE)
    ## y  <- station(loc = europe.cities[i] , cntr = europe.country[i] , src = "ECAD" , path = "/disk1/ECAD/" , param = "precip")
    if (!is.null(ss1)) y <-  station(ss1, param=param,path = "/disk1/data/ECAD/") else y <- NULL
    ## Downscaling cmip5 results
    ## browser()
    
    if (!is.null(y)) {
      Z <- coredata(y)
      y[Z < -90] <- NA
      X <- combine(X,y)
      plot(y)
    }
  }
  
  save(X,file=paste("/home/abdelkaderm/SHARED/data/",param,"_station_cordexcities.rda",sep=""))
  map.station(X,col="darkred",bg="red",cex=1,xlim=c(-30,40),ylim=c(30,90))
  text(labels=attr(X,"location"),x=attr(X,"longitude"),y=attr(X,"latitude"),cex=0.8,pos=1)
  
}
                                        #ss <- select.station(stid = "100601" , loc = "Oslo" , cntr="NORWAY" , src = "ECAD" , param = "t2m") 
#y <- station(ss,path="/disk1/data/ECAD/")  
#ds <- DSCMIP5.t2m.zoo(y,path="CMIP5.monthly/rcp45",pattern="tas",
#                        years=seq(1900,2100,by=1),plot=TRUE,dx=15,dy=15,
#                        reanalysis="ERA40_t2m_mon.rda")
