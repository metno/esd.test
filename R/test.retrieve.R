## 

t2m <- retrieve.ncdf4("data/MERRA/MERRA_t2m_mon.nc",greenwich=TRUE)
attr(t2m,"unit") <- "K"
t2m <- t2m - 273
dev.new() ; map(t2m)

t2m <- retrieve.ncdf4("/klimadata/work/abdelkaderm/data/merra/merra_t2m.nc",greenwich=FALSE)
dev.new() ; map(t2m)

t2m <- retrieve.ncdf4("/klimadata/work/abdelkaderm/data/merra/merra_t2m.nc",greenwich=FALSE)
dev.new() ; map(t2m)

t2m <- retrieve.ncdf4("CMIP3.monthly/20c3m-sresa1b/tas_cmip3_03_144.nc",greenwich=FALSE)
dev.new() ; map(t2m)

ncfile <-  
print(file) 
t2m <- retrieve.ncdf4("CMIP5.monthly/rcp45/tas_Amon_ens_rcp45_005.nc",greenwich=FALSE)
dev.new() ; map(t2m)

t2m <- retrieve.ncdf4("data/merra/MERRA_t2m_mon.nc",greenwich=FALSE,lon=c(-10,20),lat=c(40,70))
attr(t2m,"unit") <- "K"
t2m <- t2m - 273
dev.new() ; map(t2m)

t2m <- retrieve.ncdf4("data/merra/MERRA_t2m_mon.nc",greenwich=TRUE,lon=c(-60,60),lat=c(-60,60))
attr(t2m,"unit") <- "K"
t2m <- t2m - 273
dev.new() ; map(t2m)

t2m <- retrieve.ncdf4("data/MERRA/MERRA_t2m_mon.nc",greenwich=FALSE,lon=c(-60,60),lat=c(-60,60))
attr(t2m,"unit") <- "K"
t2m <- t2m - 273
dev.new() ; map(t2m)

t2m <- retrieve.ncdf4("/data/MERRA/merra_t2m.nc",greenwich=TRUE,lon=c(-10,20),lat=c(40,70))
attr(t2m,"unit") <- "K"
t2m <- t2m - 273
dev.new() ; map(t2m)

t2m <- retrieve.ncdf4("/klimadata/work/abdelkaderm/data/merra/merra_t2m.nc",greenwich=FALSE,lon=c(-10,20),lat=c(40,70))
dev.new() ; map(t2m)

t2m <- retrieve.ncdf4("/klimadata/work/abdelkaderm/data/merra/merra_t2m.nc",greenwich=FALSE,lon=c(-10,20),lat=c(40,70))
dev.new() ; map(t2m)

t2m <- retrieve.ncdf4("CMIP3.monthly/20c3m-sresa1b/pr_cmip3_03_144.nc",greenwich=FALSE,lon=c(-10,20),lat=c(40,70))
dev.new() ; map(t2m)

t2m <- retrieve.ncdf4("CMIP3.monthly/20c3m-sresa1b/tas_cmip3_03_144.nc",greenwich=FALSE,lon=c(-10,20),lat=c(40,70))
dev.new() ; map(t2m)

t2m <- retrieve.ncdf4("CMIP5.monthly/rcp45/tas_Amon_ens_rcp45_005.nc",greenwich=FALSE,lon=c(-10,20),lat=c(40,70))
dev.new() ; map(t2m)

t2m <- retrieve.ncdf4("CMIP3.monthly/20c3m-sresa1b/pr_cmip3_03_144.nc",greenwich=FALSE)
dev.new() ; map(t2m)
