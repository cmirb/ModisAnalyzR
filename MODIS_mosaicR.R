# This script contains: 

# Pre-processing of MODIS data (in HDF format)
# including 
# producing mosaiced, reporjected and resampled geotiffs

# pixelwise analysis of geotiffs to identify snow cover, cloud cover, and no data values

# annual and seasonal subsets and calculation of some spatial statistics


###### Pre-Processing #####################

# clean up work spce
rm(list=ls())

# load required packages
library(stringi)
library(raster)
library(gdalUtils)
library(rgdal)

# set working directory
WorkDir <- 'E:/'
setwd(WorkDir)

# Output
dir.create('Mosaic')

# list all data (note that first data needs to be downloaded here https://urs.earthdata.nasa.gov)

ALLmodisdata <- list.files('Data/2001/',
                           full.names = T,
                           pattern = '.tif',
                           recursive = T)

#ALLmodisdata <- lapply(ALLmodisdata, raster)

# identify unique dates
ALLmodisDATE <- stri_sub(ALLmodisdata,
                         20, # von
                         27) # bis


# get humanly readable dates
dates_all <- as.Date(ALLmodisDATE, '%Y%j')



# find missing dates 
date_range <- seq(min(dates_all), max (dates_all), by = 1)
missing_days <- as.data.frame(date_range[!date_range %in% dates_all])

# find days with data
all_days <- as.data.frame(date_range[date_range %in% dates_all])

# identify days and years ####
uniqueDATE <- unique(ALLmodisDATE)

# save missing days as csv (78 missing days)
# write.table(missing_days, file = "Results/missing_days.csv", sep =";", dec = ".", row.names = T, col.names = T)
 #write.table(all_days, file = "Results/data_days.csv", sep =";", dec = ".", row.names = T, col.names = T)



# set domain to ensure same extent
# domain <- raster(domain)

# load indivudial rasters for one day as test  
r1 <- raster("./Data/2000/MOD10A1_A2000063_h24v05_006_2016061221843_MOD_Grid_Snow_500m_NDSI_Snow_Cover_e1809c9e.tif") # load rasters
r1[r1>250] = NA # set fill values to NA to avoid overlay error
origin(r1) <- 0 # won't merge/ mosaic otherwise 

r2 <- raster("./Data/2000/MOD10A1_A2000063_h24v06_006_2016061221950_MOD_Grid_Snow_500m_NDSI_Snow_Cover_e18c0d08.tif") # load rasters
r2[r2>250] = NA # set fill values to NA to avoid overlay error
origin(r2) <- 0

r3 <- raster("./Data/2000/MOD10A1_A2000063_h25v05_006_2016061221907_MOD_Grid_Snow_500m_NDSI_Snow_Cover_e18cd703.tif") # load rasters
r3[r3>250] = NA # set fill values to NA to avoid overlay error
origin(r3) <- 0

r4 <- raster("./Data/2000/MOD10A1_A2000063_h25v06_006_2016061222329_MOD_Grid_Snow_500m_NDSI_Snow_Cover_e18c0d0a.tif") # load rasters
r4[r4>250] = NA # set fill values to NA to avoid overlay error
origin(r4) <- 0

r5 <- raster("./Data/2000/MOD10A1_A2000063_h26v06_006_2016061222107_MOD_Grid_Snow_500m_NDSI_Snow_Cover_e18dcaa7.tif") # load rasters
r5[r5>250] = NA # set fill values to NA to avoid overlay error
origin(r5) <- 0



# crop to domain 
domain <- extent(80,89,26,31)
r1 <- crop(r1, domain)
r1 <- extend(r1, domain)

r2 <- crop(r2, domain)
r2 <- extend(r2, domain)

r3 <- crop(r3, domain)
r3 <- extend(r3, domain)

r4 <- crop(r4, domain)
r4 <- extend(r4, domain)

r5 <- crop(r5, domain)
r5 <- extend(r5, domain)

## plot to check for plausibility
plot(r1)
plot(r2)
plot(r3)
plot(r4)
plot(r5)

# check extents 
extent(r1)
extent(r2)
extent(r3)
extent(r4)
extent(r5)

########### merge rasters 


# mosaic  
# rasterOptions(tolerance = 0.1)
mos1 <- mosaic(r1, r2, r3, r4, r5, fun = mean, quick = T, extent = T )
plot(mos1)


# for loop to mosaic ############

# global variable domain
domain <- extent(80,89,26,31) # extent can be adjusted 

# i=1
for (i in 1:length(uniqueDATE)){
  
  
  cat(i, '\t') # returns number of file it'S working on
  # start time
  s <- Sys.time()
  # load day data
  SINGLEdata <- ALLmodisdata[stri_sub(ALLmodisdata,
                                      20,
                                      27) == uniqueDATE[i]]
  
  
  # load rasters
  if(length(SINGLEdata)==5){
    for (r in 1:length(SINGLEdata)) {
      eval(parse(text = paste0("r",r," <- raster('./",SINGLEdata[r],"')"))) # load rasters
      eval(parse(text = paste0("r",r,"[r",r,">250] = NA"))) # set fill values to NA to avoid overlay error
      eval(parse(text = paste0("origin(r",r,") <- 0")))# origin(r1) <- 0 # won't merge/ mosaic otherwise 
    }
    
  }else{
    cat(uniqueDATE[i],' has incomplete data \n')
    for (r in 1:length(SINGLEdata)) {
      eval(parse(text = paste0("r",r," <- raster('./",SINGLEdata[r],"')"))) # load rasters
      eval(parse(text = paste0("r",r,"[r",r,">250] = NA"))) # set fill values to NA to avoid overlay error
      eval(parse(text = paste0("origin(r",r,") <- 0")))# origin(r1) <- 0 # won't merge/ mosaic otherwise 
    }
  }
  
  # crop rasters
  # -> for
  # exists? r1-5
  # if for a given day there are <5 tiles, this loop makes sure that the remaining
  # space is filled with NA values
  # 
  if(length(SINGLEdata) == 5) {
    for (r in 1:length(SINGLEdata))
      r1 <- crop(r1, domain)
    r1 <- extend(r1, domain) 
    
    r2 <- crop(r2, domain)
    r2 <- extend(r2, domain)
    
    r3 <- crop(r3, domain)
    r3 <- extend(r3, domain)
    
    r4 <- crop(r4, domain)
    r4 <- extend(r4, domain)
    
    r5 <- crop(r5, domain)
    r5 <- extend(r5, domain) 
    
    mosaic1 <- mosaic(r1, r2, r3, r4, r5, fun = mean) # there are other ways to fill the overlay areas such as nearest neighbor etc.
    
  }else{
    if(length(SINGLEdata) == 4) {
      for (r in 1: length(SINGLEdata))
        r1 <- crop(r1, domain)
      r1 <- extend(r1, domain)
      
      r2 <- crop(r2, domain)
      r2 <- extend(r2, domain)
      
      r3 <- crop(r3, domain)
      r3 <- extend(r3, domain)
      
      r4 <- crop(r4, domain)
      r4 <- extend(r4, domain)  
      mosaic1 <- mosaic(r1,r2,r3,r4, fun = mean)
    }else{
      if(length(SINGLEdata) == 3) {
        for (r in 1: length(SINGLEdata))
          r1 <- crop(r1, domain)
        r1 <- extend(r1, domain)
        
        r2 <- crop(r2, domain)
        r2 <- extend(r2, domain)
        
        r3 <- crop(r3, domain)
        r3 <- extend(r3, domain)
        mosaic1 <- mosaic(r1,r2,r3, fun = mean)
      }else{
        if(length(SINGLEdata) == 2) {
          r1 <- crop(r1, domain)
          r1 <- extend(r1, domain)
          
          r2 <- crop(r2, domain)
          r2 <- extend(r2, domain)
          mosaic1 <- mosaic(r1,r2, fun = mean)
        }else{
          if(length(SINGLEdata) == 1) {
            r1 - crop(r1, domain)
            r1 <- extend(r1, domain)
            mosaic1 <- r1
          }
        }
      }
    }
  }
  
  
  # mosaic rasters
  #mos1 <- mosaic(r1, r2, r3, r4, r5, fun = max, quick = T, extent = T )
  # plot(mosaic1, main=paste(uniqueDATE[i]))
  
  exportFOLDER <- stri_sub(uniqueDATE[i],
                           1,
                           4)  # identify export folder
  
  # write raster into export folder (individual ones for each year)
  writeRaster(mosaic1,
              paste0('Mosaic/',
                     exportFOLDER,
                     '/', uniqueDATE[i],
                     '_NDSI_SC_500m.tif'),
              overwrite = T)
  
  e <- Sys.time()
  # returns how long one loop took
  cat(e-s,'\n')
  
  gc() # important: this cleans the garbage can 
  # otherwise memory is used up
  rm(r1, r2, r3, r4, r5)
  rm(mosaic1) # remove all files from global environment so the loop can start fresh :)
}


# preabmle

rm(list=ls())

WorkDir <- "E:/"
setwd(WorkDir)

library(stringi)
library(raster)

#  List files in Mosaic year 2000

# create list of files to extract dates 
ALLmodisdata <- list.files('Data',
                           full.names = F,
                           pattern = '.tif',
                           recursive = T)


ALLmodisdummy <- list.files('Mosaic/',
                            full.names = F,
                            pattern = 'dummy.tif',
                            recursive = T)

AlldummyDATES <- stri_sub(ALLmodisdummy,
                          6,
                          13)

# extract Julian dates
ALLmodisDATE <- stri_sub(ALLmodisdata,
                         15, # von
                         22) # bis


# identify days and years 
uniqueDATE <- unique(ALLmodisDATE)

#missing(uniqueDATE)


# get humanly readable dates
dates_all <- as.Date(ALLmodisDATE, '%Y%j')


# find missing dates 
date_range <- seq(min(dates_all), max (dates_all), by = 1) # load all available dates as sequence
missing_days <- as.data.frame(date_range[!date_range %in% dates_all]) # find missing dates in sequence

hist(missing_days)


uniqueMISSING <- unique(date_range[!date_range %in% dates_all]) #create vector for missing dates to be filled with dummy files later

# find days with data
all_days <- as.data.frame(date_range[date_range %in% dates_all])

#uniqueDATE <- as.Date(uniqueDATE,'%Y%j')


library(timeDate)

uniqueMISSING <- unique(date_range[!date_range %in% dates_all])

uniqueMISSING <- format(uniqueMISSING, '%Y%j')


# find missing dates

# create dummy raster 


r1 <- raster("./Mosaic/2000/2000058__NDSI_SC_500m.tif")
extent(r1)

domain <- extent(r1)
nrows(r1)


#### test for 2019 days 1-100 
library(raster)
library(rgdal)

test_path <- "./Mosaic/test_2019/"

test_2019 <- list.files(test_path,
                        full.names = T,
                        pattern = ".sgrd")


test_2019
test_2019_stack <- stack(test_2019)

crs(test_2019_stack) <- '+proj=longlat +datum=WGS84 +no_defs'

crs(test_2019_stack)

extent(test_2019_stack)

plot(test_2019_stack)


hist(test_2019_stack)
day_2019001 <- raster("./Mosaic/test_2019/2019001_NDSI_500m.sgrd")
plot(day_2019001)
hist(day_2019001)
rast_2019 <- lapply( test_2019, raster)

############create dummy files for missing days ##########


WorkDir <- "E:/"
setwd(WorkDir)


library(raster)

dir.create("./Mosaic/dummy")
path <- ("./Mosaic/dummy/")

WorkDir <- "./Mosaic/dummy/"
setwd(WorkDir)


# create dummy file

dummy <- raster(nrows= 1200, 
                ncols = 2400, 
                xmn = 80, 
                xmx = 89, 
                ymn = 26, 
                ymx = 31) # from scratch yo crs<-('+proj=longlat +datum=WGS84 +no_defs') ?????????


# set values na

dummy <- setValues(dummy, NA)

# add crs 
crs(dummy) <- '+proj=longlat +datum=WGS84 +no_defs'


dummy <- projectRaster(dummy, crs = "+proj=longlat +datum=WGS84 +no_defs",method = 'ngb',res = 0.004200)


# write dummy
writeRaster(dummy,"dummy.tif", overwrite=F)

dummy <- raster("./dummy.tif")
dummy


# for loop to write dummies
i=1
for (i in 1:length(uniqueMISSING)){
  
  cat(i, '\t') # currently which file
  s <- Sys.time() # starting time 
  
  # create dummy file
  
  # domain <- extent(80,90,26,31)
  domain <- extent(r1)
  dummy <- raster(nrows=1200, 
                  ncols = 2400, 
                  xmn = 80, 
                  xmx = 90, 
                  ymn = 26, 
                  ymx = 31) # from scratch yo crs<-('+proj=longlat +datum=WGS84 +no_defs') ?????????
  
  
  
  # set values na
  
  dummy <- setValues(dummy, NA)
  origin(dummy) <- 0
  
  # add crs 
  crs(dummy) <- '+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0'
  
  
  
  # dummy <- crop(dummy, domain)
  # dummy <- extend(dummy,domain)
  # write dummy
  
  
  
  exportFOLDER <- stri_sub(uniqueMISSING[i], # specify export folder as according to date
                           1,
                           4)
  
  
  writeRaster(dummy,
              paste0('E:/Mosaic/',
                     exportFOLDER,
                     '/', uniqueMISSING[i],
                     '_NDSI_SC_500m_dummy.tif'),
              overwrite = T)
  
  e <- Sys.time()
  # Ausgabe der Dauer
  cat(e-s,'\n')
  
  
}



# pixelwise analysis ###################################

rm(list = ls())

library(raster)
### basic steps to be included in the loop

# this entire part arguably could have been coded better
# definitely lacks automatized processes

# load datasets for each year
WorkDir <- ("E:/Mosaic/")
setwd(WorkDir)


Mod2000 <- list.files("2000/",
                      pattern ='.tif',
                      all.files = T,
                      full.names = T,
                      recursive = T)

Mod2001 <- list.files("2001/",
                      pattern ='.tif',
                      all.files = T,
                      full.names = T,
                      recursive = T)

Mod2002 <- list.files("2002/",
                      pattern ='.tif',
                      all.files = T,
                      full.names = T,
                      recursive = T)


Mod2003 <- list.files("2003/",
                      pattern ='.tif',
                      all.files = T,
                      full.names = T,
                      recursive = T)

Mod2004 <- list.files("2004/",
                      pattern ='.tif',
                      all.files = T,
                      full.names = T,
                      recursive = T)

Mod2005 <- list.files("2005/",
                      pattern ='.tif',
                      all.files = T,
                      full.names = T,
                      recursive = T)

Mod2006 <- list.files("2006/",
                      pattern ='.tif',
                      all.files = T,
                      full.names = T,
                      recursive = T)

Mod2007 <- list.files("2007/",
                      pattern ='.tif',
                      all.files = T,
                      full.names = T,
                      recursive = T)


Mod2008 <- list.files("2008/",
                      pattern ='.tif',
                      all.files = T,
                      full.names = T,
                      recursive = T)


Mod2009 <- list.files("2009/",
                      pattern ='.tif',
                      all.files = T,
                      full.names = T,
                      recursive = T)


Mod2010 <- list.files("2010/",
                      pattern ='.tif',
                      all.files = T,
                      full.names = T,
                      recursive = T)


Mod2011 <- list.files("2011/",
                      pattern ='.tif',
                      all.files = T,
                      full.names = T,
                      recursive = T)

Mod2012 <- list.files("2012/",
                      pattern ='.tif',
                      all.files = T,
                      full.names = T,
                      recursive = T)


Mod2013 <- list.files("2013/",
                      pattern ='.tif',
                      all.files = T,
                      full.names = T,
                      recursive = T)

Mod2014 <- list.files("2014/",
                      pattern ='.tif',
                      all.files = T,
                      full.names = T,
                      recursive = T)

Mod2015 <- list.files("2015/",
                      pattern ='.tif',
                      all.files = T,
                      full.names = T,
                      recursive = T)

Mod2016 <- list.files("2016/",
                      pattern ='.tif',
                      all.files = T,
                      full.names = T,
                      recursive = T)

Mod2017 <- list.files("2017/",
                      pattern ='.tif',
                      all.files = T,
                      full.names = T,
                      recursive = T)

Mod2018 <- list.files("2018/",
                      pattern ='.tif',
                      all.files = T,
                      full.names = T,
                      recursive = T)
Mod2019 <- list.files("2019/",
                      pattern ='.tif',
                      all.files = T,
                      full.names = T,
                      recursive = T)






##### loop 

# plausibility check
# data2000 <- data.frame(nrow = 1200, ncol = 2400)
# str(data2000)
# plot(data2000)

# Mod2000 <- Mod2000[5:15]


# first make dummy rasters for three parameters: clouds / snow/ nodata values
# these will later ne overwritten
# what's important is that they have the exact same extents as the raster data to be analyzed

# Clouds
cloudraster <- raster(Mod2019[1])
cloudraster <- readAll(cloudraster)
cloudraster@data@values <- rep(0,2880000)
#plot(cloudraster)

# Snow
snowraster <- raster(Mod2019[1])
snowraster <- readAll(snowraster)
snowraster@data@values <- rep(0,2880000)

# no data
nodataraster <- raster(Mod2019[1])
nodataraster <- readAll(nodataraster)
nodataraster@data@values <- rep(0,2880000)

#Mod2019 for clouds
ST <- Sys.time()
for (i in 1:length(Mod2019)){ 
  
  
  # get data for each cell in each tif
  # call file
  rast <- raster(Mod2019[i])
  rast <- readAll(rast)
  
  # Clouds data
  # clouds are indicated by the pixel value 250 
  data1 <- rast@data@values
  
  # set everything below and above 250 to NA
  data1[data1 <= 249] <- 0
  data1[data1 >= 251] <- 0
  
  # set all 250 (cloudy days) to 1
  data1[data1 == 250] <- 1 
  data1[is.na(data1)] <- 0
  
  # Snow data
  data2 <- rast@data@values
  
  # set everything below 0 and above 100 to NA
  # 0-100 are NDSI values
  data2[data2 <= 0] <- 0
  data2[data2 >= 101] <- 0
  data2[is.na(data2)] <- 0
  
  # NO DATA Count
  data3 <- rast@data@values
  data3[data3 < 200] <- 0
  data3[data3 == 200] <- 1 # 
  data3[data3 == 201] <- 1
  data3[data3 == 254] <- 1
  data3[data3 == 255] <- 1
  data3[data3 == 211] <- 1
  data3[data3 >= 2] <- 0
  data3[is.na(data3)] <- 0
  
  # combine datasets
  cloudraster@data@values <- cloudraster@data@values + data1
  snowraster@data@values <- snowraster@data@values + data2
  nodataraster@data@values <- nodataraster@data@values + data3
  
  # keep script neat and productive
  rm(data1,data2,data3,rast)
  gc()
}
ET <- Sys.time()
CT <- ET-ST
rm(ET,ST)
CT

# days
daysraster <- raster(Mod2019[1])
daysraster <- readAll(daysraster)
daysraster@data@values <- rep(i,2880000)

# Export annual grids
writeRaster(cloudraster,
            'E:/Results/CloudCover/clouds2019.tif', overwrite = T)
writeRaster(snowraster,
            'E:/Results/SnowCover/snow2019.tif', overwrite = T)
writeRaster(nodataraster,
            'E:/Results/NoDataCover/nodata2019.tif', overwrite = T)



# calculate mean NDSI per year
NDSI2019 <- snowraster/(daysraster-nodataraster-cloudraster)

# export annual NDSI raster
writeRaster(NDSI2019,
            'E:/Results/NDSIannual/NDSI2019.tif', overwrite = T)

# get number of days with snow observations for each pixel
obs <- daysraster - cloudraster - nodataraster
plot(cloudraster)
cloudsdata <- cloudraster@data@values
#max(cloudsdata)

plot(obs)
plot(NDSI2019)
NDSIdata <- NDSI2019@data@values
max(NDSIdata)



# write dummy raster
# fill dummy raster will values from loop to get cloudy days per year
# problem: can't rasterize vector data2000
# fix this shit yo


clouds2000 <- raster1

clouds2000 <- rasterize(data2000, raster1)



i = 1

for (i in 1:length(Mod2000)){
  
  
  # get data for each cell in each tif
  # call file
  rast <- raster(Mod2000[i])
  plot(rast)
  #load values into memory
  raster1 <- readAll(rast)
  rm(rast)
  # raster1
  # get data
  data1 <- raster1@data@values
  
  data1
  # set everything below and above 250 to NA
  
  
  data1[data1 <= 29] <- 0 # snow cover threshold
  # data1
  
  data1[data1 >= 101] <- NA # anything thats not snow cover fraction hurr durr
  
  #  data1
  # hist(data1)
  
}


# export annual means as raster stacks (netcdf4)
rm(list = ls())

library(raster)
library(ncdf4)



# load dataset for each year
WorkDir <- ("E:/Results/")
setwd(WorkDir)

# NDSI Raster Stack
NDSIannual <- list.files("NDSIannual/",
                         pattern = ".tif",
                         all.files = T,
                         full.names = T)


NDSIannualstack <- stack(NDSIannual)

writeRaster(NDSIannualstack,
            filename = "NDSIannual/NDSIannual_stack.nc",
            overwrite = T,
            format = "CDF",
            varname = "NDSI Snow Cover Fractions",
            varunit = "SCFRAC",
            longname = "Annual NDSI Snow Cover Fractions - missing observation excluded",
            xname = "lon",
            yname = "lat",
            zname = "year",
            zunit = "year")


# Clouds Raster Stack
Cloudsannual <- list.files("CloudCover/",
                           pattern = ".tif",
                           all.files = T,
                           full.names = T)

Cloudsannualstack <- stack(Cloudsannual)

writeRaster(Cloudsannualstack,
            filename = "CloudCover/Cloudsannual_stack.nc",
            overwrite = T,
            format = "CDF",
            varname = "Cloud Cover",
            varunit = "No of cloudy days",
            longname = "number of cloudy days per cell over time",
            xname = "lon",
            yname = "lat",
            zname = "year",
            zunit = "year")



# Missing Data Raster Stack
NoDataannual <- list.files("NoDataCover/",
                           pattern = ".tif",
                           all.files = T,
                           full.names = T)

NoDataannualstack <- stack(NoDataannual)

writeRaster(NoDataannualstack,
            filename = "NoDataCover/NoDataannual_stack.nc",
            overwrite = T,
            format = "CDF",
            varname = "No Data Mask",
            varunit = "No of missing observations",
            longname = "number of missing observations per cell over time",
            xname = "lon",
            yname = "lat",
            zname = "year",
            zunit = "year")



# seasonal Subsets and calculation of means #######################
# winter (DJF) julian: 001-059 und 335-365/366
# pre-monsoon (MAM): 060-151
# monsoon (JJAS): 152-273
# post-monsoon (ON): 274-334

# subsetting the data into seasons should be automated but isn't done here
# tried multiple ways which weren't efficient and decided to just to it by hand 
# script needs adjusting, obv


# clean up
rm(list=ls())


library(dplyr)
library(raster)
library(gdalUtils)
library(gWidgetsRGtk2)
library(rgdal)
library(rgl)
library(stringi)

##################### list data
WorkDir <- "E:/"
setwd(WorkDir)


# dir.create("Seasonal Subsets")



ALLmodisDATA <- list.files("Mosaic",
                           full.names = T,
                           pattern = ".tif",
                           recursive = T)


ALLmodisDATE <- stri_sub(ALLmodisDATA,
                         13, # von
                         20) # bis

years <- unique(stri_sub(ALLmodisDATE,
                         1,
                         4))

days <- stri_sub(ALLmodisDATE,5,8)

dates_all <- as.Date(ALLmodisDATE, '%Y%j')

months <- format(dates_all, "%Y%m")
months <- unique(months, 1, 7)


# seasons

# list all geotiffs per year
Mod2000 <- list.files("Mosaic/2000/",
                      pattern ='.tif',
                      all.files = T,
                      full.names = T,
                      recursive = T)

Mod2001 <- list.files("Mosaic/2001/",
                      pattern ='.tif',
                      all.files = T,
                      full.names = T,
                      recursive = T)

Mod2002 <- list.files("Mosaic/2002/",
                      pattern ='.tif',
                      all.files = T,
                      full.names = T,
                      recursive = T)

# ....



##### note: leap years: 2000 / 2004 / 2008 / 2012 / 2016 / 2020

# year 2000 is missing the first couple of months, hence the adjustment
MAM2000 <- Mod2000[6:97]
JJAS2000 <- Mod2000[98:219]
ON2000 <- Mod2000[220:280]
DJF2000 <- c(Mod2000[281:n], Mod2001[1:59])

# note that the years 2000, 2004, 2008, 2012, and 2016 are leap years so the month February will have to be adjusted

# 2001
MAM2001 <- Mod2001[60:151]
JJAS2001 <- Mod2001[152:273]
ON2001 <- Mod2001[274:334]
DJF2001 <- c(Mod2001[335:365], Mod2002[1:59])

# 2002
MAM2002 <- Mod2002[60:151]
JJAS2002 <- Mod2002[152:273]
ON2002 <- Mod2002[274:334]
DJF2002 <- c(Mod2002[335:365], Mod2003[1:59])

# extract Julian dates
ALLmodisDATE <- stri_sub(ALLmodisDATA,
                         1, # von
                         7) # bis



  
  # get seasonal means ################

###### MAM 
# load year
Mod2019 <- list.files("Mosaic/2019/",
                      pattern ='.tif',
                      all.files = T,
                      full.names = T,
                      recursive = T)


#Seasons - change by hand for now

MAM2019 <- Mod2019[60:151]
#JJAS2007 <- Mod2007[152:273]
#ON2007 <- Mod2007[274:334]
#DJF2019 <- c(Mod2019[335:365], Mod2019[1:59])

# Clouds
cloudraster <- raster(Mod2001[1])
cloudraster <- readAll(cloudraster)
cloudraster@data@values <- rep(0,2880000)
#plot(cloudraster)

# Snow
snowraster <- raster(Mod2001[1])
snowraster <- readAll(snowraster)
snowraster@data@values <- rep(0,2880000)

# no data
nodataraster <- raster(Mod2001[1])
nodataraster <- readAll(nodataraster)
nodataraster@data@values <- rep(0,2880000)


# loop for seasonal statistics
i = 1
ST <- Sys.time()
for (i in 1:length(MAM2019)){ # hier j
  
  
  # get data for each cell in each tif
  # call file
  rast <- raster(MAM2019[i])
  rast <- readAll(rast)
  
  # Clouds data
  data1 <- rast@data@values
  
  # set everything below and above 250 to NA
  data1[data1 <= 249] <- 0
  data1[data1 >= 251] <- 0
  
  # set all 250 (cloudy days) to 1
  data1[data1 == 250] <- 1 
  data1[is.na(data1)] <- 0
  
  # Snow data
  data2 <- rast@data@values
  
  # set everything below 0 and above 100 to NA
  data2[data2 <= 0] <- 0
  data2[data2 >= 101] <- 0
  data2[is.na(data2)] <- 0
  
  # NO DATA Count
  data3 <- rast@data@values
  data3[data3 < 200] <- 0
  data3[data3 == 200] <- 1# 
  data3[data3 == 201] <- 1
  data3[data3 == 239] <- 1
  data3[data3 == 254] <- 1
  data3[data3 == 255] <- 1
  data3[data3 == 211] <- 1
  data3[data3 >= 2] <- 0
  data3[is.na(data3)] <- 0
  
  # combine data
  cloudraster@data@values <- cloudraster@data@values + data1
  snowraster@data@values <- snowraster@data@values + data2
  nodataraster@data@values <- nodataraster@data@values + data3
  
  # clean up
  rm(data1,data2,data3,rast)
  gc()
}
ET <- Sys.time()
CT <- ET-ST
rm(ET,ST)
CT

# days
daysraster <- raster(MAM2019[1])
daysraster <- readAll(daysraster)
daysraster@data@values <- rep(i,2880000)

# Export seasonal grid
writeRaster(cloudraster,
            'E:/Seasonal Subsets/MAM2019_clouds.tif', overwrite = T)
writeRaster(snowraster,
            'E:/Seasonal Subsets/MAM2019_snow.tif', overwrite = T)
writeRaster(nodataraster,
            'E:/Seasonal Subsets/MAM2019_nodata.tif', overwrite = T)



# NDSI Mean per year
NDSIraster <- snowraster/(daysraster-nodataraster-cloudraster)

writeRaster(NDSIraster,
            'E:/Seasonal Subsets/MAM2019_NDSI.tif', overwrite = T)
plot(NDSIraster)
plot(cloudraster)
plot(snowraster)
plot(daysraster)
plot(nodataraster)



clouddata <- cloudraster@data@values
max(clouddata)





### JJAS 
# load year
Mod2004 <- list.files("Mosaic/2004/",
                      pattern ='.tif',
                      all.files = T,
                      full.names = T,
                      recursive = T)


# Seasons - change by hand for now


JJAS2019 <- Mod2019[152:273]


# Clouds
cloudraster <- raster(Mod2001[1])
cloudraster <- readAll(cloudraster)
cloudraster@data@values <- rep(0,2880000)
#plot(cloudraster)

# Snow
snowraster <- raster(Mod2001[1])
snowraster <- readAll(snowraster)
snowraster@data@values <- rep(0,2880000)

# no data
nodataraster <- raster(Mod2001[1])
nodataraster <- readAll(nodataraster)
nodataraster@data@values <- rep(0,2880000)


# loop for seasonal statistics
i = 1
ST <- Sys.time()
for (i in 1:length(JJAS2019)){ # hier j
  
  
  # get data for each cell in each tif
  # call file
  rast <- raster(JJAS2019[i])
  rast <- readAll(rast)
  
  # Clouds data
  data1 <- rast@data@values
  
  # set everything below and above 250 to NA
  data1[data1 <= 249] <- 0
  data1[data1 >= 251] <- 0
  
  # set all 250 (cloudy days) to 1
  data1[data1 == 250] <- 1 
  data1[is.na(data1)] <- 0
  
  # Snow data
  data2 <- rast@data@values
  
  # set everything below 0 and above 100 to NA
  data2[data2 <= 0] <- 0
  data2[data2 >= 101] <- 0
  data2[is.na(data2)] <- 0
  
  # NO DATA Count
  data3 <- rast@data@values
  data3[data3 < 200] <- 0
  data3[data3 == 200] <- 1 # 
  data3[data3 == 201] <- 1
  data3[data3 == 254] <- 1
  data3[data3 == 255] <- 1
  data3[data3 == 211] <- 1
  data3[data3 >= 2] <- 0
  data3[is.na(data3)] <- 0
  
  # Daten zusammenführen
  cloudraster@data@values <- cloudraster@data@values + data1
  snowraster@data@values <- snowraster@data@values + data2
  nodataraster@data@values <- nodataraster@data@values + data3
  
  # Aufräumen am Ende jeder Loop
  rm(data1,data2,data3,rast)
  gc()
}
ET <- Sys.time()
CT <- ET-ST
rm(ET,ST)
CT

# days
daysraster <- raster(JJAS2019[1])
daysraster <- readAll(daysraster)
daysraster@data@values <- rep(i,2880000)

# Export Jahresgrid
writeRaster(cloudraster,
            'E:/Seasonal Subsets/JJAS2019_clouds.tif', overwrite = T)
writeRaster(snowraster,
            'E:/Seasonal Subsets/JJAS2019_snow.tif', overwrite = T)
writeRaster(nodataraster,
            'E:/Seasonal Subsets/JJAS2019_nodata.tif', overwrite = T)



# NDSI Mean per year
NDSIraster <- snowraster/(daysraster-nodataraster-cloudraster)

writeRaster(NDSIraster,
            'E:/Seasonal Subsets/JJAS2019_NDSI.tif', overwrite = T)






plot(NDSIraster)
plot(cloudraster)
plot(snowraster)
plot(daysraster)
plot(nodataraster)



clouddata <- cloudraster@data@values
max(clouddata)

### ON 
# load year
Mod2004 <- list.files("Mosaic/2004/",
                      pattern ='.tif',
                      all.files = T,
                      full.names = T,
                      recursive = T)




# Seasons - change by hand for now
#ON2000 <- Mod2000[220:280]

ON2018 <- Mod2018[274:334]


# Clouds
cloudraster <- raster(Mod2018[1])
cloudraster <- readAll(cloudraster)
cloudraster@data@values <- rep(0,2880000)
#plot(cloudraster)

# Snow
snowraster <- raster(Mod2018[1])
snowraster <- readAll(snowraster)
snowraster@data@values <- rep(0,2880000)

# no data
nodataraster <- raster(Mod2018[1])
nodataraster <- readAll(nodataraster)
nodataraster@data@values <- rep(0,2880000)


# loop for seasonal statistics
i = 1
ST <- Sys.time()
for (i in 1:length(ON2018)){ # hier j
  
  
  # get data for each cell in each tif
  # call file
  rast <- raster(ON2018[i])
  rast <- readAll(rast)
  
  # Clouds data
  data1 <- rast@data@values
  
  # set everything below and above 250 to NA
  data1[data1 <= 249] <- 0
  data1[data1 >= 251] <- 0
  
  # set all 250 (cloudy days) to 1
  data1[data1 == 250] <- 1 
  data1[is.na(data1)] <- 0
  
  # Snow data
  data2 <- rast@data@values
  
  # set everything below 0 and above 100 to NA
  data2[data2 <= 0] <- 0
  data2[data2 >= 101] <- 0
  data2[is.na(data2)] <- 0
  
  # NO DATA Count
  data3 <- rast@data@values
  data3[data3 < 200] <- 0
  data3[data3 == 200] <- 1 # 
  data3[data3 == 201] <- 1
  data3[data3 == 254] <- 1
  data3[data3 == 255] <- 1
  data3[data3 == 211] <- 1
  data3[data3 >= 2] <- 0
  data3[is.na(data3)] <- 0
  
  # Daten zusammenführen
  cloudraster@data@values <- cloudraster@data@values + data1
  snowraster@data@values <- snowraster@data@values + data2
  nodataraster@data@values <- nodataraster@data@values + data3
  
  # Aufräumen am Ende jeder Loop
  rm(data1,data2,data3,rast)
  gc()
}
ET <- Sys.time()
CT <- ET-ST
rm(ET,ST)
CT

# days
daysraster <- raster(ON2018[1])
daysraster <- readAll(daysraster)
daysraster@data@values <- rep(i,2880000)

# Export Jahresgrid
writeRaster(cloudraster,
            'E:/Seasonal Subsets/ON2018_clouds.tif', overwrite = T)
writeRaster(snowraster,
            'E:/Seasonal Subsets/ON2018_snow.tif', overwrite = T)
writeRaster(nodataraster,
            'E:/Seasonal Subsets/ON2018_nodata.tif', overwrite = T)



# NDSI Mean per year
NDSIraster <- snowraster/(daysraster-nodataraster-cloudraster)

writeRaster(NDSIraster,
            'E:/Seasonal Subsets/ON2018_NDSI.tif', overwrite = T)






plot(NDSIraster)

plot(cloudraster)
plot(snowraster)
plot(daysraster)
plot(nodataraster)



clouddata <- cloudraster@data@values
max(clouddata)

### DJF 
# Seasons - change by hand for now


#ON2000 <- Mod2000[220:280]
DJF2000 <- c(Mod2000[281:length(Mod2000)], Mod2001[1:59])

# n <- length(Mod2000)

# 2001
# load winter season into global environment
DJF2001 <- c(Mod2001[335:length(Mod2001)], Mod2002[1:59])
DJF2002 <- c(Mod2002[335:length(Mod2002)], Mod2003[1:59])
DJF2003 <- c(Mod2003[335:length(Mod2003)], Mod2004[1:59])
DJF2004 <- c(Mod2004[335:length(Mod2004)], Mod2005[1:59])
DJF2005 <- c(Mod2005[335:length(Mod2005)], Mod2006[1:59])
DJF2006 <- c(Mod2006[335:length(Mod2006)], Mod2007[1:59])
DJF2007 <- c(Mod2007[335:length(Mod2007)], Mod2008[1:59])
DJF2008 <- c(Mod2008[335:length(Mod2008)], Mod2009[1:59])
DJF2009 <- c(Mod2009[335:length(Mod2009)], Mod2010[1:59])
DJF2010 <- c(Mod2010[335:length(Mod2010)], Mod2011[1:59])
DJF2011 <- c(Mod2011[335:length(Mod2011)], Mod2012[1:59])
DJF2012 <- c(Mod2012[335:length(Mod2012)], Mod2013[1:59])
DJF2013 <- c(Mod2013[335:length(Mod2013)], Mod2014[1:59])
DJF2014 <- c(Mod2014[335:length(Mod2014)], Mod2015[1:59])
DJF2015 <- c(Mod2015[335:length(Mod2015)], Mod2016[1:59])
DJF2016 <- c(Mod2016[335:length(Mod2016)], Mod2017[1:59])
DJF2017 <- c(Mod2017[335:length(Mod2017)], Mod2018[1:59])
DJF2018 <- c(Mod2018[335:length(Mod2018)], Mod2019[1:59])

# Clouds
cloudraster <- raster(Mod2018[1])
cloudraster <- readAll(cloudraster)
cloudraster@data@values <- rep(0,2880000)
#plot(cloudraster)

# Snow
snowraster <- raster(Mod2018[1])
snowraster <- readAll(snowraster)
snowraster@data@values <- rep(0,2880000)

# no data
nodataraster <- raster(Mod2018[1])
nodataraster <- readAll(nodataraster)
nodataraster@data@values <- rep(0,2880000)


# loop for seasonal statistics
i = 1
ST <- Sys.time()
for (i in 1:length(DJF2018)){ # hier j
  
  
  # get data for each cell in each tif
  # call file
  rast <- raster(DJF2018[i])
  rast <- readAll(rast)
  
  # Clouds data
  data1 <- rast@data@values
  
  # set everything below and above 250 to NA
  data1[data1 <= 249] <- 0
  data1[data1 >= 251] <- 0
  
  # set all 250 (cloudy days) to 1
  data1[data1 == 250] <- 1 
  data1[is.na(data1)] <- 0
  
  # Snow data
  data2 <- rast@data@values
  
  # set everything below 0 and above 100 to NA
  data2[data2 <= 0] <- 0
  data2[data2 >= 101] <- 0
  data2[is.na(data2)] <- 0
  
  # NO DATA Count
  data3 <- rast@data@values
  data3[data3 < 200] <- 0
  data3[data3 == 200] <- 1 # 
  data3[data3 == 201] <- 1
  data3[data3 == 254] <- 1
  data3[data3 == 255] <- 1
  data3[data3 == 211] <- 1
  data3[data3 >= 2] <- 0
  data3[is.na(data3)] <- 0
  
  # Daten zusammenführen
  cloudraster@data@values <- cloudraster@data@values + data1
  snowraster@data@values <- snowraster@data@values + data2
  nodataraster@data@values <- nodataraster@data@values + data3
  
  # Aufräumen am Ende jeder Loop
  rm(data1,data2,data3,rast)
  gc()
}
ET <- Sys.time()
CT <- ET-ST
rm(ET,ST)
CT

# days
daysraster <- raster(DJF2018[1])
daysraster <- readAll(daysraster)
daysraster@data@values <- rep(i,2880000)

# Export Jahresgrid
writeRaster(cloudraster,
            'E:/Seasonal Subsets/DJF2018_clouds.tif', overwrite = T)
writeRaster(snowraster,
            'E:/Seasonal Subsets/DJF2018_snow.tif', overwrite = T)
writeRaster(nodataraster,
            'E:/Seasonal Subsets/DJF2018_nodata.tif', overwrite = T)



# NDSI Mean per year
NDSIraster <- snowraster/(daysraster-nodataraster-cloudraster)

writeRaster(NDSIraster,
            'E:/Seasonal Subsets/DJF2018_NDSI.tif', overwrite = T)

plot(NDSIraster)

plot(cloudraster)
plot(snowraster)
plot(daysraster)
plot(nodataraster)






#RASTERSTACKS + export 
plot(NDSIraster)

# NDSI Raster Stack
# MAM #
NDSI_MAM <- list.files("Seasonal Subsets/NDSI/MAM/",
                       pattern = ".tif",
                       all.files = T,
                       full.names = T)
NDSI_MAM_stack <- stack(NDSI_MAM)

writeRaster(NDSI_MAM_stack,
            filename = "Seasonal Subsets/NDSI/NDSI_MAM_stack.nc",
            overwrite = T,
            format = "CDF",
            varname = "Seasonal Mean NDSI Snow Cover Fractions",
            varunit = "SCFRAC",
            longname = "NDSI Snow Cover Fractions during the Pre-Monsoon (MAM)",
            xname = "lon",
            yname = "lat",
            zname = "year",
            zunit = "year")
animate(NDSI_MAM_stack)

# JJAS #
NDSI_JJAS <- list.files("Seasonal Subsets/NDSI/JJAS/",
                        pattern = ".tif",
                        all.files = T,
                        full.names = T)
NDSI_JJAS_stack <- stack(NDSI_JJAS)

writeRaster(NDSI_JJAS_stack,
            filename = "Seasonal Subsets/NDSI/NDSI_JJAS_stack.nc",
            overwrite = T,
            format = "CDF",
            varname = "Seasonal Mean NDSI Snow Cover Fractions",
            varunit = "SCFRAC",
            longname = "NDSI Snow Cover Fractions during the Monsoon (JJAS)",
            xname = "lon",
            yname = "lat",
            zname = "year",
            zunit = "year")
animate(NDSI_JJAS_stack)

# ON #
NDSI_ON <- list.files("Seasonal Subsets/NDSI/ON/",
                      pattern = ".tif",
                      all.files = T,
                      full.names = T)
NDSI_ON_stack <- stack(NDSI_ON)

writeRaster(NDSI_ON_stack,
            filename = "Seasonal Subsets/NDSI/NDSI_ON_stack.nc",
            overwrite = T,
            format = "CDF",
            varname = "Seasonal Mean NDSI Snow Cover Fractions",
            varunit = "SCFRAC",
            longname = "NDSI Snow Cover Fractions during the Post-Monsoon (ON)",
            xname = "lon",
            yname = "lat",
            zname = "year",
            zunit = "year")
animate(NDSI_ON_stack)



# DJF #
NDSI_DJF <- list.files("Seasonal Subsets/NDSI/DJF/",
                       pattern = ".tif",
                       all.files = T,
                       full.names = T)
NDSI_DJF_stack <- stack(NDSI_DJF)

writeRaster(NDSI_DJF_stack,
            filename = "Seasonal Subsets/NDSI/NDSI_DJF_stack.nc",
            overwrite = T,
            format = "CDF",
            varname = "Seasonal Mean NDSI Snow Cover Fractions",
            varunit = "SCFRAC",
            longname = "NDSI Snow Cover Fractions during Winter (DJF)",
            xname = "lon",
            yname = "lat",
            zname = "year",
            zunit = "year")
animate(NDSI_DJF_stack)



plot(NDSI_DJF_stack)
plot(NDSI_MAM_stack)
plot(NDSI_ON_stack)
plot(NDSI_JJAS_stack)


# calculate interannual and seasonal variance ####


# clean up
rm(list=ls())


library(dplyr)
library(raster)
library(gdalUtils)
library(gWidgetsRGtk2)
library(rgdal)
library(rgl)
library(ncdf4)

##################### list data 
WorkDir <- "E:/"
setwd(WorkDir)

Season_NDSI <- list.files("./Seasonal Subsets/NDSI/",
                          pattern = ".tif",
                          all.files = T,
                          full.names = T,
                          recursive = T)

# Seasonal Standard Deviation for NDSI ##############
# JJAS #
NDSI_DJF <- list.files("Seasonal Subsets/NDSI/DJF/",
                       pattern = ".tif",
                       all.files = T,
                       full.names = T)
NDSI_DJF_stack <- stack(NDSI_DJF)


DJF.sd <- overlay(NDSI_DJF_stack,  fun = sd, na.rm = TRUE) 
plot(DJF.sd)

# MAM #
NDSI_MAM <- list.files("Seasonal Subsets/NDSI/MAM/",
                       pattern = ".tif",
                       all.files = T,
                       full.names = T)
NDSI_MAM_stack <- stack(NDSI_MAM)


MAM.sd <- overlay(NDSI_MAM_stack,  fun = sd, na.rm = TRUE) 
plot(MAM.sd)

# JJAS #
NDSI_JJAS <- list.files("Seasonal Subsets/NDSI/JJAS/",
                        pattern = ".tif",
                        all.files = T,
                        full.names = T)
NDSI_JJAS_stack <- stack(NDSI_JJAS)


JJAS.sd <- overlay(NDSI_JJAS_stack,  fun = sd, na.rm = TRUE) 
plot(JJAS.sd)

# ON #
NDSI_ON <- list.files("Seasonal Subsets/NDSI/ON/",
                      pattern = ".tif",
                      all.files = T,
                      full.names = T)
NDSI_ON_stack <- stack(NDSI_ON)


ON.sd <- overlay(NDSI_ON_stack,  fun = sd, na.rm = TRUE) 
plot(ON.sd)

# Export Rasters 

writeRaster(DJF.sd,
            'E:/Results/NDSI_DJF_SD.tif', overwrite = T)
writeRaster(MAM.sd,
            'E:/Results/NDSI_MAM_SD.tif', overwrite = T)
writeRaster(JJAS.sd,
            'E:/Results/NDSI_JJAS_SD.tif', overwrite = T)
writeRaster(ON.sd,
            'E:/Results/NDSI_ON_SD.tif', overwrite = T)


# Seasonal Standard Deviation for Cloud Covers ##############



# DJF #
Clouds_DJF <- list.files("Seasonal Subsets/Clouds/DJF/",
                         pattern = ".tif",
                         all.files = T,
                         full.names = T)
Clouds_DJF_stack <- stack(Clouds_DJF)


Clouds_DJF.sd <- overlay(Clouds_DJF_stack,  fun = sd, na.rm = TRUE) 
plot(Clouds_DJF.sd)

# MAM #
Clouds_MAM <- list.files("Seasonal Subsets/Clouds/MAM/",
                         pattern = ".tif",
                         all.files = T,
                         full.names = T)
Clouds_MAM_stack <- stack(Clouds_MAM)


Clouds_MAM.sd <- overlay(Clouds_MAM_stack,  fun = sd, na.rm = TRUE) 
plot(Clouds_MAM.sd)

# JJAS #
Clouds_JJAS <- list.files("Seasonal Subsets/Clouds/JJAS/",
                          pattern = ".tif",
                          all.files = T,
                          full.names = T)
Clouds_JJAS_stack <- stack(Clouds_JJAS)


Clouds_JJAS.sd <- overlay(Clouds_JJAS_stack,  fun = sd, na.rm = TRUE) 
plot(Clouds_JJAS.sd )

# ON #
Clouds_ON <- list.files("Seasonal Subsets/Clouds/ON/",
                        pattern = ".tif",
                        all.files = T,
                        full.names = T)
Clouds_ON_stack <- stack(Clouds_ON)


Clouds_ON.sd <- overlay(Clouds_ON_stack,  fun = sd, na.rm = TRUE) 
plot(Clouds_ON.sd)

# Export Rasters 

writeRaster(Clouds_DJF.sd,
            'E:/Results/Clouds_DJF_SD.tif', overwrite = T)
writeRaster(Clouds_MAM.sd ,
            'E:/Results/Clouds_MAM_SD.tif', overwrite = T)
writeRaster(Clouds_JJAS.sd,
            'E:/Results/Clouds_JJAS_SD.tif', overwrite = T)
writeRaster(Clouds_ON.sd,
            'E:/Results/Clouds_ON_SD.tif', overwrite = T)





animate(NDSI_DJF_stack)
animate(NDSI_MAM_stack)
animate(NDSI_JJAS_stack)
animate(NDSI_ON_stack)



##### Trends ##### 


## Parsing out the date. My file names are
## like this: 1993154.tif which is the year and Julian day



Season_NDSI <- list.files("./Seasonal Subsets/NDSI/",
                          pattern = ".tif",
                          all.files = T,
                          full.names = T,
                          recursive = T)

WorkDir <- "E:/Seasonal Subsets/NDSI/"
setwd(WorkDir)

NDSI_DJF <- list.files("./DJF/",
                       pattern = ".tif",
                       all.files = T,
                       full.names = T,
                       recursive = T)

NDSI_MAM <- list.files("./MAM/",
                       pattern = ".tif",
                       all.files = T,
                       full.names = T)

NDSI_JJAS <- list.files("./JJAS/",
                        pattern = ".tif",
                        all.files = T,
                        full.names = T)

NDSI_ON <- list.files("./ON/",
                      pattern = ".tif",
                      all.files = T,
                      full.names = T)


Date_NDSI_DJF <- stri_sub(NDSI_DJF,
                          10,
                          13)
Date_NDSI_MAM <- stri_sub(NDSI_MAM,
                          10,
                          13)
Date_NDSI_JJAS <- stri_sub(NDSI_JJAS,
                           12,
                           15)
Date_NDSI_ON <- stri_sub(NDSI_ON,
                         8,
                         11)




# trend analysis for elevation zones

# clean up
rm(list=ls())


##################### list data #############
WorkDir <- "E:/"
setwd(WorkDir)
library(raster)
library(stringi)

# entire region
DEM <- raster("./GIS/DEM_New/DEM_.sdat")
DEM <- readAll(DEM)
plot(DEM)

# Nepal
Nepal <- getData("GADM", country = "Nepal", level = 0)
plot(Nepal)
str(Nepal)

DEM_Nepal <- crop(DEM, Nepal)
DEM_Nepal <- mask(DEM, Nepal)
DEM_Nepal <- crop(DEM_Nepal, Nepal)
plot(DEM_Nepal)

slas <- slopeAspect(DEM_Nepal)
plot(slas)
hill <- hillShade(slas[[1]], slas[[2]], 40, 70)
plot(hill, col=grey(0:100/100), legend = F)




# rolwaling
#DEM_ROL <- raster("./Results/Rolwaling/Rolwaling_DEM_.tif")
#plot(DEM_ROL)




library(raster)
library(rasterVis)
library(maptools) ## for readShapeLines
library(colorspace) ## for terrain_hcl
library(rasterVis)

terrainTheme <- modifyList(rasterTheme(region=terrain_hcl(n=15)),
                           list(panel.background=list(col='white')))
hsTheme <- modifyList(GrTheme(),                   
                      list(regions=list(alpha=0.4)))

#lp <- levelplot(DEM_Nepal, par.settings= terrainTheme, margin =F, colorkey = T)
lp <- levelplot(DEM_Nepal, par.settings=terrainTheme, margin=FALSE, colorkey=T, maxpixels = 2e5) +
  levelplot(hill, par.settings=hsTheme, maxpixels = 2e7)


lp







Dec1_pl <- levelplot(Dec1_Means, par.settings = mapTheme, at=my.at) # this makes a pretty plot
Dec1_pl + layer(sp.lines(Nepal, lwd =0.8, col="darkgray")) 







# elevation zones

elabove3k <- DEM
elabove3k@data@values <- rep(0,2550170)
plot(elabove3k)
data <- DEM@data@values 
data[data <=2999] <- NA
data[is.na(data)] <- NA
elabove3k@data@values <- elabove3k@data@values + data
plot(elabove3k)
plot(Nepal, add=T)

el3k4k <- DEM
el3k4k@data@values <- rep(0,2550170)
plot(el3k4k)
data <- DEM@data@values 
data[data <=2999] <- NA
data[data >= 4000] <- NA
data[is.na(data)] <- NA
el3k4k@data@values <- el3k4k@data@values + data
plot(el3k4k)
plot(Nepal, add=T)


el4k5k <- DEM
el4k5k@data@values <- rep(0,2550170)
plot(el4k5k)
data <- DEM@data@values 
data[data <=3999] <- NA
data[data >= 5000] <- NA
data[is.na(data)] <- NA
el4k5k@data@values <- el4k5k@data@values + data
plot(el4k5k)
plot(Nepal, add=T)


el5k6k <- DEM
el5k6k@data@values <- rep(0,2550170)
plot(el5k6k)
data <- DEM@data@values 
data[data <=4999] <- NA
data[data >= 6000] <- NA
data[is.na(data)] <- NA
el5k6k@data@values <- el5k6k@data@values + data
plot(el5k6k)
plot(Nepal, add=T)

el6k <- DEM
el6k@data@values <- rep(0,2550170)
plot(el6k)
data <- DEM@data@values 
data[data <=5999] <- NA
data[is.na(data)] <- NA
el6k@data@values <- el6k@data@values + data
plot(el6k)
plot(Nepal, add=T)

plot(DEM)
plot(Nepal, add=T)




# elevation zones rolwaling
DEM_ROL <- readAll(DEM_ROL)

elabove3k <- DEM_ROL
elabove3k@data@values <- rep(0,15246)
plot(elabove3k)
data <- DEM_ROL@data@values 
data[data <=2999] <- NA
data[is.na(data)] <- NA
elabove3k@data@values <- elabove3k@data@values + data
plot(elabove3k)


el3k4k <- DEM_ROL
el3k4k@data@values <- rep(0,15.246)
plot(el3k4k)
data <- DEM_ROL@data@values 
data[data <=2999] <- NA
data[data >= 4000] <- NA
data[is.na(data)] <- NA
el3k4k@data@values <- el3k4k@data@values + data
plot(el3k4k)


el4k5k <- DEM_ROL
el4k5k@data@values <- rep(0,15.246)
plot(el4k5k)
data <- DEM_ROL@data@values 
data[data <=3999] <- NA
data[data >= 5000] <- NA
data[is.na(data)] <- NA
el4k5k@data@values <- el4k5k@data@values + data
plot(el4k5k)

el5k6k <- DEM_ROL
el5k6k@data@values <- rep(0,15.246)
plot(el5k6k)
data <- DEM_ROL@data@values 
data[data <=4999] <- NA
data[data >= 6000] <- NA
data[is.na(data)] <- NA
el5k6k@data@values <- el5k6k@data@values + data
plot(el5k6k)


el6k <- DEM_ROL
el6k@data@values <- rep(0,15.246)
plot(el6k)
data <- DEM_ROL@data@values 
data[data <=5999] <- NA
data[is.na(data)] <- NA
el6k@data@values <- el6k@data@values + data
plot(el6k)

# load mean NDSI files


# annual
NDSI_Annual <- list.files("./Results/NDSI_threshold/Annual/",
                          pattern = ".tif", 
                          all.files = T, 
                          full.names = T)




# seasons
#2000 - 2018
NDSI_DJF <- list.files("./Seasonal Subsets/NDSI/DJF/", pattern = ".tif", all.files = T, full.names = T)

#2000 - 2019
NDSI_MAM <- list.files("./Seasonal Subsets/NDSI/MAM/", pattern = ".tif", all.files = T, full.names = T)

# 2000 - 2019
NDSI_JJAS <- list.files("./Seasonal Subsets/NDSI/JJAS/", pattern = ".tif", all.files = T, full.names = T)

# 2000 - 2018
NDSI_ON <- list.files("./Seasonal Subsets/NDSI/ON/", pattern = ".tif", all.files = T, full.names = T)

# mask values below 0.4 NDSI

#plot(r_mask)


for (i in length(NDSI_ON)) {
  
  ST <- Sys.time()  
  r_mask <- raster(NDSI_ON[1])
  r_mask <- readAll(r_mask)
  r_mask@data@values <- rep(0,2880000)
  
  r <- raster(NDSI_ON[19])
  r <- readAll(r)
  data <- r@data@values
  data[data <=40] <- NA
  r_mask@data@values <- r_mask@data@values + data
  plot(r_mask)
  rm(data)
  gc()
  ET <- Sys.time()
  CT <- ET-ST
  rm(ET,ST)
  CT
  writeRaster(r_mask,
              "./Results/NDSI_threshold/Seasons/ON/NDSI2018.tif", overwrite = T)
}


NDSI_Annual <- list.files("./Results/NDSI_threshold/Annual/", pattern = ".tif", all.files = T, full.names = T)
NDSI_Annual <- stack(NDSI_Annual)
extent(NDSI_Annual) <- extent(DEM)
NDSI_DJF <- list.files("./Results/NDSI_threshold/Seasons/DJF/", pattern = ".tif", all.files = T, full.names = T)
NDSI_DJF <- stack(NDSI_DJF)
extent(NDSI_DJF) <- extent(DEM)
NDSI_MAM <- list.files("./Results/NDSI_threshold/Seasons/MAM/", pattern = ".tif", all.files = T, full.names = T)
NDSI_MAM <- stack(NDSI_MAM)
extent(NDSI_MAM) <- extent(DEM)
NDSI_ON <- list.files("./Results/NDSI_threshold/Seasons/ON/", pattern = ".tif", all.files = T, full.names = T)
NDSI_ON <- stack(NDSI_ON)
extent(NDSI_ON) <- extent(DEM)

extent <- extent(86.07052, 86.71218, 27.68974, 28.10224)
NDSI_Annual <- crop(NDSI_Annual, extent)
extent(NDSI_Annual) <- extent
NDSI_DJF <- crop(NDSI_DJF, extent)
extent(NDSI_DJF) <- extent
NDSI_MAM <- crop(NDSI_MAM, extent)
extent(NDSI_MAM) <- extent
NDSI_ON <- crop(NDSI_ON, elabove3k)
extent(NDSI_ON) <- extent

#el3k4k <- crop(el3k4k, DEM_ROL)
#extent(el3k4k) <- extent(trend_sig_DJF)


# the following is adapted from https://matinbrandt.wordpress.com/2013/11/15/pixel-wise-time-series-trend-anaylsis-with-ndvi-gimms-and-r/

# slope, p-value and confidence level for Annual NDSI ####
# calculate slope to get magnitude and direction of trends
# ON slope p value confidence level
time <- 1:nlayers(NDSI_ON) # identify time slices, i.e. years 
fun=function(x) { if (is.na(x[1])){ NA } 
  else 
  { m = lm(x ~ time); summary(m)$coefficients[2] }} # calc linear regression / coefficient[2] <- slope
NDSI_ON.slope=calc(NDSI_ON, fun) # applay function to stack
NDSI_ON.slope=NDSI_ON.slope*19 # multiply slope by number of years
plot(NDSI_ON.slope, main = "NDSI ON SLOPE")


# calc p-value
# extract p-value to see which trends are significant
fun=function(x) { if (is.na(x[1])){ NA } else { m = lm(x ~ time); summary(m)$coefficients[8] }} #coefficient[8] <- significance
p_ON <- calc(NDSI_ON, fun=fun)
plot(p_ON, main="p-Value")

# calc R-squared
fun=function(x) { if (is.na(x[1])){ NA } else { m = lm(x ~ time); summary(m)$r.squared }} #get r_squared for each cell <- 
rs_ON <- calc(NDSI_ON, fun=fun)
plot(rs_ON, main="R-squared")


# calc intercept
#fun=function(x) { if (is.na(x[1])){ NA } else { m = lm(x ~ time); summary(m)$coefficients[1] }} #get r_squared for each cell <- 
#int_ON <- calc(NDSI_ON, fun=fun)
#plot(int_ON, main="Intercept")

# get confidence level of 95 percent

m = c(0, 0.05, 1, 0.05, 1, 0) # intervals
rclmat = matrix(m, ncol=3, byrow=TRUE)

# mask all insignificant trends and only get NDSI changes which are significant with a 95% level 
p.mask_ON = reclassify(p_ON, rclmat)
fun=function(x) { x[x<1] <- NA; return(x)}
p.mask.NA_ON = calc(p.mask_ON, fun)


trend_sig_ON <- mask(NDSI_ON.slope, p.mask.NA_ON)
plot(trend_sig_ON, main = "Significant ON NDSI Changes (2001-2018)")

trend_sig_ON_above3k <- mask(trend_sig_ON, elabove3k)
plot(trend_sig_ON_above3k, main = "Significant ON NDSI Changes (2001-2018), elevation zone above 3000m")
#plot(DEM)
#plot(trend_sig_ON, add=T)

trend_sig_ON_3k4k <- mask(trend_sig_ON, el3k4k)
plot(trend_sig_ON_3k4k, main = "Significant ON NDSI Changes (2001-2018), elevation zone 3000 - 4000m")

trend_sig_ON_4k5k <- mask(trend_sig_ON, el4k5k)
plot(trend_sig_ON_3k4k, main = "Significant ON NDSI Changes (2001-2018), elevation zone 4000 - 5000m")

trend_sig_ON_5k6k <- mask(trend_sig_ON, el5k6k)
plot(trend_sig_ON_3k4k, main = "Significant ON NDSI Changes (2001-2018), elevation zone 5000 - 6000m")

trend_sig_ON_6k7k <- mask(trend_sig_ON, el6k)
plot(trend_sig_ON_3k4k, main = "Significant ON NDSI Changes (2001-2018), elevation zone above 6000m")

summary(trend_sig_ON)
hist(trend_sig_ON,
     main = "Significant Changes in Mean ON NDSI (>0.4)")



# export rasters
writeRaster(trend_sig_ON,
            "E:/Results/NDSI_threshold/Rolwaling/ON/NDSI_ON_Trend.tif",
            overwrite = T)

writeRaster(trend_sig_ON_3k4k,
            "E:/Results/NDSI_threshold/Rolwaling/ON/NDSI_ON_Trend_3k4k.tif",
            overwrite = T)
writeRaster(trend_sig_ON_4k5k,
            "E:/Results/NDSI_threshold/Rolwaling/ON/NDSI_ON_Trend_4k5k.tif",
            overwrite = T)
writeRaster(trend_sig_ON_5k6k,
            "E:/Results/NDSI_threshold/Rolwaling/ON/NDSI_ON_Trend_5k6k.tif",
            overwrite = T)
writeRaster(trend_sig_ON_6k7k,
            "E:/Results/NDSI_threshold/Rolwaling/ON/NDSI_ON_Trend_6k.tif",
            overwrite = T)

writeRaster(NDSI_ON.slope,
            "E:/Results/NDSI_threshold/Rolwaling/ON/NDSI_ON_Slope.tif",
            overwrite = T)

writeRaster(rs_ON,
            "E:/Results/NDSI_threshold/Rolwaling/ON/NDSI_ON_Rsquared.tif",
            overwrite = T)


plot(trend_sig_Annual_above3k)
############ MAM###############





# the following is adapted from https://matinbrandt.wordpress.com/2013/11/15/pixel-wise-time-series-trend-anaylsis-with-ndvi-gimms-and-r/

# slope, p-value and confidence level for MAM NDSI ####
# calculate slope to get magnitude and direction of trends
# annual slope p value confidence level
time <- 1:nlayers(NDSI_MAM) # identify time slices, i.e. years 
fun=function(x) { if (is.na(x[1])){ NA } 
  else 
  { m = lm(x ~ time); summary(m)$coefficients[2] }} # calc linear regression / coefficient[2] <- slope
NDSI_MAM.slope=calc(NDSI_MAM, fun) # applay function to stack
NDSI_MAM.slope=NDSI_MAM.slope*19 # multiply slope by number of years
plot(NDSI_MAM.slope, main = "NDSI MAM SLOPE")


# calc p-value
# extract p-value to see which trends are significant
fun=function(x) { if (is.na(x[1])){ NA } else { m = lm(x ~ time); summary(m)$coefficients[8] }} #coefficient[8] <- significance
p_MAM <- calc(NDSI_MAM, fun=fun)
plot(p_MAM, main="p-Value")

# calc R-squared
fun=function(x) { if (is.na(x[1])){ NA } else { m = lm(x ~ time); summary(m)$r.squared }} #get r_squared for each cell <- 
rs_MAM <- calc(NDSI_MAM, fun=fun)
plot(rs_MAM, main="R-squared")


# calc intercept
#fun=function(x) { if (is.na(x[1])){ NA } else { m = lm(x ~ time); summary(m)$coefficients[1] }} #get r_squared for each cell <- 
#int_MAM <- calc(NDSI_MAM, fun=fun)
#plot(int_MAM, main="Intercept")

# get confidence level of 95 percent

m = c(0, 0.05, 1, 0.05, 1, 0) # intervals
rclmat = matrix(m, ncol=3, byrow=TRUE)

# mask all insignificant trends and only get NDSI changes which are significant with a 95% level 
p.mask_MAM = reclassify(p_MAM, rclmat)
fun=function(x) { x[x<1] <- NA; return(x)}
p.mask.NA_MAM = calc(p.mask_MAM, fun)

trend_sig_MAM <- mask(NDSI_MAM.slope, p.mask.NA_MAM)
plot(trend_sig_MAM, main = "Significant MAM NDSI Changes (2001-2018)")

trend_sig_MAM_above3k <- mask(trend_sig_MAM, elabove3k)
plot(trend_sig_MAM_above3k, main = "Significant MAM NDSI Changes (2001-2018), elevation zone above 3000m")
plot(DEM)
plot(trend_sig_MAM, add=T)

trend_sig_MAM_3k4k <- mask(trend_sig_MAM, el3k4k)
plot(trend_sig_MAM_3k4k, main = "Significant MAM NDSI Changes (2001-2018), elevation zone 3000 - 4000m")

trend_sig_MAM_4k5k <- mask(trend_sig_MAM, el4k5k)
plot(trend_sig_MAM_4k5k, main = "Significant MAM NDSI Changes (2001-2018), elevation zone 4000 - 5000m")

trend_sig_MAM_5k6k <- mask(trend_sig_MAM, el5k6k)
plot(trend_sig_MAM_5k6k, main = "Significant MAM NDSI Changes (2001-2018), elevation zone 5000 - 6000m")

trend_sig_MAM_6k7k <- mask(trend_sig_MAM, el6k)
plot(trend_sig_MAM_6k7k, main = "Significant MAM NDSI Changes (2001-2018), elevation zone above 6000m")

summary(trend_sig_MAM)
hist(trend_sig_MAM,
     main = "Significant Changes in Mean MAM NDSI (>0.4)")


# export rasters
writeRaster(trend_sig_MAM,
            "E:/Results/NDSI_threshold/Seasons/MAM/NDSI_MAM_Trend.tif",
            overwrite = T)

writeRaster(trend_sig_MAM_3k4k,
            "E:/Results/NDSI_threshold/Seasons/MAM/NDSI_MAM_Trend_3k4k.tif",
            overwrite = T)
writeRaster(trend_sig_MAM_4k5k,
            "E:/Results/NDSI_threshold/Seasons/MAM/NDSI_MAM_Trend_4k5k.tif",
            overwrite = T)
writeRaster(trend_sig_MAM_5k6k,
            "E:/Results/NDSI_threshold/Seasons/MAM/NDSI_MAM_Trend_5k6k.tif",
            overwrite = T)
writeRaster(trend_sig_MAM_6k7k,
            "E:/Results/NDSI_threshold/Seasons/MAM/NDSI_MAM_Trend_6k.tif",
            overwrite = T)

writeRaster(NDSI_MAM.slope,
            "E:/Results/NDSI_threshold/Seasons/MAM/NDSI_MAM_Slope.tif",
            overwrite = T)

writeRaster(rs_MAM,
            "E:/Results/NDSI_threshold/Seasons/MAM/NDSI_MAM_Rsquared.tif",
            overwrite = T)

writeRaster(int_MAM,
            "E:/Results/NDSI_threshold/Seasons/MAM/NDSI_MAM_Intercept.tif",
            overwrite = T)



############ ON###############





# the following is adapted from https://matinbrandt.wordpress.com/2013/11/15/pixel-wise-time-series-trend-anaylsis-with-ndvi-gimms-and-r/

# slope, p-value and confidence level for ON NDSI ####
# calculate slope to get magnitude and direction of trends
# annual slope p value confidence level
time <- 1:nlayers(NDSI_ON) # identify time slices, i.e. years 
fun=function(x) { if (is.na(x[1])){ NA } 
  else 
  { m = lm(x ~ time); summary(m)$coefficients[2] }} # calc linear regression / coefficient[2] <- slope
NDSI_ON.slope=calc(NDSI_ON, fun) # applay function to stack
NDSI_ON.slope=NDSI_ON.slope*19 # multiply slope by number of years
plot(NDSI_ON.slope, main = "NDSI ON SLOPE")


# calc p-value
# extract p-value to see which trends are significant
fun=function(x) { if (is.na(x[1])){ NA } else { m = lm(x ~ time); summary(m)$coefficients[8] }} #coefficient[8] <- significance
p_ON <- calc(NDSI_ON, fun=fun)
plot(p_ON, main="p-Value")

# calc R-squared
fun=function(x) { if (is.na(x[1])){ NA } else { m = lm(x ~ time); summary(m)$r.squared }} #get r_squared for each cell <- 
rs_ON <- calc(NDSI_ON, fun=fun)
plot(rs_ON, main="R-squared")


# calc intercept
#fun=function(x) { if (is.na(x[1])){ NA } else { m = lm(x ~ time); summary(m)$coefficients[1] }} #get r_squared for each cell <- 
#int_ON <- calc(NDSI_ON, fun=fun)
#plot(int_ON, main="Intercept")

# get confidence level of 95 percent

m = c(0, 0.05, 1, 0.05, 1, 0) # intervals
rclmat = matrix(m, ncol=3, byrow=TRUE)

# mask all insignificant trends and only get NDSI changes which are significant with a 95% level 
p.mask_ON = reclassify(p_ON, rclmat)
fun=function(x) { x[x<1] <- NA; return(x)}
p.mask.NA_ON = calc(p.mask_ON, fun)

trend_sig_ON <- mask(NDSI_ON.slope, p.mask.NA_ON)
plot(trend_sig_ON, main = "Significant ON NDSI Changes (2001-2018)")

trend_sig_ON_above3k <- mask(trend_sig_ON, elabove3k)
plot(trend_sig_ON_above3k, main = "Significant ON NDSI Changes (2001-2018), elevation zone above 3000m")
#plot(DEM)
#plot(trend_sig_ON, add=T)

trend_sig_ON_3k4k <- mask(trend_sig_ON, el3k4k)
plot(trend_sig_ON_3k4k, main = "Significant ON NDSI Changes (2001-2018), elevation zone 3000 - 4000m")

trend_sig_ON_4k5k <- mask(trend_sig_ON, el4k5k)
plot(trend_sig_ON_4k5k, main = "Significant ON NDSI Changes (2001-2018), elevation zone 4000 - 5000m")

trend_sig_ON_5k6k <- mask(trend_sig_ON, el5k6k)
plot(trend_sig_ON_5k6k, main = "Significant ON NDSI Changes (2001-2018), elevation zone 5000 - 6000m")

trend_sig_ON_6k7k <- mask(trend_sig_ON, el6k)
plot(trend_sig_ON_6k7k, main = "Significant ON NDSI Changes (2001-2018), elevation zone above 6000m")

summary(trend_sig_ON)
hist(trend_sig_ON,
     main = "Significant Changes in Mean ON NDSI (>0.4)")

# export rasters
writeRaster(trend_sig_ON,
            "E:/Results/NDSI_threshold/Seasons/ON/NDSI_ON_Trend.tif",
            overwrite = T)

writeRaster(trend_sig_ON_3k4k,
            "E:/Results/NDSI_threshold/Seasons/ON/NDSI_ON_Trend_3k4k.tif",
            overwrite = T)
writeRaster(trend_sig_ON_4k5k,
            "E:/Results/NDSI_threshold/Seasons/ON/NDSI_ON_Trend_4k5k.tif",
            overwrite = T)
writeRaster(trend_sig_ON_5k6k,
            "E:/Results/NDSI_threshold/Seasons/ON/NDSI_ON_Trend_5k6k.tif",
            overwrite = T)
writeRaster(trend_sig_ON_6k7k,
            "E:/Results/NDSI_threshold/Seasons/ON/NDSI_ON_Trend_6k.tif",
            overwrite = T)

writeRaster(NDSI_ON.slope,
            "E:/Results/NDSI_threshold/Seasons/ON/NDSI_ON_Slope.tif",
            overwrite = T)

writeRaster(rs_ON,
            "E:/Results/NDSI_threshold/Seasons/ON/NDSI_ON_Rsquared.tif",
            overwrite = T)

writeRaster(int_ON,
            "E:/Results/NDSI_threshold/Seasons/ON/NDSI_ON_Intercept.tif",
            overwrite = T)



# export annual means and sums as netcdfs #############


# stack annual statistics
rm(list = ls())

library(raster)
library(ncdf4)




# load dataset for each year
WorkDir <- ("E:/Results/")
setwd(WorkDir)

# NDSI Raster Stack
NDSIannual <- list.files("NDSIannual/",
                         pattern = ".tif",
                         all.files = T,
                         full.names = T)


NDSIannualstack <- stack(NDSIannual)
NDSI2000 <- raster(NDSIannual[1])
plot(NDSI2000)



writeRaster(NDSIannualstack,
            filename = "NDSIannual/NDSIannual_stack.nc",
            overwrite = T,
            format = "CDF",
            varname = "NDSI Snow Cover Fractions",
            varunit = "SCFRAC",
            longname = "Annual NDSI Snow Cover Fractions - missing observation excluded",
            xname = "lat",
            yname = "lon",
            zname = "year",
            zunit = "year")


# Clouds Raster Stack
Cloudsannual <- list.files("CloudCover/",
                           pattern = ".tif",
                           all.files = T,
                           full.names = T)

Cloudsannualstack <- stack(Cloudsannual)

writeRaster(Cloudsannualstack,
            filename = "CloudCover/Cloudsannual_stack.nc",
            overwrite = T,
            format = "CDF",
            varname = "Cloud Cover",
            varunit = "No of cloudy days",
            longname = "number of cloudy days per cell over time",
            xname = "lat",
            yname = "lon",
            zname = "year",
            zunit = "year")



# Missing Data Raster Stack
NoDataannual <- list.files("NoDataCover/",
                           pattern = ".tif",
                           all.files = T,
                           full.names = T)

NoDataannualstack <- stack(NoDataannual)

writeRaster(NoDataannualstack,
            filename = "NoDataCover/NoDataannual_stack.nc",
            overwrite = T,
            format = "CDF",
            varname = "No Data Mask",
            varunit = "No of missing observations",
            longname = "number of missing observations per cell over time",
            xname = "lat",
            yname = "lon",
            zname = "year",
            zunit = "year")





####### get descriptive statistics for temporal subsets #####
# annual and seasonal variability


# this script is for making plots of mean ndsi (annual and seasonal)
# all plots are masked by a shapefile for nepal above elevation 3000m

# clean up
rm(list=ls())


##################### list data #############
WorkDir <- "E:/"
setwd(WorkDir)
library(raster)
library(stringi)
library(rasterVis)


# entire region
DEM <- raster("./GIS/DEM_New/DEM_.sdat")
DEM <- readAll(DEM)
plot(DEM)

# Nepal
Nepal <- getData("GADM", country = "Nepal", level = 0)
plot(Nepal)
str(Nepal)

#####

# this bit cuts off the parts of the DEM that are below and above desired zone, only needs to be executed once
elabove3k <- DEM
elabove3k@data@values <- rep(0,2880000)
plot(elabove3k)
data <- DEM@data@values 
data[data <=2999] <- NA
data[is.na(data)] <- NA
elabove3k@data@values <- elabove3k@data@values + data
elabove3k <- crop(elabove3k, Nepal)
elabove3k <- mask(elabove3k, Nepal)
plot(elabove3k)
plot(Nepal, add=T)


el3k4k <- DEM
el3k4k@data@values <- rep(0,2880000)
plot(el3k4k)
data <- DEM@data@values 
data[data <=2999] <- NA
data[data >= 4000] <- NA
data[is.na(data)] <- NA
el3k4k@data@values <- el3k4k@data@values + data
plot(el3k4k)
plot(Nepal, add=T)


el4k5k <- DEM
el4k5k@data@values <- rep(0,2880000)
plot(el4k5k)
data <- DEM@data@values 
data[data <=3999] <- NA
data[data >= 5000] <- NA
data[is.na(data)] <- NA
el4k5k@data@values <- el4k5k@data@values + data
plot(el4k5k)
plot(Nepal, add=T)


el5k6k <- DEM
el5k6k@data@values <- rep(0,2880000)
plot(el5k6k)
data <- DEM@data@values 
data[data <=4999] <- NA
data[data >= 6000] <- NA
data[is.na(data)] <- NA
el5k6k@data@values <- el5k6k@data@values + data
plot(el5k6k)
plot(Nepal, add=T)

el6k <- DEM
el6k@data@values <- rep(0,2880000)
plot(el6k)
data <- DEM@data@values 
data[data <=5999] <- NA
data[is.na(data)] <- NA
el6k@data@values <- el6k@data@values + data
plot(el6k)
plot(Nepal, add=T)

plot(DEM)
plot(Nepal, add=T)

#writeRaster(el3k4k,"./elevation zones/el3k4k.tif", overwrite=T)
#writeRaster(el4k5k,"./elevation zones/el4k5k.tif", overwrite=T)
#writeRaster(el5k6k,"./elevation zones/el5k6k.tif", overwrite=T)
#writeRaster(el6k,"./elevation zones/el6k.tif", overwrite=T)





##### note that these were resampled in SAGA #####

elabove3k <- raster("./elevation zones/elabove3k.tif") # elevation zone above 3k
el3k4k <- raster("./elevation zones/history/el3k4k.tif")
el4k5k <- raster("./elevation zones/history/el4k5k.tif")
el5k6k <- raster("./elevation zones/history/el5k6k.tif")
el6k <- raster("./elevation zones/el6k.tif")




# annual ################

# load mean NDSi files
NDSI_Annual <- list.files("./Results/NDSIannual/", pattern = ".tif", all.files = T, full.names = T)
Annual_date <- stri_sub(NDSI_Annual, 26,29) # extract years
Dec1 <- NDSI_Annual[1:9] # make smaller sets so that one decade can be displayed per plot
Dec2 <- NDSI_Annual[10:18]


#library(rasterVis)

# this bit sets the graphical parameters 
my.at <- seq(0,100, by=20)
mapTheme <- rasterTheme(region=brewer.pal(9,"Blues"))
mapTheme$panel.background$col='lightgrey'

Dec1_date <- stri_sub(Dec1, 26,29) # extract dates of decade
Dec1 <- stack(Dec1) # make rasterstack
Dec1 <- crop(Dec1, Nepal) # crop by shapefile Nepal
Dec1 <- mask(Dec1, Nepal) # mask by shapefile nepal
Dec1 <- mask(Dec1, elabove3k) # mask everything below 3000 m
Dec1_Means <- setZ(Dec1, Dec1_date) # this way R know which years to plot
Dec1_pl <- levelplot(Dec1_Means, par.settings = mapTheme, at=my.at) # this makes a pretty plot
Dec1_pl + layer(sp.lines(Nepal, lwd =0.8, col="darkgray")) # and this add the country outline


Dec2_date <- stri_sub(Dec2, 26,29)
Dec2 <- stack(Dec2)
Dec2 <- crop(Dec2, Nepal)
Dec2 <- mask(Dec2, Nepal)
Dec2 <- mask(Dec2, elabove3k)
Dec2_Means <- setZ(Dec2, Dec2_date)
Dec2_pl <- levelplot(Dec2_Means, par.settings = mapTheme, at=my.at)
Dec2_pl + layer(sp.lines(Nepal, lwd =0.8, col="darkgray"))


Annual_Means <- setZ(NDSI_Annual, Annual_date) # set Z coordinate to display years
Annual_pl <- levelplot(Annual_Means, par.settings = mapTheme, at=my.at) # set maptheme to blue
Annual_pl + layer(sp.lines(Nepal, lwd =0.8, col="darkgray")) # add outline


NDSI_Annual <- stack(NDSI_Annual)
extent(elabove3k) <- extent(NDSI_Annual)
NDSI_Annual <- crop(NDSI_Annual, Nepal)
NDSI_Annual <- mask(NDSI_Annual, Nepal)
extent(elabove3k) <- extent(NDSI_Annual)
NDSI_Annual <- mask(NDSI_Annual, elabove3k)

my.at <- seq(0,15, by=1)
mapTheme <- rasterTheme(region=brewer.pal(9,"Blues"))
mapTheme$panel.background$col='lightgrey'

Annual.sd <- overlay(NDSI_Annual,  fun = sd, na.rm = TRUE) # calculates standard deviation of mean annual NDSI 
#plot(Annual.sd)
Annual.sd.pl <- levelplot(Annual.sd, par.settings = mapTheme, at=my.at, main = "Standard Deviation of Mean Annual NDSI (2001 - 2018)") # set maptheme to blue
Annual.sd.pl + layer(sp.lines(Nepal, lwd =0.8, col="darkgray"))



# winter season ###############

NDSI_DJF <- list.files("./Seasonal Subsets/NDSI/DJF/", pattern = ".tif", all.files = T, full.names = T)
Annual_date <- stri_sub(NDSI_DJF, 32,35) # extract years
Dec1 <- NDSI_DJF[1:9] # make smaller sets so that one decade can be displayed per plot
Dec2 <- NDSI_DJF[10:19]

#library(rasterVis)

# this bit sets the graphical parameters 
my.at <- seq(0,100, by=20)
mapTheme <- rasterTheme(region=brewer.pal(9,"Blues"))
mapTheme$panel.background$col='lightgrey'

Dec1_date <- stri_sub(Dec1, 32,35) # extract dates of decade
Dec1 <- stack(Dec1) # make rasterstack
Dec1 <- crop(Dec1, Nepal) # crop by shapefile Nepal
Dec1 <- mask(Dec1, Nepal) # mask by shapefile nepal
Dec1 <- mask(Dec1, elabove3k) # mask everything below 3000 m
Dec1_Means <- setZ(Dec1, Dec1_date) # this way R know which years to plot
Dec1_pl <- levelplot(Dec1_Means, par.settings = mapTheme, at=my.at) # this makes a pretty plot
Dec1_pl + layer(sp.lines(Nepal, lwd =0.8, col="darkgray")) # and this add the country outline


Dec2_date <- stri_sub(Dec2, 32,35)
Dec2 <- stack(Dec2)
Dec2 <- crop(Dec2, Nepal)
Dec2 <- mask(Dec2, Nepal)
Dec2 <- mask(Dec2, elabove3k)
Dec2_Means <- setZ(Dec2, Dec2_date)
Dec2_pl <- levelplot(Dec2_Means, par.settings = mapTheme, at=my.at)
Dec2_pl + layer(sp.lines(Nepal, lwd =0.8, col="darkgray"))


Annual_Means <- setZ(NDSI_Annual, Annual_date) # set Z coordinate to display years
Annual_pl <- levelplot(Annual_Means, par.settings = mapTheme, at=my.at) # set maptheme to blue
Annual_pl + layer(sp.lines(Nepal, lwd =0.8, col="darkgray")) # add outline


NDSI_DJF <- stack(NDSI_DJF)
extent(elabove3k) <- extent(NDSI_DJF)
NDSI_DJF <- crop(NDSI_DJF, Nepal)
NDSI_DJF <- mask(NDSI_DJF, Nepal)
extent(elabove3k) <- extent(NDSI_DJF)
NDSI_DJF <- mask(NDSI_DJF, elabove3k)

my.at <- seq(0,30, by=5)
mapTheme <- rasterTheme(region=brewer.pal(9,"Blues"))
mapTheme$panel.background$col='lightgrey'

DJF.sd <- overlay(NDSI_DJF,  fun = sd, na.rm = TRUE) # calculates standard deviation of mean DJF NDSI 
plot(DJF.sd)
DJF.sd.pl <- levelplot(DJF.sd, par.settings = mapTheme, at=my.at, main = "Standard Deviation of Mean DJF NDSI (2001 - 2018)") # set maptheme to blue
DJF.sd.pl + layer(sp.lines(Nepal, lwd =0.8, col="darkgray"))








# pre-monsoon ############
NDSI_MAM <- list.files("./Seasonal Subsets/NDSI/MAM/", pattern = ".tif", all.files = T, full.names = T)

Annual_date <- stri_sub(NDSI_MAM, 32,35) # extract years
Dec1 <- NDSI_MAM[1:10] # make smaller sets so that one decade can be displayed per plot
Dec2 <- NDSI_MAM[11:20]

#library(rasterVis)

# this bit sets the graphical parameters 
my.at <- seq(0,100, by=20)
mapTheme <- rasterTheme(region=brewer.pal(9,"Blues"))
mapTheme$panel.background$col='lightgrey'

Dec1_date <- stri_sub(Dec1, 32,35) # extract dates of decade
Dec1 <- stack(Dec1) # make rasterstack
Dec1 <- crop(Dec1, Nepal) # crop by shapefile Nepal
Dec1 <- mask(Dec1, Nepal) # mask by shapefile nepal
Dec1 <- mask(Dec1, elabove3k) # mask everything below 3000 m
Dec1_Means <- setZ(Dec1, Dec1_date) # this way R know which years to plot
Dec1_pl <- levelplot(Dec1_Means, par.settings = mapTheme, at=my.at) # this makes a pretty plot
Dec1_pl + layer(sp.lines(Nepal, lwd =0.8, col="darkgray")) # and this add the country outline


Dec2_date <- stri_sub(Dec2, 32,35)
Dec2 <- stack(Dec2)
Dec2 <- crop(Dec2, Nepal)
Dec2 <- mask(Dec2, Nepal)
Dec2 <- mask(Dec2, elabove3k)
Dec2_Means <- setZ(Dec2, Dec2_date)
Dec2_pl <- levelplot(Dec2_Means, par.settings = mapTheme, at=my.at)
Dec2_pl + layer(sp.lines(Nepal, lwd =0.8, col="darkgray"))


MAM_Means <- setZ(NDSI_MAM, Annual_date) # set Z coordinate to display years
MAM_pl <- levelplot(MAM_Means, par.settings = mapTheme, at=my.at) # set maptheme to blue
MAM_pl + layer(sp.lines(Nepal, lwd =0.8, col="darkgray")) # add outline


NDSI_MAM <- stack(NDSI_MAM)
extent(elabove3k) <- extent(NDSI_MAM)
NDSI_MAM <- crop(NDSI_MAM, Nepal)
NDSI_MAM <- mask(NDSI_MAM, Nepal)
extent(elabove3k) <- extent(NDSI_MAM)
NDSI_MAM <- mask(NDSI_MAM, elabove3k)

my.at <- seq(0,30, by=5)
mapTheme <- rasterTheme(region=brewer.pal(9,"Blues"))
mapTheme$panel.background$col='lightgrey'

MAM.sd <- overlay(NDSI_MAM,  fun = sd, na.rm = TRUE) # calculates standard deviation of mean MAM NDSI 
plot(MAM.sd)
MAM.sd.pl <- levelplot(MAM.sd, par.settings = mapTheme, at=my.at, main = "Standard Deviation of Mean MAM NDSI (2000 - 2019)") # set maptheme to blue
MAM.sd.pl + layer(sp.lines(Nepal, lwd =0.8, col="darkgray"))




# monsoon ####
NDSI_JJAS <- list.files("./Seasonal Subsets/NDSI/JJAS/", pattern = ".tif", all.files = T, full.names = T)

Annual_date <- stri_sub(NDSI_JJAS, 34,37) # extract years
Dec1 <- NDSI_JJAS[1:10] # make smaller sets so that one decade can be displayed per plot
Dec2 <- NDSI_JJAS[11:20]

#library(rasterVis)

# this bit sets the graphical parameters 
my.at <- seq(0,100, by=20)
mapTheme <- rasterTheme(region=brewer.pal(9,"Blues"))
mapTheme$panel.background$col='lightgrey'

Dec1_date <- stri_sub(Dec1, 34,37) # extract dates of decade
Dec1 <- stack(Dec1) # make rasterstack
Dec1 <- crop(Dec1, Nepal) # crop by shapefile Nepal
Dec1 <- mask(Dec1, Nepal) # mask by shapefile nepal
Dec1 <- mask(Dec1, elabove3k) # mask everything below 3000 m
Dec1_Means <- setZ(Dec1, Dec1_date) # this way R know which years to plot
Dec1_pl <- levelplot(Dec1_Means, par.settings = mapTheme, at=my.at) # this makes a pretty plot
Dec1_pl + layer(sp.lines(Nepal, lwd =0.8, col="darkgray")) # and this add the country outline


Dec2_date <- stri_sub(Dec2,34,37)
Dec2 <- stack(Dec2)
Dec2 <- crop(Dec2, Nepal)
Dec2 <- mask(Dec2, Nepal)
Dec2 <- mask(Dec2, elabove3k)
Dec2_Means <- setZ(Dec2, Dec2_date)
Dec2_pl <- levelplot(Dec2_Means, par.settings = mapTheme, at=my.at)
Dec2_pl + layer(sp.lines(Nepal, lwd =0.8, col="darkgray"))

#NDSI_JJAS <- stack(NDSI_JJAS)
#JJAS_Means <- setZ(NDSI_JJAS, Annual_date) # set Z coordinate to display years
#JJAS_pl <- levelplot(JJAS_Means, par.settings = mapTheme, at=my.at) # set maptheme to blue
#JJAS_pl + layer(sp.lines(Nepal, lwd =0.8, col="darkgray")) # add outline


NDSI_JJAS <- stack(NDSI_JJAS)
extent(elabove3k) <- extent(NDSI_JJAS)
NDSI_JJAS <- crop(NDSI_JJAS, Nepal)
NDSI_JJAS <- mask(NDSI_JJAS, Nepal)
extent(elabove3k) <- extent(NDSI_JJAS)
NDSI_JJAS <- mask(NDSI_JJAS, elabove3k)

my.at <- seq(0,30, by=5)
mapTheme <- rasterTheme(region=brewer.pal(9,"Blues"))
mapTheme$panel.background$col='lightgrey'

JJAS.sd <- overlay(NDSI_JJAS,  fun = sd, na.rm = TRUE) # calculates standard deviation of mean JJAS NDSI 
plot(JJAS.sd)
JJAS.sd.pl <- levelplot(JJAS.sd, par.settings = mapTheme, at=my.at, main = "Standard Deviation of Mean JJAS NDSI (2000 - 2019)") # set maptheme to blue
JJAS.sd.pl + layer(sp.lines(Nepal, lwd =0.8, col="darkgray"))



# post-monsoon ######
NDSI_ON <- list.files("./Seasonal Subsets/NDSI/ON/", pattern = ".tif", all.files = T, full.names = T)

Annual_date <- stri_sub(NDSI_ON, 30,33) # extract years
Dec1 <- NDSI_ON[1:10] # make smaller sets so that one decade can be displayed per plot
Dec2 <- NDSI_ON[11:19]

#library(rasterVis)

# this bit sets the graphical parameters 
my.at <- seq(0,100, by=20)
mapTheme <- rasterTheme(region=brewer.pal(9,"Blues"))
mapTheme$panel.background$col='lightgrey'

Dec1_date <- stri_sub(Dec1, 30,33) # extract dates of decade
Dec1 <- stack(Dec1) # make rasterstack
Dec1 <- crop(Dec1, Nepal) # crop by shapefile Nepal
Dec1 <- mask(Dec1, Nepal) # mask by shapefile nepal
Dec1 <- mask(Dec1, elabove3k) # mask everything below 3000 m
Dec1_Means <- setZ(Dec1, Dec1_date) # this way R know which years to plot
Dec1_pl <- levelplot(Dec1_Means, par.settings = mapTheme, at=my.at) # this makes a pretty plot
Dec1_pl + layer(sp.lines(Nepal, lwd =0.8, col="darkgray")) # and this add the country outline


Dec2_date <- stri_sub(Dec2,30,33)
Dec2 <- stack(Dec2)
Dec2 <- crop(Dec2, Nepal)
Dec2 <- mask(Dec2, Nepal)
Dec2 <- mask(Dec2, elabove3k)
Dec2_Means <- setZ(Dec2, Dec2_date)
Dec2_pl <- levelplot(Dec2_Means, par.settings = mapTheme, at=my.at)
Dec2_pl + layer(sp.lines(Nepal, lwd =0.8, col="darkgray"))

#NDSI_ON <- stack(NDSI_ON)
#ON_Means <- setZ(NDSI_ON, Annual_date) # set Z coordinate to display years
#ON_pl <- levelplot(ON_Means, par.settings = mapTheme, at=my.at) # set maptheme to blue
#ON_pl + layer(sp.lines(Nepal, lwd =0.8, col="darkgray")) # add outline


NDSI_ON <- stack(NDSI_ON)
extent(elabove3k) <- extent(NDSI_ON)
NDSI_ON <- crop(NDSI_ON, Nepal)
NDSI_ON <- mask(NDSI_ON, Nepal)
extent(elabove3k) <- extent(NDSI_ON)
NDSI_ON <- mask(NDSI_ON, elabove3k)

my.at <- seq(0,30, by=5)
mapTheme <- rasterTheme(region=brewer.pal(9,"Blues"))
mapTheme$panel.background$col='lightgrey'

ON.sd <- overlay(NDSI_ON,  fun = sd, na.rm = TRUE) # calculates standard deviation of mean ON NDSI 
plot(ON.sd)
ON.sd.pl <- levelplot(ON.sd, par.settings = mapTheme, at=my.at, main = "Standard Deviation of Mean ON NDSI (2000 - 2018)") # set maptheme to blue
ON.sd.pl + layer(sp.lines(Nepal, lwd =0.8, col="darkgray"))


##############descriptive statistics ##############

NDSI_DJF %>%
  map_df(function(x){
    data.frame(
      mx=raster::maxValue(x),
      mn=raster::minValue(x),
      avg=mean(x[],na.rm=T),
      md=median(x[],na.rm=T),
      stdev=sd(x[],na.rm=T))}) # this function treats rasters and rasterstacks as dataframes and calculates statistics



library(purrr)
extent(el3k4k)
el3k4k <- crop(el3k4k, Nepal)
el3k4k <- mask(el3k4k, Nepal)
extent(el5k6k) <- extent(elabove3k)

Annual <- list.files("./Results/NDSIannual/", pattern = ".tif", all.files = T, full.names = T) %>%
  map(raster) %>% 
  map(function(x) crop(x, Nepal))  %>% 
  map(function(x) mask(x, Nepal))  %>% 
  map(function(x) mask(x, el6k)) %>% 
  map_df(function(x){
    data.frame(
      mx=raster::maxValue(x),
      mn=raster::minValue(x),
      avg=mean(x[],na.rm=T),
      md=median(x[],na.rm=T),
      stdev=sd(x[],na.rm=T))})


write.table(Annual, file = "E:/Results/Statistics/NDSI/Annual_Means_el6k_Statistics.csv", sep =";", dec = ".", row.names = T, col.names = T)



NDSI_DJF <- list.files("./Seasonal Subsets/NDSI/DJF/", pattern = ".tif", all.files = T, full.names = T)%>%
  map(raster) %>% 
  map(function(x) crop(x, Nepal))  %>% 
  map(function(x) mask(x, Nepal))  %>% 
  map(function(x) mask(x, el6k)) %>% 
  map_df(function(x){
    data.frame(
      mx=raster::maxValue(x),
      mn=raster::minValue(x),
      avg=mean(x[],na.rm=T),
      md=median(x[],na.rm=T),
      stdev=sd(x[],na.rm=T))})

write.table(NDSI_DJF, file = "E:/Results/Statistics/NDSI/DJF_Means_el6k_Statistics.csv", sep =";", dec = ".", row.names = T, col.names = T)


NDSI_MAM <- list.files("./Seasonal Subsets/NDSI/MAM/", pattern = ".tif", all.files = T, full.names = T)%>%
  map(raster) %>% 
  map(function(x) crop(x, Nepal))  %>% 
  map(function(x) mask(x, Nepal))  %>% 
  map(function(x) mask(x, el6k)) %>% 
  map_df(function(x){
    data.frame(
      mx=raster::maxValue(x),
      mn=raster::minValue(x),
      avg=mean(x[],na.rm=T),
      md=median(x[],na.rm=T),
      stdev=sd(x[],na.rm=T))})
write.table(NDSI_MAM, file = "E:/Results/Statistics/NDSI/MAM_Means_el6k_Statistics.csv", sep =";", dec = ".", row.names = T, col.names = T)




NDSI_ON <- list.files("./Seasonal Subsets/NDSI/ON/", pattern = ".tif", all.files = T, full.names = T)%>%
  map(raster) %>% 
  map(function(x) crop(x, Nepal))  %>% 
  map(function(x) mask(x, Nepal))  %>% 
  map(function(x) mask(x, el5k6k)) %>% 
  map_df(function(x){
    data.frame(
      mx=raster::maxValue(x),
      mn=raster::minValue(x),
      avg=mean(x[],na.rm=T),
      md=median(x[],na.rm=T),
      stdev=sd(x[],na.rm=T))})
write.table(NDSI_DJF, file = "E:/Results/Statistics/NDSI/ON_Means_el5k6k_Statistics.csv", sep =";", dec = ".", row.names = T, col.names = T)


NDSI_ON <- list.files("./Seasonal Subsets/NDSI/ON/", pattern = ".tif", all.files = T, full.names = T)%>%
  map(raster) %>% 
  map(function(x) crop(x, Nepal))  %>% 
  map(function(x) mask(x, Nepal))  %>% 
  map(function(x) mask(x, el6k)) %>% 
  map_df(function(x){
    data.frame(
      mx=raster::maxValue(x),
      mn=raster::minValue(x),
      avg=mean(x[],na.rm=T),
      md=median(x[],na.rm=T),
      stdev=sd(x[],na.rm=T))})
write.table(NDSI_DJF, file = "E:/Results/Statistics/NDSI/ON_Means_el6k_Statistics.csv", sep =";", dec = ".", row.names = T, col.names = T)




### JJAS ####


NDSI_JJAS <- list.files("./Seasonal Subsets/NDSI/JJAS/", pattern = ".tif", all.files = T, full.names = T)%>%
  map(raster) %>% 
  map(function(x) crop(x, Nepal))  %>% 
  map(function(x) mask(x, Nepal))  %>% 
  map(function(x) mask(x, elabove3k)) %>% 
  map_df(function(x){
    data.frame(
      mx=raster::maxValue(x),
      mn=raster::minValue(x),
      avg=mean(x[],na.rm=T),
      md=median(x[],na.rm=T),
      stdev=sd(x[],na.rm=T))})
write.table(NDSI_JJAS, file = "E:/Results/Statistics/NDSI/JJAS_Means_Statistics.csv", sep =";", dec = ".", row.names = T, col.names = T)



NDSI_JJAS <- list.files("./Seasonal Subsets/NDSI/JJAS/", pattern = ".tif", all.files = T, full.names = T)%>%
  map(raster) %>% 
  map(function(x) crop(x, Nepal))  %>% 
  map(function(x) mask(x, Nepal))  %>% 
  map(function(x) mask(x, el3k4k)) %>% 
  map_df(function(x){
    data.frame(
      mx=raster::maxValue(x),
      mn=raster::minValue(x),
      avg=mean(x[],na.rm=T),
      md=median(x[],na.rm=T),
      stdev=sd(x[],na.rm=T))})
write.table(NDSI_JJAS, file = "E:/Results/Statistics/NDSI/JJAS_Means_el3k4k_Statistics.csv", sep =";", dec = ".", row.names = T, col.names = T)


NDSI_JJAS <- list.files("./Seasonal Subsets/NDSI/JJAS/", pattern = ".tif", all.files = T, full.names = T)%>%
  map(raster) %>% 
  map(function(x) crop(x, Nepal))  %>% 
  map(function(x) mask(x, Nepal))  %>% 
  map(function(x) mask(x, el4k5k)) %>% 
  map_df(function(x){
    data.frame(
      mx=raster::maxValue(x),
      mn=raster::minValue(x),
      avg=mean(x[],na.rm=T),
      md=median(x[],na.rm=T),
      stdev=sd(x[],na.rm=T))})
write.table(NDSI_JJAS, file = "E:/Results/Statistics/NDSI/JJAS_Means_el4k5k_Statistics.csv", sep =";", dec = ".", row.names = T, col.names = T)


NDSI_JJAS <- list.files("./Seasonal Subsets/NDSI/JJAS/", pattern = ".tif", all.files = T, full.names = T)%>%
  map(raster) %>% 
  map(function(x) crop(x, Nepal))  %>% 
  map(function(x) mask(x, Nepal))  %>% 
  map(function(x) mask(x, el5k6k)) %>% 
  map_df(function(x){
    data.frame(
      mx=raster::maxValue(x),
      mn=raster::minValue(x),
      avg=mean(x[],na.rm=T),
      md=median(x[],na.rm=T),
      stdev=sd(x[],na.rm=T))})
write.table(NDSI_JJAS, file = "E:/Results/Statistics/NDSI/JJAS_Means_el5k6k_Statistics.csv", sep =";", dec = ".", row.names = T, col.names = T)


NDSI_JJAS <- list.files("./Seasonal Subsets/NDSI/JJAS/", pattern = ".tif", all.files = T, full.names = T)%>%
  map(raster) %>% 
  map(function(x) crop(x, Nepal))  %>% 
  map(function(x) mask(x, Nepal))  %>% 
  map(function(x) mask(x, el6k)) %>% 
  map_df(function(x){
    data.frame(
      mx=raster::maxValue(x),
      mn=raster::minValue(x),
      avg=mean(x[],na.rm=T),
      md=median(x[],na.rm=T),
      stdev=sd(x[],na.rm=T))})
write.table(NDSI_JJAS, file = "E:/Results/Statistics/NDSI/JJAS_Means_el6k_Statistics.csv", sep =";", dec = ".", row.names = T, col.names = T)








# cloud cover #####

DJF_Clouds <- list.files("./Seasonal Subsets/Clouds/DJF/", pattern = ".tif", all.files = T, full.names = T) %>%
  map(raster) %>% 
  map(function(x) crop(x, Nepal))  %>% 
  map(function(x) mask(x, Nepal))  %>% 
  map(function(x) mask(x, elabove3k)) %>% 
  map_df(function(x){
    data.frame(
      mx=raster::maxValue(x),
      mn=raster::minValue(x),
      avg=mean(x[],na.rm=T),
      md=median(x[],na.rm=T),
      stdev=sd(x[],na.rm=T))})
write.table(DJF_Clouds, file = "E:/Results/Statistics/Clouds/DJF_Clouds_elabove3k_Statistics.csv", sep =";", dec = ".", row.names = T, col.names = T)

MAM_Clouds <- list.files("./Seasonal Subsets/Clouds/MAM/", pattern = ".tif", all.files = T, full.names = T) %>%
  map(raster) %>% 
  map(function(x) crop(x, Nepal))  %>% 
  map(function(x) mask(x, Nepal))  %>% 
  map(function(x) mask(x, elabove3k)) %>% 
  map_df(function(x){
    data.frame(
      mx=raster::maxValue(x),
      mn=raster::minValue(x),
      avg=mean(x[],na.rm=T),
      md=median(x[],na.rm=T),
      stdev=sd(x[],na.rm=T))})
write.table(MAM_Clouds, file = "E:/Results/Statistics/Clouds/MAM_Clouds_elabove3k_Statistics.csv", sep =";", dec = ".", row.names = T, col.names = T)


JJAS_Clouds <- list.files("./Seasonal Subsets/Clouds/JJAS/", pattern = ".tif", all.files = T, full.names = T) %>%
  map(raster) %>% 
  map(function(x) crop(x, Nepal))  %>% 
  map(function(x) mask(x, Nepal))  %>% 
  map(function(x) mask(x, elabove3k)) %>% 
  map_df(function(x){
    data.frame(
      mx=raster::maxValue(x),
      mn=raster::minValue(x),
      avg=mean(x[],na.rm=T),
      md=median(x[],na.rm=T),
      stdev=sd(x[],na.rm=T))})
write.table(JJAS_Clouds, file = "E:/Results/Statistics/Clouds/JJAS_Clouds_elabove3k_Statistics.csv", sep =";", dec = ".", row.names = T, col.names = T)



ON_Clouds <- list.files("./Seasonal Subsets/Clouds/ON/", pattern = ".tif", all.files = T, full.names = T) %>%
  map(raster) %>% 
  map(function(x) crop(x, Nepal))  %>% 
  map(function(x) mask(x, Nepal))  %>% 
  map(function(x) mask(x, elabove3k)) %>% 
  map_df(function(x){
    data.frame(
      mx=raster::maxValue(x),
      mn=raster::minValue(x),
      avg=mean(x[],na.rm=T),
      md=median(x[],na.rm=T),
      stdev=sd(x[],na.rm=T))})
write.table(ON_Clouds, file = "E:/Results/Statistics/Clouds/ON_Clouds_elabove3k_Statistics.csv", sep =";", dec = ".", row.names = T, col.names = T)



Annual_Clouds <- list.files("./Results/CloudCover/", pattern = ".tif", all.files = T, full.names = T) %>%
  map(raster) %>% 
  map(function(x) crop(x, Nepal))  %>% 
  map(function(x) mask(x, Nepal))  %>% 
  map(function(x) mask(x, el6k)) %>% # adjust for various elevation zones
  
  map_df(function(x){
    data.frame(
      mx=raster::maxValue(x),
      mn=raster::minValue(x),
      avg=mean(x[],na.rm=T),
      md=median(x[],na.rm=T),
      stdev=sd(x[],na.rm=T))})

write.table(Annual_Clouds, file = "E:/Results/Statistics/Clouds/Cloud_Sums_el6k_Statistics.csv", sep =";", dec = ".", row.names = T, col.names = T)




# line plots for mean values #####


Annual_Means <- read.table("./Results/Statistics/NDSI/Annual/MEAN_ANNUAL_NDSI_ElevationZones.csv", dec=".", sep = ";", header = T)

library(ggplot2)
library(ggthemes)
library(scales)

plot_annual <- ggplot(Annual_Means, aes(x=year, y=Annual_Means$elabove3k, fill = Annual_Means$elabove3k))
plot_annual + geom_line(aes(x=Annual_Means$year, y=Annual_Means$elabove3k, col = I("steelblue2")))+
  geom_line(aes(x=Annual_Means$year, y=Annual_Means$el3k4k, col = I("yellow")))+
  geom_line(aes(x=Annual_Means$year, y=Annual_Means$el4k5k, col = I("green"))) +
  geom_line(aes(x=Annual_Means$year, y=Annual_Means$el5k6k, col = I("chartreuse4"))) +
  geom_line(aes(x=Annual_Means$year, y=Annual_Means$el6k, col = I("mediumturquoise"))) +
  theme(axis.text.x = element_text(colour = "black", angle = 90, size = 13))+
  labs(x= "Year",
       y = "Mean Snow Covered Area [in %]",
       title = "Mean Snow Covered Area [in %] for Various Elevation Zones") + 
  theme(legend.position = "right")


library(reshape2)
reshape_annual <- melt(Annual_Means, id.vars = "year")

plot_annual <- ggplot(reshape_annual, aes(x=year, y=value, fill =variable)) #, color = variable
plot_annual <- plot_annual + geom_line() +
  labs(x= "Year",
       y = "Mean Annual Snow Covered Area [in %]",
       title = "Mean Snow Covered Area [in %] for Various Elevation Zones") + 
  theme(legend.position = "right") +
  theme(legend.title = element_blank())

plot_annual <- plot_annual + stat_smooth(method = "auto", formula = y ~ x, size = 1)
plot_annual <- plot_annual + scale_fill_colorblind()
plot_annual


box_annual <- ggplot(reshape_annual, aes(x=variable, y=value, fill =variable, color = variable))
plot_annual + geom_boxplot(outlier.shape= 16, outlier.size =2, notch=T) +
  labs(x= "Year",
       y = "Mean Annual Snow Covered Area [in %]",
       title = "Mean Snow Covered Area [in %] for Various Elevation Zones") + 
  theme(legend.position = "right")+
  theme(legend.title = element_blank())

# clouds

Annual_Clouds <- read.table("./Results/Statistics/Clouds/MEAN_ANNUAL_Clouds_ElevationZones.csv", dec=".", sep = ";", header = T)
reshape_annual <- melt(Annual_Clouds, id.vars = "year")

plot_annual_clouds <- ggplot(reshape_annual, aes(x=year, y=value, fill =variable, color = variable))
plot_annual_clouds <- plot_annual_clouds + geom_line() +
  labs(x= "Year",
       y = "Mean Annual Cloud Covered Area [in %]",
       title = "Mean Cloud Covered Area [in %] for Various Elevation Zones") + 
  theme(legend.position = "right")+
  theme(legend.title = element_blank())
plot_annual_clouds <- plot_annual_clouds + stat_smooth(method = "auto", formula = y ~ x, size = 1)
plot_annual_clouds <- plot_annual_clouds + scale_fill_colorblind()
plot_annual_clouds



#### standard deviation
Annual_SD <- read.table("./Results/Statistics/NDSI/Annual/SD_Annual_NDSI_ElevationZones.csv", dec=".", sep = ";", header = T)

library(ggplot2)

plot_annual <- ggplot(Annual_SD, aes(x=year, y=Annual_Means$elabove3k, fill = Annual_Means$elabove3k))
plot_annual + geom_line(aes(x=Annual_Means$year, y=Annual_Means$elabove3k, col = I("steelblue2")))+
  geom_line(aes(x=Annual_Means$year, y=Annual_Means$el3k4k, col = I("yellow")))+
  geom_line(aes(x=Annual_Means$year, y=Annual_Means$el4k5k, col = I("green"))) +
  geom_line(aes(x=Annual_Means$year, y=Annual_Means$el5k6k, col = I("chartreuse4"))) +
  geom_line(aes(x=Annual_Means$year, y=Annual_Means$el6k, col = I("mediumturquoise"))) +
  theme(axis.text.x = element_text(colour = "black", angle = 90, size = 13))+
  labs(x= "Year",
       y = "Mean Snow Covered Area [in %]",
       title = "Mean Snow Covered Area [in %] for Various Elevation Zones") + 
  theme(legend.position = "right")


library(reshape2)
reshape_annual_SD <- melt(Annual_SD, id.vars = "year")

plot_annual <- ggplot(reshape_annual_SD, aes(x=year, y=value, fill =variable, color = variable))
plot_annual + geom_line() +
  labs(x= "Year",
       y = "Standard Deviation of Annual Snow Covered Area [in NDSI units]",
       title = "Standard Deviation of Covered Area [in NDSI units] for Various Elevation Zones") + 
  theme(legend.position = "right")+
  theme(legend.title = element_blank())

#### seasonal means line plot
Seasonal_Means <- read.table("./Results/Statistics/NDSI/NDSI_time_Series_seasonal_means.csv", dec=".", sep = ";", header = T)

library(ggplot2)

plot_annual <- ggplot(Seasonal_Means, aes(x=year, y=Annual_Means$elabove3k, fill = Annual_Means$elabove3k))
plot_annual + geom_line(aes(x=Annual_Means$year, y=Annual_Means$elabove3k, col = I("steelblue2")))+
  geom_line(aes(x=Annual_Means$year, y=Annual_Means$el3k4k, col = I("yellow")))+
  geom_line(aes(x=Annual_Means$year, y=Annual_Means$el4k5k, col = I("green"))) +
  geom_line(aes(x=Annual_Means$year, y=Annual_Means$el5k6k, col = I("chartreuse4"))) +
  geom_line(aes(x=Annual_Means$year, y=Annual_Means$el6k, col = I("mediumturquoise"))) +
  theme(axis.text.x = element_text(colour = "black", angle = 90, size = 13))+
  labs(x= "Year",
       y = "Mean Snow Covered Area [in %]",
       title = "Mean Snow Covered Area [in %] for Various Elevation Zones") + 
  theme(legend.position = "right")


library(reshape2)
reshape_Seasonal_Means <- melt(Seasonal_Means, id.vars = "year")

library(lubridate)
reshape_Seasonal_Means$year <- dmy(reshape_Seasonal_Means$year)


plot_annual <- ggplot(reshape_Seasonal_Means, aes(x=year, y=value, fill =variable, color = variable))
plot_annual <- plot_annual + geom_line() +
  labs(x= "Year",
       y = "Median Seasonal Snow Covered Area [in NDSI units]",
       title = "Median Seasonal Snow Covered Area [in NDSI units] for Various Elevation Zones") + 
  theme(legend.position = "right")+
  theme(legend.title = element_blank())

plot_annual + stat_smooth(method = "auto", formula = y ~ x, size = 12)


##### hovmoeller plot #####



SC_seasons <- list.files("./Seasonal Subsets/NDSI/TimeSeries/", pattern = ".tif", all.files = T, full.names = T)
TS_month <- stri_sub(SC_seasons, 36, 41)
library(zoo)
TS_month  <- as.Date(as.yearmon(TS_month, "%Y%m"))

SC_seasons <- stack(SC_seasons)
SC_seasons <- mask(SC_seasons, Nepal)
SC_seasons <- crop(SC_seasons, Nepal)
#animate(SC_seasons)





idx <- TS_month
SC_seasons <- setZ(SC_seasons, idx, name = "time")
names(SC_seasons) <- as.character(idx)

library(rasterVis)

my.at <- seq(0,100, by=20)
mapTheme <- rasterTheme(region=brewer.pal(9,"Blues"))
mapTheme$panel.background$col='lightgrey'

hovmoller(SC_seasons, 
          at = do.breaks(c(-100, 100), 20),
          contour = F, interpolate = F,
          par.settings = mapTheme,
          main = "NDSI Mean Seasonal Snow Cover Anomalies Across Nepal")






SC_annual <- list.files("./Results/NDSIannual/", pattern = ".tif", all.files = T, full.names = T)
years <- stri_sub(SC_annual, 26, 29)
SC_annual <- stack(SC_annual)
#SC_annual <- mask(SC_annual, Nepal)
#SC_annual <- mask(SC_annual, elabove3k)
#SC_annual <- crop(SC_annual, Nepal)
idx <- years
SC_annual <- setZ(SC_annual, idx, name="Year")
names(SC_annual) <- as.character(idx)

my.at <- seq(0,100, by=20)
mapTheme <- rasterTheme(region=brewer.pal(9,"Blues"))
mapTheme$panel.background$col='lightgrey'

hovmoller(SC_annual,
          contour = F, interpolate = T,
          at = do.breaks(c(-10, 15), 10),
          dirXY = x,
          par.settings = RdBuTheme,
          main = "NDSI Mean Annual Snow Cover Anomalies Across Nepal")

# is usually used to show wave characteristics of climate parameters - here it nicely displays snow cover anomalies by latitude in time
hovmoller(SC_annual,
          contour = F, interpolate = T,
          #at = do.breaks(c(-5, 5), 10),
          #dirXY = x,
          par.settings = mapTheme,
          main = "NDSI Mean Annual Snow Cover Anomalies by Latitude (Nepal)")



# load seasons without NDSI threshold

ON <- list.files("./Seasonal Subsets/NDSI/ON/", pattern = ".tif", all.files = T, full.names = T)
DJF <- list.files("./Seasonal Subsets/NDSI/DJF/", pattern = ".tif", all.files = T, full.names = T)
MAM <- list.files("./Seasonal Subsets/NDSI/MAM/", pattern = ".tif", all.files = T, full.names = T)


DJF <- stack(DJF)
DJF <- crop(DJF, Nepal)
DJF <- mask(DJF, Nepal)

MAM <- stack(MAM)
MAM <- crop(MAM, Nepal)
MAM <- mask(MAM, Nepal)


ON <- stack(ON)
ON <- crop(ON, Nepal)
ON <- mask(ON, Nepal)















# # entire region
DEM <- raster("./GIS/DEM_New/DEM_.sdat")
DEM <- readAll(DEM)
plot(DEM)

# Nepal
Nepal <- getData("GADM", country = "Nepal", level = 0)
plot(Nepal)
str(Nepal)

library(rasterVis)

my.at <- seq(20,100, by=20)
mapTheme <- rasterTheme(region=brewer.pal(9,"Blues"))
mapTheme$panel.background$col='lightgrey'



# maps for mean seasonal NDSI


NDSI_DJF.mean <- overlay(DJF, fun =mean) #, nr.rm =T)
plot(NDSI_DJF.mean)
summary(NDSI_DJF.mean)

NDSI_MAM.mean <- overlay(MAM, fun=mean) #, na.rm=T)
plot(NDSI_MAM.mean)
summary(NDSI_MAM.mean)

NDSI_ON.mean <- overlay(ON,  fun = mean) #, na.rm = TRUE) 
plot(NDSI_ON.mean)
summary(NDSI_ON.mean)

ON.mean.pl_nt <- levelplot(NDSI_ON.mean, par.settings = mapTheme, at=my.at, main = "Mean Snow Cover During Post-Monsoon")
ON.mean.pl_nt + layer(sp.lines(Nepal, lwd =0.8, col="darkgray"))

DJF.mean.pl_nt <- levelplot(NDSI_DJF.mean, par.settings = mapTheme, at=my.at, main = "Mean Snow Cover During Winter")
DJF.mean.pl_nt + layer(sp.lines(Nepal, lwd =0.8, col="darkgray"))

MAM.mean.pl_nt <- levelplot(NDSI_MAM.mean, par.settings = mapTheme, at=my.at, main = "Mean Snow Cover During Pre-Monsoon")
MAM.mean.pl_nt + layer(sp.lines(Nepal, lwd =0.8, col="darkgray"))


# maps for median

# maps for mean seasonal NDSI

NDSI_Annual.median <- overlay(Annual, fun=median) #, na.rm=T)
plot(NDSI_Annual.median)
summary(NDSI_Annual.median)


NDSI_DJF.median <- overlay(DJF, fun =median) #, nr.rm =T)
plot(NDSI_DJF.median)
summary(NDSI_DJF.median)

NDSI_MAM.median <- overlay(MAM, fun=median) #, na.rm=T)
plot(NDSI_MAM.median)
summary(NDSI_MAM.median)

NDSI_ON.median <- overlay(ON,  fun = median)#, na.rm = TRUE) 
plot(NDSI_ON.median)
summary(NDSI_ON.median)

ON.median.pl_nt <- levelplot(NDSI_ON.median, par.settings = mapTheme, at=my.at, main = "Median Snow Cover During Post-Monsoon")
ON.median.pl_nt + layer(sp.lines(Nepal, lwd =0.8, col="darkgray"))

DJF.median.pl_nt <- levelplot(NDSI_DJF.median, par.settings = mapTheme, at=my.at, main = "Median Snow Cover During Winter")
DJF.median.pl_nt + layer(sp.lines(Nepal, lwd =0.8, col="darkgray"))

MAM.median.pl_nt <- levelplot(NDSI_MAM.median, par.settings = mapTheme, at=my.at, main = "Median Snow Cover During Pre-Monsoon")
MAM.median.pl_nt + layer(sp.lines(Nepal, lwd =0.8, col="darkgray"))



# maps for max seasonal NDSI

NDSI_Annual.max <- overlay(Annual, fun=max) #, na.rm=T)
plot(NDSI_Annual.max)
summary(NDSI_Annual.max)


NDSI_DJF.max <- overlay(DJF, fun =max) #, nr.rm =T)
plot(NDSI_DJF.max)
summary(NDSI_DJF.max)

NDSI_MAM.max <- overlay(MAM, fun=max) #, na.rm=T)
plot(NDSI_MAM.max)
summary(NDSI_MAM.max)

NDSI_ON.max <- overlay(ON,  fun = max)#, na.rm = TRUE) 
plot(NDSI_ON.max)
summary(NDSI_ON.max)

ON.max.pl_nt <- levelplot(NDSI_ON.max, par.settings = mapTheme, at=my.at, main = "Maximum Snow Cover During Post-Monsoon (2000 - 2018)")
ON.max.pl_nt + layer(sp.lines(Nepal, lwd =0.8, col="darkgray"))

DJF.max.pl_nt <- levelplot(NDSI_DJF.max, par.settings = mapTheme, at=my.at, main = "Maximum Snow Cover During Winter (2000 - 2019)")
DJF.max.pl_nt + layer(sp.lines(Nepal, lwd =0.8, col="darkgray"))

MAM.max.pl_nt <- levelplot(NDSI_MAM.max, par.settings = mapTheme, at=my.at, main = "Maximum Snow Cover During Pre-Monsoon (2000 - 2018)")
MAM.max.pl_nt + layer(sp.lines(Nepal, lwd =0.8, col="darkgray"))



library(raster)
library(stringi)

# entire region
DEM <- raster("./GIS/DEM_New/DEM_.sdat")
DEM <- readAll(DEM)
plot(DEM)

# Nepal
Nepal <- getData("GADM", country = "Nepal", level = 0)
plot(Nepal)
str(Nepal)

DEM_Nepal <- crop(DEM, Nepal)
DEM_Nepal <- mask(DEM, Nepal)
DEM_Nepal <- crop(DEM_Nepal, Nepal)
plot(DEM_Nepal)

slas <- slopeAspect(DEM_Nepal)
plot(slas)
hill <- hillShade(slas[[1]], slas[[2]], 40, 70)
plot(hill, col=grey(0:100/100), legend = F)



library(raster)
library(rasterVis)
library(maptools) ## for readShapeLines
library(colorspace) ## for terrain_hcl
library(rasterVis)

terrainTheme <- modifyList(rasterTheme(region=terrain_hcl(n=15)),
                           list(panel.background=list(col='white')))
hsTheme <- modifyList(GrTheme(),                   
                      list(regions=list(alpha=0.4)))

#lp <- levelplot(DEM_Nepal, par.settings= terrainTheme, margin =F, colorkey = T)
lp_ON <- levelplot(DEM_Nepal, par.settings=terrainTheme, margin=FALSE, colorkey=T, maxpixels = 2e5) +
  levelplot(hill, par.settings=hsTheme, maxpixels = 2e7) +
  levelplot(NDSI_ON.mean, par.settings = mapTheme, at=my.at, main = "Mean Snow Cover During Post-Monsoon (2000 - 2018)", colorkey=T)



lp_ON

lp_DJF <- levelplot(DEM_Nepal, par.settings=terrainTheme, margin=FALSE, colorkey=T, maxpixels = 2e5) +
  levelplot(hill, par.settings=hsTheme, maxpixels = 2e7) +
  levelplot(NDSI_DJF.mean, par.settings = mapTheme, at=my.at, main = "Mean Snow Cover During Winter (2000 - 2019)", colorkey=T)

lp_DJF


lp_MAM <- levelplot(DEM_Nepal, par.settings=terrainTheme, margin=FALSE, colorkey=T, maxpixels = 2e5) +
  levelplot(hill, par.settings=hsTheme, maxpixels = 2e7) +
  levelplot(NDSI_MAM.mean, par.settings = mapTheme, at=my.at, main = "Mean Snow Cover During Pre-Monsoon (2000 - 2019)", colorkey=T)


lp_MAM


# max plots


lp_ON_max <- levelplot(DEM_Nepal, par.settings=terrainTheme, margin=FALSE, colorkey=T) +#, maxpixels = 2e5) +
  #levelplot(hill, par.settings=hsTheme, maxpixels = 2e7) +
  levelplot(NDSI_ON.max, par.settings = mapTheme, at=my.at, main = "Mean Snow Cover During Post-Monsoon (2000 - 2018)", colorkey=T)


lp_ON_max

lp_DJF_max <- levelplot(DEM_Nepal, par.settings=terrainTheme, margin=FALSE, colorkey=T)+#, maxpixels = 2e5) +
  # levelplot(hill, par.settings=hsTheme, maxpixels = 2e7) +
  levelplot(NDSI_DJF.max, par.settings = mapTheme, at=my.at, main = "Mean Snow Cover During Winter (2000 - 2019)", colorkey=T)

lp_DJF_max


lp_MAM_max <- levelplot(DEM_Nepal, par.settings=terrainTheme, margin=FALSE, colorkey=T)+#, maxpixels = 2e5) +
  #levelplot(hill, par.settings=hsTheme, maxpixels = 2e7) +
  levelplot(NDSI_MAM.max, par.settings = mapTheme, at=my.at, main = "Mean Snow Cover During Pre-Monsoon (2000 - 2019)", colorkey=T)


lp_MAM_max



