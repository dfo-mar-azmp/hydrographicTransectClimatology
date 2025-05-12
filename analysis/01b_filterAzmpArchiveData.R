rm(list=ls())
library(oce)
library(csasAtlPhys)
library(sp) # for point.in.polygon
source('00_setupFile.R') # has polygons and stations
load(paste(destDirData, 'ctd.rda', sep = '/'))

# 1. Classify transect for each profile
lon <- unlist(lapply(ctd, function(k) k[['longitude']][1]))
lat <- unlist(lapply(ctd, function(k) k[['latitude']][1]))
ctdTransect <- lapply(polygons, function(k) mapply(function(longitude, latitude) point.in.polygon(point.x = longitude,
                                                                                                  point.y = latitude,
                                                                                                  pol.x = k[['longitude']],
                                                                                                  pol.y = k[['latitude']]),
                                                   lon,
                                                   lat))
ctdTransect <- do.call("rbind", ctdTransect)
transect <- vector(mode = 'list', length = length(ctd))
# note some stations might belong to multiple transects (e.g. LL_01 is louisbourg and st.anns bank)
for (i in 1:dim(ctdTransect)[2]){
  ok <- which(ctdTransect[,i] == 1)
  transect[[i]] <- rownames(ctdTransect)[ok]
}

# 2. Classify the station name for each profile
ctdStation <- lapply(stations, function(k) mapply(function(longitude, latitude) point.in.polygon(point.x = longitude,
                                                                                                 point.y = latitude,
                                                                                                 pol.x = k[['polyLongitude']],
                                                                                                 pol.y = k[['polyLatitude']]),
                                                  lon,
                                                  lat))
stationName <- vector(mode = 'logical', length = length(ctd))
for(i in 1:length(ctdStation)){
  ok <- which(ctdStation[[i]] == 1)
  stationName[ok] <- stations[[i]][['stationName']]
}

##
# for debugging purposes to double check which stations weren't assigned a station/transect
##
library(ocedata)
data("coastlineWorldFine")
proj <- '+proj=merc'
fillcol <- 'lightgrey'
lonlim <- c(-70, -56)
latlim <- c(41, 48)

nostn <- which(stationName == "FALSE")
notransect <- which(unlist(lapply(transect,length)) == 0)
both <- intersect(nostn, notransect)
par(mar = c(3.5, 3.5, 1, 1))
mapPlot(coastlineWorldFine,
        longitudelim = lonlim,
        latitudelim = latlim,
        col = fillcol,
        proj = proj,
        grid = c(2,1))
mapPoints(lon[notransect[!notransect %in% both]], lat[notransect[!notransect %in% both]], pch = 20, col = 'blue')
mapPoints(lon[nostn[!nostn %in% both]], lat[nostn[!nostn %in% both]], pch = 20, col = 'red')
mapPoints(lon[both], lat[both], pch = 20, col = 'black')
lapply(polygons, function(k) mapPolygon(k[['longitude']], k[['latitude']], border = 'red'))
legend('topleft', pch = c(20, 20, NA), lty = c(NA, NA, 1), col = c('black', 'red', 'red'), legend = c('no stn or tran', 'no stn', 'polygons'))

# 3. Classify which season, roughly, it will be filtered when all data is combined
startTime <- as.POSIXct(unlist(lapply(ctd, function(k) k[['startTime']])), origin = '1970-01-01', tz = 'UTC')
season <- vector(mode = 'logical', length = length(ctd))
month <- as.POSIXlt(startTime)$mon + 1
okfall <- month %in% 9:11
okspring <- month %in% 3:5
season[okfall] <- 'fall'
season[okspring] <- 'spring'


# add the transect, station name, and season to the metadata of each ctd station

ctdExtra <- NULL
for (i in 1:length(ctd)){
  tran <- transect[[i]]
  stn <- stationName[i]
  seas <- season[i]
  ctd[[i]] <- oceSetMetadata(ctd[[i]], 
                             'transect',
                             tran[1])
  ctd[[i]] <- oceSetMetadata(ctd[[i]],
                             'stationName',
                             stn)
  ctd[[i]] <- oceSetMetadata(ctd[[i]],
                             'season',
                             seas)
  ctd[[i]] <- oceSetMetadata(ctd[[i]],
                             'program',
                             'azmp');
  ctd[[i]] <- oceSetMetadata(ctd[[i]],
                             'dataType',
                             'CD')
  ctd[[i]] <- oceSetMetadata(ctd[[i]],
                             'dataSource',
                             'azmpMission')
  
  if(length(tran) > 1){
    print(i)
    for(it in 2:length(tran)){
      dupctd <- ctd[[i]]
      dupctd <- oceSetMetadata(dupctd, 
                               'transect',
                               tran[it])
      dupctd <- oceSetMetadata(dupctd,
                               'stationName',
                               stn)
      dupctd <- oceSetMetadata(dupctd,
                               'season',
                               seas)
      dupctd <- oceSetMetadata(dupctd,
                               'program',
                               'azmp')
      dupctd <- oceSetMetadata(dupctd,
                               'dataType',
                               'CD')
      dupctd <- oceSetMetadata(dupctd,
                               'dataSource',
                               'azmpMission')
      ctdExtra <- c(ctdExtra, dupctd)
    }
  }
}

# combine stations that belong to more than one transect
ctd <- c(ctd, ctdExtra)
# save
save(ctd, file = paste(destDirData, 'ctdFiltered.rda', sep = '/'))
