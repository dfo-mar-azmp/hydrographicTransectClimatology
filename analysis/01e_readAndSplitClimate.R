rm(list=ls())
library(sp)
library(oce)
source('00_setupFile.R')
files <- list.files(path = destDirData, pattern = '^climate.*\\.csv$', full.names = TRUE)
d <- read.csv(files[length(files)], stringsAsFactors = FALSE)
uc <- unique(d$cruise_id)

# pull out all the ctd profiles by cruise and using the ID
climatectd <- NULL
for (ic in 1:length(uc)){
  cruise <- uc[ic]
  ok <- d$cruise_id %in% cruise
  dd <- d[ok, ]
  uid <- unique(dd$stn_id)
  cat(paste('For cruise', uc[ic], 'there are', length(uid), 'stations'), sep = '\n')
  
  # create a ctd object for each station
  ctd <- vector(mode = 'list', length = length(uid))
  for(id in 1:length(uid)){
    ok <- dd$stn_id %in% uid[id]
    ddd <- dd[ok,]
    o <- order(ddd$pressure)
    ddd <- ddd[o, ]
    timeadj <- ifelse(nchar(ddd$cruise_time) == 1, paste('000', ddd$cruise_time), 
                      ifelse(nchar(ddd$cruise_time) == 2, paste0('00', ddd$cruise_time), 
                             ifelse(nchar(ddd$cruise_time) == 3, paste0('0', ddd$cruise_time), ddd$cruise_time)))
    time <- as.POSIXct(paste(paste(ddd$year, ddd$month, ddd$day, sep = '-'), timeadj), 
                       format = '%Y-%m-%d %H%M', tz = 'UTC')
    
    ctd[[id]] <- as.ctd(salinity = ddd$salinity,
                         temperature = ddd$temperature,
                         pressure = ddd$pressure,
                         #cruise = cruise,
                         time = time,
                         startTime = head(time, 1),
                         latitude = ddd$latitude,
                         longitude = ddd$longitude)
    ctd[[id]] <- oceSetMetadata(ctd[[id]], 'depthMax', max(ddd$maximum_depth))
    ctd[[id]] <- oceSetMetadata(ctd[[id]], 'cruiseNumber', cruise)
    ctd[[id]] <- oceSetMetadata(ctd[[id]], 'dataType', ddd$datatype[1])
  }
  # 3. Classify which station belongs to which transect
  lon <- unlist(lapply(ctd, function(k) k[['longitude']][1]))
  lat <- unlist(lapply(ctd, function(k) k[['latitude']][1]))
  cat(paste('      Classifying transects and stations'), sep = '\n')
  ctdTransect <- lapply(polygons, function(k) mapply(function(longitude, latitude) point.in.polygon(point.x = longitude,
                                                                                                    point.y = latitude,
                                                                                                    pol.x = k[['longitude']],
                                                                                                    pol.y = k[['latitude']]),
                                                     lon,
                                                     lat))
  ctdTransect <- do.call("rbind", ctdTransect)
  transect <- vector(mode = 'list', length = length(ctd))
  # doing it a bit differently since one station can belong in two transect (e.g. LL_01 is included in STAB)
  for (i in 1:dim(ctdTransect)[2]){
    ok <- which(ctdTransect[,i] == 1)
    transect[[i]] <- rownames(ctdTransect)[ok]
  }
  
  # 4. Classify the station name for each profile
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
  
  # check which files didn't get put into a transect, keep the ones that did
  # and also check that it's within time frame
  hasTransect <- unlist(lapply(transect,length)) != 0
  if(any(hasTransect)){
    ctdkeep <- ctd[hasTransect]
    cat(paste('          Found', length(ctdkeep), 'stations that fall within azmp transect boundaries.'), sep = '\n')
    stationkeep <- stationName[hasTransect]
    transectkeep <- transect[hasTransect]
    
    # 5. Classify which season
    startTime <- as.POSIXct(unlist(lapply(ctdkeep, function(k) k[['startTime']])), origin = '1970-01-01', tz = 'UTC')
    season <- vector(mode = 'logical', length = length(ctdkeep))
    month <- as.POSIXlt(startTime)$mon + 1
    okfall <- month %in% 9:11
    okspring <- month %in% 3:5
    season[okfall] <- 'fall'
    season[okspring] <- 'spring'
    
    
    # add the transect, station name, and season to the metadata of each ctd station
    
    ctdExtra <- NULL
    for (i in 1:length(ctdkeep)){
      tran <- transectkeep[[i]]
      stn <- stationkeep[i]
      seas <- season[i]
      ctdkeep[[i]] <- oceSetMetadata(ctdkeep[[i]], 
                                     'transect',
                                     tran[1])
      ctdkeep[[i]] <- oceSetMetadata(ctdkeep[[i]],
                                     'stationName',
                                     stn)
      ctdkeep[[i]] <- oceSetMetadata(ctdkeep[[i]],
                                     'season',
                                     seas)
      ctdkeep[[i]] <- oceSetMetadata(ctdkeep[[i]],
                                     'program',
                                     'azmp')
      ctdkeep[[i]] <- oceSetMetadata(ctdkeep[[i]],
                                 'dataSource',
                                 'climate')
      if(length(tran) > 1){
        print(i)
        for(it in 2:length(tran)){
          dupctd <- ctdkeep[[i]]
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
                                     'dataSource',
                                     'climate')
          ctdExtra <- c(ctdExtra, dupctd)
        }
      }
    }
    climatectd <- c(climatectd, ctdkeep, ctdExtra)
  }
}

save(climatectd, file = paste(destDirData, 'climatectdFiltered.rda', sep = '/'))