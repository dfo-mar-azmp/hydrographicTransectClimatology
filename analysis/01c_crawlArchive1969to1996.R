rm(list=ls())
library(oce)
source('00_setupFile.R')
years <- 1981:1996 # reduced number of years since we're only concerned with data for the two climatology periods
extraarcctd <- NULL
# iterate through each year
for(iy in 1:length(years)){
  path <- paste(arcpath, years[iy], sep = '/')
  files <- list.files(path, pattern = '^.*DN\\.ODF$', full.names = TRUE)
  files <- files[!files %in% badfiles]
  # read in data
  cat(paste('Reading in', length(files), 'files for year', years[iy],'.'), sep = '\n')
  ctd <- lapply(files, read.ctd.odf)
  # helpful if debugging troublesome files
  # ctd <- vector(mode = 'list', length = length(files))
  # for(i in 1:length(files)){
  #   cat(paste(i), sep = '\n')
  #   ctd[[i]] <- read.ctd.odf(files[i])
  # }
  # Classify transect for each profile
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
  for (i in 1:dim(ctdTransect)[2]){
    ok <- which(ctdTransect[,i] == 1)
    transect[[i]] <- rownames(ctdTransect)[ok]
  }
  
  # Classify the station name for each profile
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
  ctdTime <- as.POSIXct(unlist(lapply(ctd, function(k) k[['startTime']])), origin = '1970-01-01', tz = 'UTC')
  ctdMonth <- as.POSIXlt(ctdTime)$mon + 1
  inTime <- ctdMonth %in% c(9:11, 3:5)
  keep <- intersect(which(hasTransect != FALSE), which(inTime != FALSE))
  if(length(keep) == 0){
    cat(paste('          No files found in azmp transect boundaries and time period'), sep = '\n')
  } else {
    ctdkeep <- ctd[keep]
    cat(paste('          Found', length(ctdkeep), 'files that fall within azmp transect boundaries.'), sep = '\n')
    stationkeep <- stationName[keep]
    transectkeep <- transect[keep]
    
    # Classify season
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
                                     'dataType',
                                     'CD')
      ctdkeep[[i]] <- oceSetMetadata(ctdkeep[[i]],
                                 'dataSource',
                                 'extraFromArchive')
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
                                   'dataType',
                                   'CD')
          dupctd <- oceSetMetadata(dupctd,
                                     'dataSource',
                                     'extraFromArchive')
          ctdExtra <- c(ctdExtra, dupctd)
        }
      }
    }
  
    if(is.null(extraarcctd)){
      extraarcctd <- c(ctdkeep, ctdExtra)
    } else {
      extraarcctd <- c(extraarcctd, ctdkeep, ctdExtra)
    }
  }
}
# trim 
extraarcctd <- lapply(extraarcctd, ctdTrim)
# handle flags, if there are any
extraarcctd <- lapply(extraarcctd, function(k) if(length(k[['flags']]) != 0){handleFlags(k, flags = 2:4)} else {k})
# save
save(extraarcctd, file = paste(destDirData, 'extraarcctdFiltered.rda', sep = '/'))
