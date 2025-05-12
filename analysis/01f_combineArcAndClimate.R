rm(list=ls())
library(oce)
library(ocedata)
source('00_setupFile.R')
load(paste(destDirData, 'ctdFiltered.rda', sep = '/'))
arcctd <- ctd
load(paste(destDirData, 'extraarcctdFiltered.rda', sep = '/'))
load(paste(destDirData, 'climatectdFiltered.rda', sep = '/'))

# 'ctd.rda' data from known azmp missions are the main focus
# 'extraarcctd.rda' or other ctd data that lives in the archive that is within the time and in transect boundaries
# 'climatectd.rda' is all other data that lives in the climate database, note that nothing has been done to that data yet

# 1. find duplicates that are in extraarcctd.rda
#    here a duplicate will be defined as any profile within 8km and 6hours
earcStart <- as.POSIXct(unlist(lapply(extraarcctd, function(k) k[['startTime']])), origin = '1970-01-01', tz = 'UTC')
earcLon <- unlist(lapply(extraarcctd, function(k) k[['longitude']][1]))
earcLat <- unlist(lapply(extraarcctd, function(k) k[['latitude']][1]))
arcStart <- as.POSIXct(unlist(lapply(arcctd, function(k) k[['startTime']])), origin = '1970-01-01', tz = 'UTC')
arcLon <- unlist(lapply(arcctd, function(k) k[['longitude']][1]))
arcLat <- unlist(lapply(arcctd, function(k) k[['latitude']][1]))
duplicates <- mapply(function(start, longitude, latitude) {dt <- difftime(start, earcStart, units = 'hours');
                                                           dist <- geodDist(earcLon, earcLat,
                                                                            longitude, latitude);
                                                           which(abs(dt) < 6 & dist < 8)},
                     arcStart,
                     arcLon,
                     arcLat)
extraarcDuplicates <- unique(unlist(duplicates))
if(length(extraarcDuplicates) != 0){
  allctd <- c(arcctd, extraarcctd)
  allStart <- c(arcStart, earcStart)
  allLon <- c(arcLon, earcLon)
  allLat <- c(arcLat, earcLat)
} else {
  allctd <- c(arcctd, extraarcctd[-extraarcDuplicates])
  allStart <- c(arcStart, earcStart[-extraarcDuplicates])
  allLon <- c(arcLon, earcLon[-extraarcDuplicates])
  allLat <- c(arcLat, earcLat[-extraarcDuplicates])
}


# 2. find duplicates that are already in climate.rda, same criteria as above
#    note that we'll also check for duplicates in climate first
cliStart <- as.POSIXct(unlist(lapply(climatectd, function(k) k[['startTime']])), origin = '1970-01-01', tz = 'UTC')
cliLon <- unlist(lapply(climatectd, function(k) k[['longitude']][1]))
cliLat <- unlist(lapply(climatectd, function(k) k[['latitude']][1]))
# line below takes a LONG TIME to run
duplicates <- mapply(function(start, longitude, latitude) {dt <- difftime(start, cliStart, units = 'hours');
                                                           dist <- geodDist(cliLon, cliLat,
                                                                            longitude, latitude);
                                                           which(abs(dt) < 6 & dist < 8)},
                    cliStart,
                    cliLon,
                    cliLat)
# have to remove the index from each list
for(i in 1:length(duplicates)){
  duplicates[[i]] <- duplicates[[i]][duplicates[[i]] != i]
}
duplicatesInClimate <- unique(unlist(duplicates))
climatekeep <- climatectd[-duplicatesInClimate]
ckeepStart <- cliStart[-duplicatesInClimate]
ckeepLon <- cliLon[-duplicatesInClimate]
ckeepLat <- cliLat[-duplicatesInClimate]

# now check against duplicates in arc and extra arc ctd
duplicates <- mapply(function(start, longitude, latitude) {dt <- difftime(start, ckeepStart, units = 'hours');
                                                           dist <- geodDist(ckeepLon, ckeepLat,
                                                           longitude, latitude);
                                                           which(abs(dt) < 6 & dist < 8)},
                     allStart,
                     allLon,
                     allLat)
climateDuplicates <- unique(unlist(duplicates))
finalClimate <- climatekeep[-climateDuplicates]

# 3. all data together
arcAndClimateCtd <- c(allctd, finalClimate)

# 4. Futher filtering based on dataType and time range for both seasons
dataType <- unlist(lapply(arcAndClimateCtd, function(k) k[['dataType']]))
okdt <- dataType %in% c('XB', 'TE', 'CD', 'BF', 'CD', 'BO')
arcAndClimateCtd <- arcAndClimateCtd[okdt]

startTime <- as.POSIXct(unlist(lapply(arcAndClimateCtd, function(k) k[['startTime']])), origin = '1970-01-01', tz = 'UTC')
startMD <- format(startTime, '%m-%d')

azmpClimatologyRanges <- climatologyTimeRanges[climatologyTimeRanges[['program']] == 'azmp', ]
# 5. Check season based on dates
# not sure why the apply way to do 'okseason' failed, switch to for loop
okseason <- vector(mode = 'list', length = dim(azmpClimatologyRanges)[2])
for(i in 1:dim(azmpClimatologyRanges)[1]){
  k <- azmpClimatologyRanges[i, ]
  okseason[[i]] <- startMD %in% format(seq.Date(from = k[['start']], to = k[['end']], by = 'day'), '%m-%d')
}
okseason <- do.call('rbind', okseason)
hasseason <- apply(okseason, 2, any)
arcAndClimateCtd <- arcAndClimateCtd[hasseason]
# save
save(arcAndClimateCtd, file = paste(destDirData, 'arcAndClimateCtd.rda', sep = '/'))