rm(list=ls())
library(oce)
source('00_setupFile.R')
load(paste(destDirData, 'arcAndClimateCtd.rda', sep = '/'))
# append correct season based on the dates (unline general months in previous scripts)
startTime <- as.POSIXct(unlist(lapply(arcAndClimateCtd, function(k) k[['startTime']])), origin = '1970-01-01', tz = 'UTC')
startMD <- format(startTime, '%m-%d')
programClimatologyRanges <- climatologyTimeRanges[climatologyTimeRanges[['program']] == 'azmp', ]

okseason <- vector(mode = 'list', length = dim(programClimatologyRanges)[1])
season <- vector(mode = 'logical', length = length(arcAndClimateCtd))
for(i in 1:dim(programClimatologyRanges)[1]){
  k <- programClimatologyRanges[i, ]
  oki <- as.Date(paste(fakeYear, startMD, sep ='-')) %in% seq(from = k[['start']], to = k[['end']], by = 'day')
  okseason[[i]] <- oki
  season[oki] <- k[['season']]
}

okseason <- do.call('rbind', okseason)
hasseason <- apply(okseason, 2, any)

arcAndClimateCtd <- arcAndClimateCtd[hasseason]
season <- season[hasseason]

for(i in 1:length(arcAndClimateCtd)){
  arcAndClimateCtd[[i]] <- oceSetMetadata(arcAndClimateCtd[[i]],
                                          'season',
                                          season[i])
}

save(arcAndClimateCtd, file = paste(destDirData, 'arcAndClimateCtd.rda', sep = '/'))
