library(csasAtlPhys)
source('00_setupFile.R')
load(paste(destDirData, 'newClimatologyWAnomalies.rda', sep = '/'))

# remove ones where a climatology couldn't be calculated
isna <- unlist(lapply(climatology, function(k) is.na(k[['latitude0']])))
climatology <- climatology[!isna]

transect <- unlist(lapply(climatology, function(k) k[['transect']]))
season <- unlist(lapply(climatology, function(k) k[['season']]))
climatologyYear <- unlist(lapply(climatology, function(k) min(k[['climatologyYears']])))
hasAnomaly <- unlist(lapply(climatology, function(k) 'temperatureAnomaly' %in% names(k[['section']][['station',1]][['data']])))

save(transect, season, climatologyYear, climatology, hasAnomaly, file = '99_codeToHelpWriteClimatologyDifferences.rda')