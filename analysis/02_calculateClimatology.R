rm(list=ls())
# logical that decides if missing stations should be filled with smoothed climatology profiles
# set to TRUE for 'final' climatology product
# set to FALSE for climatology metrics for stations with sufficient sampling
fillWithSmooth <- FALSE
library(csasAtlPhys)
data("transectDepthBins")
data("transectDefinitions")
# omit the surface bin
transectDepthBins <- transectDepthBins[transectDepthBins[['bin']] != 0, ]
library(oce)
data("ctd")
ghostctd <- ctd
plotProfile <- oce::plotProfile
topoFile <- download.topo(west = -75, east = -50,
                          south = 38, north = 50,
                          resolution = 1)
ocetopo <- read.topo(topoFile)
# for rounding to get stations spacing
mround <- function(x,base, type = 'round'){
  if(type == 'round'){
    return(base*round(x/base))
  }
  if(type == 'ceiling'){
    return(base*ceiling(x/base))
  }
}
source('00_setupFile.R')

load(paste(destDirData, 'arcAndClimateCtd.rda', sep = '/'))
# for easier handling of data
cat(paste(length(arcAndClimateCtd), 'profiles for AZMP.'), sep = '\n')
ctd <- arcAndClimateCtd
rm(arcAndClimateCtd)

# get some important meta for knowing which profile belongs to which
startTime <- as.POSIXct(unlist(lapply(ctd, function(k) k[['startTime']])), origin = '1970-01-01', tz = 'UTC')
startYear <- as.POSIXlt(startTime)$year + 1900
transects <- unlist(lapply(ctd, function(k) k[['transect']]))
dataType <- unlist(lapply(ctd, function(k) k[['dataType']]))
season <- unlist(lapply(ctd, function(k) k[['season']]))
station <- unlist(lapply(ctd, function(k) k[['stationName']]))
program <- unlist(lapply(ctd, function(k) k[['program']]))

df <- data.frame(transect = transects,
                 season = season,
                 program = program,
                 station = station,
                 dataType = dataType,
                 startTime = startTime,
                 year = startYear)
# remove instances where no transect was detected
df <- df[!is.na(df[['transect']]), ]
# create lookup table for climatology calculations
coreTransects <- c('cabotStrait', 'louisbourg', 'halifaxInshore', 'brownsBank', 'northEastChannel')
seasons <- c('spring', 'fall')
lookdf <- do.call('rbind', lapply(seasons, function(k) data.frame(transect = coreTransects,
                                                                  season = k)))
lookdf <- data.frame(lookdf, program = 'azmp')

start <- c(1981, 1991)
end <- c(2010, 2020)
lookdfnew <- NULL
for(ic in 1:length(start)){
  for(i in 1:dim(lookdf)[1]){
    newdf <- data.frame(lookdf[i, ],
                        start = start[ic],
                        end = end[ic])
    if(is.null(lookdfnew)){
      lookdfnew <- newdf
    } else {
      lookdfnew <- rbind(lookdfnew, newdf) 
    }
  }
}
lookdf <- lookdfnew

dfStationNames <- data.frame(transect = unlist(lapply(stations, function(k) k[['transectName']])),
                             station = unlist(lapply(stations, function(k) k[['stationName']])))
# add halifaxExtended
dfStationNamesAdd <- data.frame(transect = unlist(lapply(halifaxExtendedStationPolygons, function(k) k[['transectName']])),
                                station = unlist(lapply(halifaxExtendedStationPolygons, function(k) k[['stationName']])))
dfStationNames <- rbind(dfStationNames,
                        dfStationNamesAdd)
# remove the second definition of HL_07
okHL7 <- which(dfStationNames[['station']] == 'HL_07' & dfStationNames[['transect']] == 'halifaxInshore')
dfStationNames <- dfStationNames[-okHL7[2], ]
climatology <- vector(mode = 'list', length = dim(lookdf)[1])
for(it in 1:dim(lookdf)[1]){
  seasonlook <- lookdf[['season']][it]
  transectlook <- lookdf[['transect']][it]
  climatologyYears <- lookdf[['start']][it]:lookdf[['end']][it]
  programlook <- lookdf[['program']][it]
  ok <- df[['season']] == seasonlook & df[['transect']] == transectlook & df[['year']] %in% climatologyYears & df[['program']] == programlook
  d <- df[ok, ]
  transectStations <- dfStationNames[dfStationNames[['transect']] %in% ifelse(transectlook == 'northEastChannel', 'northeastChannel', transectlook) ,]
  cat(paste('Calculating a', seasonlook, 
            'climatology for', transectlook, 
            'for climatology period', paste(range(climatologyYears), collapse = ' to '),
            'for', programlook), sep = '\n')
  # some plot output, commenting out for now
  # pdf(file = paste(destDirSuppFigures, paste0(paste('transectProfiles', 
  #                                             transectlook, 
  #                                             programlook,
  #                                             seasonlook,
  #                                             paste(min(climatologyYears), max(climatologyYears), sep = 'to'), sep = '_'),
  #                                             '.pdf'), sep = '/'))
  tranCtd <- vector(mode = 'list')
  origCtd <- vector(mode = 'list', length = dim(transectStations)[1])
  names(origCtd) <- transectStations[['station']]
  missingStn <- missingStnClimMinYear <- NULL
  cnt <- 1
  for(ts in 1:dim(transectStations)[1]){
    stationlook <- transectStations[['station']][ts]
    okstn <- d[['station']] %in% stationlook
    dd <- d[okstn, ]
    # the row names of the data frame will give us the index of which ctd it is
    idx <- as.numeric(rownames(dd))
    stnctd <- ctd[idx]
    origCtd[[ts]] <- stnctd
    if(length(unique(dd[['year']])) < 10){
      cat(paste('      Unable to calculate climatology for station', stationlook, 'due to insufficient number of profiles'), sep = '\n')
      missingStn <- c(missingStn, stationlook)
      missingStnClimMinYear <- c(missingStnClimMinYear, lookdf[['start']][it])
    } else {
    stnStartTime <- as.POSIXct(unlist(lapply(stnctd, function(k) k[['startTime']])), origin = '1970-01-01', tz = 'UTC')
    pressure <- unlist(lapply(stnctd, function(k) k[['pressure']])) # want it separate from the data for averaging
    stndata <- data.frame(temperature = unlist(lapply(stnctd, function(k) k[['temperature']])),
                          salinity = unlist(lapply(stnctd, function(k) k[['salinity']])),
                          sigmaTheta = unlist(lapply(stnctd, function(k) k[['sigmaTheta']])))
    # might have to figure out something smarter to subset the to max depth bin for each station.
    # update : added the tolerance to get the lower bound of the bins
    # update : use the median max depth to omit using profiles that are deeper than most
    medianMaxDepth <- median(unlist(lapply(stnctd, function(k) max(k[['pressure']]))))
    maxDepthBinIdx <- which.min(abs(medianMaxDepth - (transectDepthBins[['bin']] + transectDepthBins[['tolerance']])))
    stnDepthBins <- transectDepthBins[1:maxDepthBinIdx, ]
    avgProfile <- apply(stndata, 2, function(k) mapply(function(bin, tolerance) mean(k[pressure >= (bin - tolerance) & pressure < (bin + tolerance)], na.rm = TRUE),
                                                       stnDepthBins$bin,
                                                       stnDepthBins$tolerance))
    sdProfile <- apply(stndata, 2, function(k) mapply(function(bin, tolerance) sd(k[pressure >= (bin - tolerance) & pressure < (bin + tolerance)], na.rm = TRUE),
                                                       stnDepthBins$bin,
                                                       stnDepthBins$tolerance))
    avgProfile <- as.data.frame(avgProfile)
    sdProfile <- as.data.frame(sdProfile)
    Tlim <- range(stndata[['temperature']], na.rm = TRUE)
    Slim <- range(stndata[['salinity']], na.rm = TRUE)
    STlim <- range(stndata[['sigmaTheta']], na.rm = TRUE)
    plim <- rev(range(pressure, na.rm = TRUE))
    par(mfrow=c(2,2), oma = c(0,0,1,0))
      
    plotProfile(ghostctd, xtype = 'temperature', Tlim = Tlim, plim = plim, col = 'white')
    polygon(c(avgProfile[['temperature']] - sdProfile[['temperature']], 
              rev(avgProfile[['temperature']] + sdProfile[['temperature']])),
            c(stnDepthBins[['bin']], 
            rev(stnDepthBins[['bin']])), 
            border = NA, col = 'grey')
    lapply(stnctd, function(k) lines(k[['temperature']], k[['pressure']]))
    lines(avgProfile[['temperature']], stnDepthBins[['bin']], col = 'red', lwd = 2)
    # add station name
    mtext(stationlook, outer = TRUE)
    plotProfile(ghostctd, xtype = 'salinity', Slim = Slim, plim = plim, col = 'white')
    polygon(c(avgProfile[['salinity']] - sdProfile[['salinity']], 
              rev(avgProfile[['salinity']] + sdProfile[['salinity']])),
            c(stnDepthBins[['bin']], 
              rev(stnDepthBins[['bin']])), 
            border = NA, col = 'grey')
    lapply(stnctd, function(k) lines(k[['salinity']], k[['pressure']]))
    lines(avgProfile[['salinity']], stnDepthBins[['bin']], col = 'red', lwd = 2)

    plotProfile(ghostctd, xtype = 'sigmaTheta', densitylim = STlim, plim = plim, col = 'white')
    polygon(c(avgProfile[['sigmaTheta']] - sdProfile[['sigmaTheta']], 
              rev(avgProfile[['sigmaTheta']] + sdProfile[['sigmaTheta']])),
            c(stnDepthBins[['bin']], 
              rev(stnDepthBins[['bin']])), 
            border = NA, col = 'grey')
    lapply(stnctd, function(k) lines(k[['sigmaTheta']], k[['pressure']]))
    lines(avgProfile[['sigmaTheta']], stnDepthBins[['bin']], col = 'red', lwd = 2)
      
    plotTS(ghostctd, Tlim = Tlim, Slim = Slim, col = 'white')
    # polygon in T-S space is kind of weird.
    polygon(c(avgProfile[['salinity']] - sdProfile[['salinity']], 
              rev(avgProfile[['salinity']] + sdProfile[['salinity']])),
            c(avgProfile[['temperature']] - sdProfile[['temperature']], 
              rev(avgProfile[['temperature']] + sdProfile[['temperature']])),
            border = NA, col = 'grey')
    lapply(stnctd, function(k) points(k[['salinity']], k[['temperature']]))
    lines(avgProfile[['salinity']], avgProfile[['temperature']], col = 'red', lwd = 2)
    ### commented out lines are for investigation purposes.
    # for BBL_01, spring
    # check <- unlist(lapply(stnctd, function(k) any(k[['salinity']] > 32.5)))
    # bad <- which(check == TRUE)
    # badctd <- stnctd[bad]
    
    # for BBL_02, spring
    # check <- unlist(lapply(stnctd, function(k) any(k[['salinity']] < 30.9)))
    # bad <- which(check == TRUE)
    # badctd <- stnctd[bad]
    
    # for CSL_03, fall
    # check <- unlist(lapply(stnctd, function(k) any(k[['salinity']] > 35)))
    # bad <- which(check == TRUE)
    # badctd <- stnctd[bad]
    
    # for CSL_06, spring - THIS ONE IS FINE
    # check <- unlist(lapply(stnctd, function(k) any(k[['temperature']] > 3 & k[['salinity']] < 32)))
    # bad <- which(check == TRUE)
    # badctd <- stnctd[bad]
    
    # for HL_02, fall
     # check <- unlist(lapply(stnctd, function(k) any(k[['salinity']] < 28))) # two tests here, salinity < 28 & salinty > 36
     # bad <- which(check == TRUE)
     # badctd <- stnctd[bad]
    
    # for HL_01, spring - THIS ONE IS FINE
    # check <- unlist(lapply(stnctd, function(k) any(k[['temperature']] > 7)))
    # bad <- which(check == TRUE)
    # badctd <- stnctd[bad]
    
    # for LL_09, fall
    # check <- unlist(lapply(stnctd, function(k) any(k[['salinity']] < 25)))
    # bad <- which(check == TRUE)
    # badctd <- stnctd[bad]
    
    # for LL_03, spring
    # check <- unlist(lapply(stnctd, function(k) any(k[['salinity']] < 25)))
    # bad <- which(check == TRUE)
    # badctd <- stnctd[bad]
    
    # for LL_06, spring
    # check <- unlist(lapply(stnctd, function(k) any(k[['temperature']] < 0.5))) # two tests, main is sigmaTheta < 25, interest, temp < 0.5
    # bad <- which(check == TRUE)
    # badctd <- stnctd[bad]
    
    # for HL_01
    #check <- unlist(lapply(stnctd, function(k) any(k[['sigmaTheta']] < 24)))
    #bad <- which(check == TRUE)
    #badctd <- stnctd[bad]
    
    # make each average profile a ctd object
    fakeMonth <- ifelse(seasonlook == 'spring', '04', '10')
    if(programlook == 'azomp') fakeMonth <- '05'
    if(programlook == 'iml') fakeMonth <- ifelse(seasonlook == 'winter', '03', ifelse(seasonlook == 'spring', '06', '11'))
    okstation <- which(unlist(lapply(stations, function(k) k[['stationName']])) == stationlook)
    stnmeta <- stations[[okstation[1]]]
    tranCtd[[cnt]] <- as.ctd(salinity = avgProfile[['salinity']],
                            temperature = avgProfile[['temperature']],
                            pressure = stnDepthBins[['bin']],
                            startTime = paste(max(climatologyYears), fakeMonth, 15, sep = '-'),
                            longitude = stnmeta[['longitude']],
                            latitude = stnmeta[['latitude']])
    # have to set sigmaTheta based on averaging
    tranCtd[[cnt]] <- oceSetData(tranCtd[[cnt]],
                                'sigmaThetaAvg', # have to use 'sigmaThetaAvg' since oce will internally calculate 'sigmaTheta'
                                avgProfile[['sigmaTheta']],
                                unit = expression(kg/m^3))
    # add standard deviation
    tranCtd[[cnt]] <- oceSetData(tranCtd[[cnt]],
                                 'temperatureSd',
                                 sdProfile[['temperature']])
    tranCtd[[cnt]] <- oceSetData(tranCtd[[cnt]],
                                 'salinitySd',
                                 sdProfile[['salinity']])
    tranCtd[[cnt]] <- oceSetData(tranCtd[[cnt]],
                                 'sigmaThetaAvgSd',
                                 sdProfile[['sigmaTheta']])
    # add some metadata
    tranCtd[[cnt]] <- oceSetMetadata(tranCtd[[cnt]],
                                    'stationName',
                                    stnmeta[['stationName']])
    tranCtd[[cnt]] <- oceSetMetadata(tranCtd[[cnt]],
                                    'transect',
                                    stnmeta[['transectName']])
    cnt <- cnt + 1
    }
  }
  #dev.off()
  if(length(tranCtd) > dim(transectStations)[1]/2){ # require that there be more than half of the stations present
    # order the stations
    ddlon <- unlist(lapply(tranCtd, function(k) k[['longitude']]))
    ddlat <- unlist(lapply(tranCtd, function(k) k[['latitude']]))
    if(transectlook == 'brownsBank'){
      o <- order(ddlat, decreasing = TRUE)
    } else {
      o <- order(ddlon)
    }
    tranCtd <- tranCtd[o]
    # # create section and barnes interpolate
    s <- as.section(tranCtd)
    sg <- sectionGrid(s)
    factor <- ifelse(transectlook %in% c('halifaxInshore', 'halifaxExtended', 'louisbourg'), 2.5, 1.4)
    xr <- mround(median(diff(sg[['distance', 'byStation']]))/2, 5, type = 'ceiling') * factor
    yr <- 10
    xgrid <- seq(0, ceiling(max(sg[['distance', 'byStation']])) + 1.5*xr, by = xr/2)
    # have to add the is.na part to not interpolate farther down than measurements
    ygrid <- seq(5, ceiling(max(sg[['pressure']][!is.na(sg[['temperature']])])), by = yr/2)
    ygrid <- seq(5, ceiling(max(sg[['pressure']][!is.na(sg[['temperature']])])), by = yr)
    ss <- sectionSmooth(sg, method = 'barnes', xg = xgrid, yg = ygrid, xr = xr, yr = yr)
    # get some meta data things for plotting that was defined previously
    # get bottom polygon
    okdef <- which(names(transectDefinitions) %in% transectlook)
    longitude0 <- transectDefinitions[[okdef]][['info']][['start_longitude']]
    latitude0 <- transectDefinitions[[okdef]][['info']][['start_latitude']]
    ylim <- c(0, transectDefinitions[[okdef]][['info']][['yaxis_max']])
    # get xlim for section plots by using the defined stations
    okstations <- lapply(transectStations[['station']], function(kk) which(unlist(lapply(stations, function(k) k[['stationName']])) == kk))
    transectLongitude <- unlist(lapply(okstations, function(k) stations[[k[1]]][['longitude']]))
    transectLatitude <- unlist(lapply(okstations, function(k) stations[[k[1]]][['latitude']]))
    transectDistance <- geodDist(longitude1 = transectLongitude,
                                 latitude1 = transectLatitude,
                                 longitude2 = longitude0,
                                 latitude2 = latitude0)
    # create missing station data.frame
    if(is.null(missingStn)){
      missingDf <- NULL
    } else {
      missingDf <- data.frame(station = missingStn, 
                              climYear = missingStnClimMinYear,
                              season = seasonlook,
                              transect = transectlook)
      if(fillWithSmooth){
        for(ims in 1:length(missingStn)){
          stnlook <- missingStn[ims]
          # code taken and adapted from 99_compareSectionSmoothToMissingProfiles.R
          # 1. get the station longitude/latitude for the transect
          #   note: as of 20210820 - missing stations only for halifax and lousibourg
          transectlon <- switch(transectlook,
                                'louisbourg' = unlist(lapply(louisbourgStationPolygons, function(k) k[['longitude']])),
                                'halifaxInshore' = unlist(lapply(halifaxStationPolygons, function(k) k[['longitude']])))
          transectlat <- switch(transectlook,
                                'louisbourg' = unlist(lapply(louisbourgStationPolygons, function(k) k[['latitude']])),
                                'halifaxInshore' = unlist(lapply(halifaxStationPolygons, function(k) k[['latitude']])))
          transectStnName <- switch(transectlook,
                                    'louisbourg' = unlist(lapply(louisbourgStationPolygons, function(k) k[['stationName']])),
                                    'halifaxInshore' = unlist(lapply(halifaxStationPolygons, function(k) k[['stationName']])))
          okstn <- which(transectStnName == stnlook)
          stnlon <- transectlon[okstn]
          stnlat <- transectlat[okstn]
          # 2. get smoothed section longitude and latitude and water depth
          sslon <- ss[['longitude', 'byStation']]
          sslat <- ss[['latitude', 'byStation']]
          sswaterDepth <- unlist(lapply(ss[['station']], function(k) k[['waterDepth']]))
          # find closest smoothed profile, and subset it to the inferred waterdepth
          dist <- geodDist(longitude2 = stnlon,
                           latitude2 = stnlat,
                           longitude1 = sslon,
                           latitude1 = sslat)
          okss <- which.min(dist)
          ssctd <- ss[['station', okss]]
          ssctd <- subset(ssctd, pressure <= sswaterDepth[okss])
          # test to see if differences caused by averaging
          ssctd[['pressure']] <- ssctd[['pressure']] - 5
          # 3. modify some of the metadata so it can be slotted into the climatology
          ssctd <- oceSetMetadata(ssctd,
                                  'startTime',
                                  paste(max(climatologyYears), fakeMonth, 15, sep = '-'))
          ssctd <- oceSetMetadata(ssctd,
                                  'longitude',
                                  stnlon)
          ssctd <- oceSetMetadata(ssctd,
                                  'latitude',
                                  stnlat)
          ssctd <- oceSetMetadata(ssctd,
                                  'stationName',
                                   stnlook)
          ssctd <- oceSetMetadata(ssctd,
                                  'transect',
                                  transectlook)
          # 4. ssctd will be have dz = 5m, do binMeanPressureCtd averaging
          #    use waterdepth to subset the depth bins
          stnDepthBins <- transectDepthBins[transectDepthBins[['bin']] <= sswaterDepth[okss], ]
          #ssctdsub <- binMeanPressureCtd(ssctd, bin = stnDepthBins[['bin']], tolerance = stnDepthBins[['tolerance']])
          ssctdsub <- approxPressureCtd(ssctd, bin = stnDepthBins[['bin']])
          tranCtd[[cnt]] <- ssctdsub
          cnt <- cnt + 1
        }
        # repeat what was done above to create section and sectionsmooth
        # order the stations
        ddlon <- unlist(lapply(tranCtd, function(k) k[['longitude']]))
        ddlat <- unlist(lapply(tranCtd, function(k) k[['latitude']]))
        if(transectlook == 'brownsBank'){
          o <- order(ddlat, decreasing = TRUE)
        } else {
          o <- order(ddlon)
        }
        tranCtd <- tranCtd[o]
        # # create section and barnes interpolate
        s <- as.section(tranCtd)
        sg <- sectionGrid(s)
        factor <- ifelse(transectlook %in% c('halifaxInshore', 'halifaxExtended', 'louisbourg'), 2.5, 1.4)
        xr <- mround(median(diff(sg[['distance', 'byStation']]))/2, 5) * factor
        yr <- 10
        xgrid <- seq(0, ceiling(max(sg[['distance', 'byStation']])) + 1.5*xr, by = xr/2)
        # have to add the is.na part to not interpolate farther down than measurements
        ygrid <- seq(5, ceiling(max(sg[['pressure']][!is.na(sg[['temperature']])])), by = yr/2)
        ygrid <- seq(5, ceiling(max(sg[['pressure']][!is.na(sg[['temperature']])])), by = yr)
        ss <- sectionSmooth(sg, method = 'barnes', xg = xgrid, yg = ygrid, xr = xr, yr = yr)
      }
    } # closes create missing station data.frame
      # save info
    climatology[[it]][['season']] <- seasonlook
    climatology[[it]][['transect']] <- transectlook
    climatology[[it]][['program']] <- programlook
    climatology[[it]][['longitude0']] <- longitude0
    climatology[[it]][['latitude0']] <- latitude0
    climatology[[it]][['ylim']] <- ylim
    climatology[[it]][['xlim']] <- c(0, max(transectDistance))
    climatology[[it]][['avgProfiles']] <- tranCtd
    climatology[[it]][['section']] <- s
    climatology[[it]][['sectionGrid']] <- sg
    climatology[[it]][['sectionSmooth']] <- ss
    climatology[[it]][['climatologyYears']] <- climatologyYears
    climatology[[it]][['missingStations']] <- missingDf
    climatology[[it]][['originalStations']] <- origCtd
    }  else {# closes if length(tranCtd) != 0
      cat(paste('   Unable to create a', seasonlook, 
          'climatology for', transectlook, 
          'for climatology period', paste(range(climatologyYears), collapse = ' to '),
          'for', programlook), sep = '\n')
    missingDf <- data.frame(station = missingStn, 
                            climYear = missingStnClimMinYear,
                            season = seasonlook,
                            transect = transectlook)
    climatology[[it]][['season']] <- seasonlook
    climatology[[it]][['transect']] <- transectlook
    climatology[[it]][['program']] <- programlook
    climatology[[it]][['longitude0']] <- NA
    climatology[[it]][['latitude0']] <- NA
    climatology[[it]][['ylim']] <- NA
    climatology[[it]][['xlim']] <- NA
    climatology[[it]][['avgProfiles']] <- NA
    climatology[[it]][['section']] <- NA
    climatology[[it]][['sectionGrid']] <- NA
    climatology[[it]][['sectionSmooth']] <- NA
    climatology[[it]][['climatologyYears']] <- climatologyYears
    climatology[[it]][['missingStations']] <- missingDf
    climatology[[it]][['originalStations']] <- origCtd
    }
}

save(climatology, file = paste(destDirData, paste0('newClimatology', ifelse(fillWithSmooth, 'Filled', ''), '.rda'), sep = '/'))