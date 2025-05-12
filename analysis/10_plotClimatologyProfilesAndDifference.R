rm(list=ls())
fillWithSmooth <- TRUE # logical that decides if missing stations filled with smoothed climatology profiles should be used
library(csasAtlPhys)
data("transectPlotLimits")
library(oce)
plotProfile <- oce::plotProfile
library(ocedata)
data('coastlineWorldFine')
data("ctd")
ghostctd <- ctd
topoFile <- download.topo(west = -75, east = -50,
                          south = 38, north = 50,
                          resolution = 1)
ocetopo <- read.topo(topoFile)
source('00_setupFile.R')
# get all stations names
allStationNames <- unlist(lapply(stations, function(k) k[['stationName']]))
# for rounding to get stations spacing
mround <- function(x,base){
  base*round(x/base)
}

if(fillWithSmooth){
  load(paste(destDirData, 'newClimatologyFilled.rda', sep = '/'))
} else {
  load(paste(destDirData, 'newClimatology.rda', sep = '/'))
}

climdf <- do.call('rbind', lapply(climatology, function(k) data.frame(season = k[['season']],
                                                                      transect = k[['transect']],
                                                                      program = k[['program']],
                                                                      climatology = k[['climatologyYears']][1])))
utranseas <- unique(climdf[!names(climdf) %in% 'climatology'])

for(i in 1:dim(utranseas)[1]){
  look <- utranseas[i, ]
  ok <- which(climdf[['season']] %in% look[['season']] &
                climdf[['transect']] %in% look[['transect']] &
                climdf[['program']] %in% look[['program']])
  d <- climatology[ok]
  # need to check if 'avgProfiles' exists in each 'climatology' ( need to do this for NEC)
  keep <- NULL
  for(ii in 1:length(d)){
    k <- d[[ii]]
    if(!all(is.na(k[['avgProfiles']]))){
      keep <- c(keep, ii)
    }
  }
  d <- d[keep]
  if(length(d) == 0){
    cat(paste('skipping', look[['season']], look[['transect']], look[['program']], ', no data'))
    next
  }
  dStations <- unique(unlist(lapply(d, function(k) unlist(lapply(k[['avgProfiles']], function(kk) kk[['stationName']])))))
  okStations <- unlist(lapply(dStations, function(k) which(allStationNames == k)))
  stnLon <- unlist(lapply(stations[okStations], function(k) k[['longitude']]))
  stnLat <- unlist(lapply(stations[okStations], function(k) k[['latitude']]))
  allT <- unlist(lapply(d, function(k) lapply(k[['avgProfiles']], function(kk) c(kk[['temperature']] + kk[['temperatureSd']],
                                                                                 kk[['temperature']] - kk[['temperatureSd']]))))
  allS <- unlist(lapply(d, function(k) lapply(k[['avgProfiles']], function(kk) c(kk[['salinity']] + kk[['salinitySd']],
                                                                                 kk[['salinity']] - kk[['salinitySd']]))))
  allST <- unlist(lapply(d, function(k) lapply(k[['avgProfiles']], function(kk) c(kk[['sigmaThetaAvg']] + kk[['sigmaThetaAvgSd']],
                                                                                  kk[['sigmaThetaAvg']] - kk[['sigmaThetaAvgSd']]))))
  profT <- unlist(lapply(d, function(k) unlist(lapply(k[['originalStations']], function(kk) lapply(kk, function(kkk) subset(kkk, pressure >= 10)[['temperature']])))))
  profS <- unlist(lapply(d, function(k) unlist(lapply(k[['originalStations']], function(kk) lapply(kk, function(kkk) subset(kkk, pressure >= 10)[['salinity']])))))
  profST <- unlist(lapply(d, function(k) unlist(lapply(k[['originalStations']], function(kk) lapply(kk, function(kkk) subset(kkk, pressure >= 10)[['sigmaTheta']])))))
  Tlim <- range(c(allT, profT), na.rm = TRUE)
  Slim <- range(c(allS, profS), na.rm = TRUE)
  STlim <- range(c(allST, profST), na.rm = TRUE)
  
  # function to plot line and polygon to keep code DRY
  plotLineAndPolygon <- function(var){
    for(p in 1:length(dd)){
      polyCol <- switch(as.character(climYear[p]),
                        '1981' = hcl.colors(n=5, palette = 'Burg')[3], 
                        '1991' = hcl.colors(n=5, palette = 'Blues')[3])
      alpha <- switch(as.character(climYear[p]),
                      '1981' = 150,
                      '1991' = 100)
      profile <- dd[[p]]
      ok <- !is.na(profile[[var]])
      polygon(c(profile[[var]][ok] - profile[[paste0(var,'Sd')]][ok], 
                rev(profile[[var]][ok] + profile[[paste0(var,'Sd')]][ok])),
              c(profile[['pressure']][ok], 
                rev(profile[['pressure']][ok])), 
              border = NA, col = rgb(t(col2rgb(polyCol)), alpha = alpha, max = 255))
    }
    for(p in 1:length(dd)){
      linCol <- switch(as.character(climYear[p]),
                       '1981' = hcl.colors(n=5, palette = 'Burg')[1],
                       '1991' = hcl.colors(n=5, palette = 'Blues 2')[1])
      profile <- dd[[p]]
      ok <- !is.na(profile[[var]])
      lines(profile[[var]], profile[['pressure']], col = linCol, lwd = 2)
    }
  }
  mar <- c(3.5, 3.5, 3.5, 2.0)
  # mar[1,3] must be the same for profileMar and tsMar
  xmar <- 1.5
  profileMar <- c(xmar, 0.5, xmar, 1.5)
  tsMar <- c(xmar, 3.0, xmar, 0)
  for(stn in dStations){
    dd <- lapply(d, function(k) k[['avgProfiles']][[which(unlist(lapply(k[['avgProfiles']], '[[', 'stationName')) == stn)]])
    # get original profiles
    ddorig <- unname(unlist(lapply(d, function(k) k[['originalStations']][names(k[['originalStations']]) == stn])))
    ## remove duplicates used in both climatologies
    ddorigStartTime <- as.POSIXct(unlist(lapply(ddorig, '[[', 'startTime')), origin = '1970-01-01', tz = 'UTC')
    ust <- unique(ddorigStartTime)
    okorig <- unlist(lapply(ust, function(k) which(ddorigStartTime == k)[1]))
    ddorig <- ddorig[okorig]
    ddorigStartTime <- ddorigStartTime[okorig]
    ## get year and define colormap
    ddorigYear <- as.POSIXlt(ddorigStartTime)$year + 1900
    datacm <- colormap(z = ddorigYear, breaks = c(1981, 1991, 2010, 2020), col = hcl.colors(5, palette = 'RdYlGn')[c(1, 2, 5)])
    # some definitions for plotting
    climYear <- unlist(lapply(d, function(k) k[['climatologyYears']][1]))
    allP <- unlist(lapply(dd, '[[', 'pressure'))
    plim <- rev(range(allP, na.rm = TRUE))
    filename <- paste0(paste('10_profileAndDifference',
                             look[['program']],
                             look[['transect']],
                             look[['season']],
                             gsub('_', '', stn),
                             'wData',
                             sep = '_'),
                       '.png')
    png(filename = paste(destDirFigures,
                         filename, sep = '/'),
        width = 7, height = ifelse(length(dd)==1, 4.5, 7), units = 'in',
        res = 200, pointsize = 13)
    stnIdx <- which(dStations == stn)
    # now plot
    ## set up plotting scheme
    palwidth <- 0.22
    if(length(dd) == 2){
      mlay <- matrix(c(1, 2, 3, 7,
                       4, 5, 6, 0),
                     nrow = 2,
                     byrow = TRUE)
      layout(mat = mlay, widths = c(rep(1-palwidth, 3), palwidth))
    } else {
      mlay <- matrix(c(1, 2, 3, 4),
                     nrow = 1,
                     byrow = TRUE)
      layout(mat = mlay, widths = c(rep(1-palwidth, 3), palwidth))
    }
    par(oma = c(0, 3, 3, 1))
    # temperature
    plotProfile(ghostctd, xtype = 'temperature', Tlim = Tlim, plim = plim, col = 'white', mar = profileMar)
    for(istn in 1:length(ddorig)){
      lines(ddorig[[istn]][['temperature']], ddorig[[istn]][['pressure']], col = datacm$zcol[istn])
    }
    plotLineAndPolygon(var = 'temperature')
    # add stn label
    #mtext(text = stn, side = 3, line = 3.2, adj=0, cex = 0.9)
    #cat(paste('Temp profile measurements', paste(par('fin'), collapse = ', ')), sep = '\n')
    #fin <- par('fin')
    # salinity
    plotProfile(ghostctd, xtype = 'salinity', Slim = Slim, plim = plim, col = 'white', mar = profileMar, ylab = '')
    for(istn in 1:length(ddorig)){
      lines(ddorig[[istn]][['salinity']], ddorig[[istn]][['pressure']], col = datacm$zcol[istn])
    }
    plotLineAndPolygon(var = 'salinity')
    # sigmaTheta
    plotProfile(ghostctd, xtype = 'sigmaTheta', densitylim = STlim, plim = plim, col = 'white', mar = profileMar, ylab = '')
    for(istn in 1:length(ddorig)){
      lines(ddorig[[istn]][['sigmaTheta']], ddorig[[istn]][['pressure']], col = datacm$zcol[istn])
    }
    plotLineAndPolygon(var = 'sigmaThetaAvg')
    # calculate and plot climatology profile differences
    if(length(dd) == 1){
      par(cex = 0.8)
      drawPalette(colormap = datacm, zlab = '', mar = profileMar)
      mtext(text = 'Year', side = 4, line = 4.8, cex = 4/5)
      dev.off()
      next
    }
    ## construct data.frame 
    dfclim <- vector(mode = 'list', length = length(dd))
    for(id in 1:length(dd)){
      ddd <- dd[[id]]
      p <- ddd[['pressure']]
      data <- as.data.frame(ddd@data)
      keep <- grepl(pattern = '^(?:salinity|temperature|sigmaTheta)', names(data))
      data <- data[,keep]
      names(data) <- paste0(names(data), climYear[id])
      dfclim[[id]] <- data.frame(pressure = p,
                                 data)
    }
    climm <- merge(dfclim[[1]], dfclim[[2]], by = 'pressure', all = TRUE)
    # calculate difference
    vars <- c('temperature', 'salinity', 'sigmaThetaAvg')
    varsanomaly <- vector(mode = 'list', length = length(vars))
    varssd <- vector(mode = 'list', length = length(vars))
    for(iv in 1:length(vars)){
      var <- vars[iv]
      # 1991 minus 1981
      vardiff <- climm[[paste0(var, '1991')]] - climm[[paste0(var, '1981')]]
      vardiffsumsqsd <- climm[[paste0(var, 'Sd1991')]]^2 + climm[[paste0(var, 'Sd1981')]]^2
      vardiffsd <- sqrt(vardiffsumsqsd)
      varsanomaly[[iv]] <- vardiff
      varssd[[iv]] <- vardiffsd
    }
    names(varsanomaly) <- paste0(vars, 'Anomaly')
    names(varssd) <- paste0(vars, 'Sd')
    Talim <- range(c(varsanomaly[['temperatureAnomaly']] - (varssd[['temperatureSd']]/2),
                     varsanomaly[['temperatureAnomaly']] + (varssd[['temperatureSd']]/2)),
                   na.rm = TRUE)
    Salim <- range(c(varsanomaly[['salinityAnomaly']]- (varssd[['salinitySd']]/2),
                     varsanomaly[['salinityAnomaly']] + (varssd[['salinitySd']]/2)),
                   na.rm = TRUE)
    STalim <- range(c(varsanomaly[['sigmaThetaAvgAnomaly']]- (varssd[['sigmaThetaAvgSd']]/2),
                      varsanomaly[['sigmaThetaAvgAnomaly']] + (varssd[['sigmaThetaAvgSd']]/2)),
                    na.rm = TRUE)
    for(var in vars){
      # initiate plot
      ylab <- switch(var,
                     'temperature' = NULL,
                     'salinity' = '',
                     'sigmaThetaAvg' = '')
      yaxt <- switch(var,
                     'temperature' = 's',
                     'salinity' = 'n',
                     'sigmaThetaAvg' = 'n')
      plotProfile(ghostctd, xtype = ifelse(var == 'sigmaThetaAvg', 'sigmaTheta', var), 
                  Tlim = Talim, 
                  Slim = Salim,
                  densitylim = STalim,
                  plim = plim, 
                  col = 'white',
                  mar = profileMar,
                  xlab = '',
                  ylab = ylab,
                  yaxt = yaxt)
      polyCol <- hcl.colors(n=5, palette = 'Grays')[3]
      linCol <- hcl.colors(n=5, palette = 'Grays')[1]
      alpha <- 100
      # +/- 0.5 sd
      varanomaly <- paste0(var, 'Anomaly')
      varsd <- paste0(var, 'Sd')
      x <- varsanomaly[[varanomaly]]
      xsd <- varssd[[varsd]]
      ok <- !is.na(x)
      polygon(c(x[ok] - (xsd[ok]/2), 
                rev(x[ok] + (xsd[ok]/2))),
              c(climm[['pressure']][ok], 
                rev(climm[['pressure']][ok])), 
              border = NA, col = rgb(t(col2rgb(polyCol)), alpha = alpha, max = 255))
      # difference
      lines(x, climm[['pressure']], col = linCol, lwd = 2)
      # vertical line at 0
      abline(v = 0)
    }
    par(cex = 0.8)
    par(mar = profileMar)
    drawPalette(colormap = datacm, zlab = '', mar = profileMar)
    mtext(text = 'Year', side = 4, line = 4.8, cex = 4/5)
    dev.off()
  }
}