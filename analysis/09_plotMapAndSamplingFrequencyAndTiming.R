rm(list=ls())
library(csasAtlPhys)
library(oce)
library(ocedata)
data(coastlineWorldFine)
topoFile <- download.topo(west = -75, east = -50,
                          south = 38, north = 50,
                          resolution = 1)
ocetopo <- read.topo(topoFile)
CapStr <- function(y) {
  c <- strsplit(y, " ")[[1]]
  paste(toupper(substring(c, 1,1)), substring(c, 2),
        sep="", collapse=" ")
}

library(oce)
# for rounding to get stations spacing
mround <- function(x,base){
  base*round(x/base)
}
source('00_setupFile.R')

load(paste(destDirData, 'arcAndClimateCtd.rda', sep = '/'))
# for easier handling of data
ctd <- arcAndClimateCtd
rm(arcAndClimateCtd)

# keep only certain data types
dataType <- unlist(lapply(ctd, function(k) k[['dataType']]))
okdt <- dataType %in% c('XB', 'TE', 'CD', 'BF', 'CD', 'BO')
ctd <- ctd[okdt]
# get some important meta for knowing which profile belongs to which
startTime <- as.POSIXct(unlist(lapply(ctd, function(k) k[['startTime']])), origin = '1970-01-01', tz = 'UTC')
startYear <- as.POSIXlt(startTime)$year + 1900
transects <- unlist(lapply(ctd, function(k) k[['transect']]))
dataType <- unlist(lapply(ctd, function(k) k[['dataType']]))
season <- unlist(lapply(ctd, function(k) k[['season']]))
station <- unlist(lapply(ctd, function(k) k[['stationName']]))


df <- data.frame(transect = transects,
                 season = season,
                 station = station,
                 dataType = dataType,
                 startTime = startTime,
                 year = startYear)
df <- df[df[['year']] %in% 1981:2020, ]
# remove instances where no transect was detected
df <- df[!is.na(df[['transect']]), ]
# remove instances where a profile was not associated with a station
df <- df[df[['station']] != FALSE, ]
# remove instances where a profile was not associated with a season
df <- df[df[['season']] != FALSE, ]
coreTransects <- c('cabotStrait', 'louisbourg', 'halifaxInshore', 'brownsBank', 'northEastChannel')
seasons <- c('spring', 'fall')
df <- df[df[['transect']] %in% coreTransects, ]

# split by transect, then season, then station
# this is just for a visual check
# this helped identify that profiles that were not associated with a station or season needed to be omitted
#   from the data frame.
sdf <- lapply(split(df, df[['transect']]), function(k) lapply(split(k, k[['season']]), function(kk) split(kk, kk[['station']])))

# make a table by transect, season, and station
tbl <- lapply(split(df, df[['transect']]), function(k) lapply(split(k, k[['season']]), function(kk) {totable <- data.frame(station = kk[['station']], year = factor(kk[['year']], 1981:2020));
table(totable)}))
# for writing purposes, get number of stations found prior to 1997
nstnb97 <- lapply(tbl, function(k) lapply(k, function(kk) sum(kk[ , as.numeric(colnames(kk)) < 1997])))
dfstn <- do.call('rbind', lapply(nstnb97, function(k) data.frame(season = names(k), nstn = unlist(k))))
dfstn <- data.frame(transect = gsub('(\\w+)\\.\\w+', '\\1', row.names(dfstn)), dfstn)
row.names(dfstn) <- c()
save(dfstn, file = paste(destDirData, 'nStnsPriorTo1997.rda', sep = '/'))

# now plot number of stations per season, per transect, per station
fakeYear <- 2020
# some things for map
proj <- '+proj=merc'
fillcol <- 'lightgray'
lonlim <- c(-70, -56)
latlim <- c(41.5, 48)
mapmar <- c(1.25, 10, 0, 10)
plotmar <- c(1.5, 1, 1.5, 0)
palettemar <- c(2, 0, 2, 6)
for(i in 1:length(tbl)){
  png(paste(destDirFigures, 
            paste0(paste('09_stationMapSamplingFrequencyAndTiming', 
                         names(tbl)[i], 
                         sep = '_'), 
                   '.png'), 
            sep = '/'),
      width = 7.5, height = 6, units = 'in', # portrait
      #width = 8, height = 4.5, units = 'in', # landscape
      pointsize = 10, res = 250)
  # layout for plot
  palwidth <- 0.15
  mlay <- matrix(data = c(1, 1, 1, 1,
                          2, 0, 4, 0,
                          3, 6, 5, 7),
                 nrow = 3, byrow = TRUE)
  layout(mat = mlay, 
         widths = c(1-palwidth, palwidth, 1-palwidth, palwidth),
         heights = c(1.25, 1, 1))
  oma <- c(1.5, 4, 1, 1)
  par(cex = 0.8) # for nice labels in palette
  par(oma = oma)
  # map
  ## get transect stations
  # plot transect stations with labels
  transect <- names(tbl)[i]
  stns <- switch(transect,
                 'brownsBank' = brownsBankStationPolygons,
                 'cabotStrait' = cabotStraitStationPolygons,
                 'halifaxInshore' = halifaxStationPolygons,
                 'louisbourg' = louisbourgStationPolygons,
                 'northEastChannel' = northeastChannelStationPolygons)
  stnlab <- unlist(lapply(stns, '[[', 'stationName'))
  stnnum <- as.numeric(gsub('\\w+_(\\w+)', '\\1', stnlab))
  o <- order(stnnum)
  stnlab <- stnlab[o]
  stnlon <- unlist(lapply(stns, '[[', 'longitude'))[o]
  stnlat <- unlist(lapply(stns, '[[', 'latitude'))[o]
  ## set limits based on stn coordinates
  tranlonlim <- range(stnlon)
  if(diff(tranlonlim) < 1.5) tranlonlim <- tranlonlim + c(-0.5, 0.5)
  tranlatlim <- range(stnlat)
  if(diff(tranlatlim) < 1.5) tranlatlim <- tranlatlim + c(-0.5, 0.5)
  ## increase limits if tight
  par(mar = mapmar)
  mapPlot(coastlineWorldFine, 
          longitudelim = tranlonlim,
          latitudelim = tranlatlim,
          col = fillcol, 
          proj = proj,
          grid = TRUE)
  bathylevels <- c(-3000, -2000, -1000, -200)
  bathycol <- 'lightgrey'
  mapContour(longitude = ocetopo[['longitude']],
             latitude = ocetopo[['latitude']],
             z = ocetopo[['z']],
             levels = bathylevels,
             lwd = 0.8, col = bathycol)
  cat('after map', sep = '\n')
  par('mfg')
  ## plot transect stations with labels
  mapPoints(longitude = stnlon, latitude = stnlat, pch = 20)
  ### get indicies for labelling on left and right side of point
  labelidx <- 1:length(stnlab)
  okidx <- labelidx %in% seq(1, max(labelidx), 2)
  ### right side
  mapText(longitude = stnlon[okidx],
          latitude = stnlat[okidx],
          labels = stnlab[okidx],
          pos = 4,
          cex = 4/5)
  ### left side
  mapText(longitude = stnlon[!okidx],
          latitude = stnlat[!okidx],
          labels = stnlab[!okidx],
          pos = 2,
          cex = 4/5)
  d <- tbl[[i]]
  dstn <- sdf[[which(names(sdf) == names(tbl)[i])]]
  for(is in 1:length(seasons)){
    season <- seasons[is]
    ok <- which(names(d) == season)
    dd <- t(d[[ok]])
    stnnum <- as.numeric(gsub('\\w+_(\\w+)', '\\1', colnames(dd)))
    o <- order(stnnum)
    dd <- dd[, o]
    cm <- colormap(z = dd, breaks = c(0, 0.9, 1.9, 2), col = hcl.colors(n=5, palette = 'Red-Green')[c(2, 4, 5)])
    x <- as.numeric(rownames(dd))
    y <- 1:dim(dd)[2]
    par(mar = plotmar)
    imagep(x = x,
           y = y,
           z = dd,
           colormap = cm,
           drawPalette = FALSE,
           axes = FALSE,
           mar = plotmar)
    cat('after imagep', sep = '\n')
    print(par('mfg'))
    textlab <- expand.grid(x = x,
                           y = y)
    text(x = textlab$x,
         y = textlab$y,
         labels = as.vector(dd),
         col = ifelse(as.vector(dd) > 1, 'white', 'black'),
         cex = 4/5)
    # get total number of profiles for 1981 to 2010 and 1991 to 2020 climatology
    ddtotyear <- dd
    ddtotyear[ddtotyear > 1] <- 1
    clim1total <- apply(ddtotyear[x %in% 1981:2010, ], 2, sum)
    clim2total <- apply(ddtotyear[x %in% 1991:2020, ], 2, sum)
    mtext(text = paste(clim1total, clim2total, sep = ','), side = 4, at = y, las = 1, line = 0.25, cex = 4/5)
    box()
    abline(h = y + 0.5)
    abline(v = x + 0.5)
    axis(1)
    if(season == 'spring') {axis(2, at = y, labels = colnames(dd), las = 1)} else {axis(2, labels = FALSE)}
    # x-axis label
    mtext(side = 1, text = 'Year', line = 1.8, cex = 4/5)
    # label season
    mtext(paste0(CapStr(season)), side = 3, line = 0.25)
    # plot sampling timing throughout the season
    okdstn <- names(dstn) == season
    dstnseas <- dstn[[which(okdstn)]]
    ## get climatology date ranges
    okclimtr <- climatologyTimeRanges[['program']] == 'azmp' & climatologyTimeRanges[['season']] == season
    climStart <- climatologyTimeRanges[['start']][okclimtr]
    climEnd <- climatologyTimeRanges[['end']][okclimtr]
    ## order data based on station number
    stnnum <- as.numeric(gsub('\\w+_(\\w+)', '\\1', names(dstnseas)))
    o <- order(stnnum)
    dstnseas <- dstnseas[o]
    ## define limits
    xlim <- as.POSIXct(c(climStart, climEnd), tz = 'UTC')
    ylim <- c(1,max(length(dstnseas)))
    zlim <- c(1980, 2020)
    # initiate plot
    par(mar = plotmar)
    plot(x = xlim, y = 1:2, col = 'white',
         xlim = xlim, ylim = ylim,
         xlab = '',
         xaxt = 'n', yaxt = 'n')
    # add x-axis
    xlabs <- axis.POSIXct(1)
    ## add grid
    abline(v = xlabs, lty = 3, col = 'lightgrey')
    ## x-label
    mtext(text = 'Date', side = 1, line = 1.8, cex = 4/5)
    # y-axis
    if(season == 'spring') {axis(side = 2, at = ylim[1]:ylim[2], labels = names(dstnseas), las = 1)} else {axis(2, labels = FALSE)}
    ## add grid
    abline(h = ylim[1]:ylim[2], lty = 3, col = 'lightgrey')
    cat('after ts plot', sep = '\n')
    print(par('mfg'))    
    # go through each station
    for(istn in 1:length(dstnseas)){
      stn <- dstnseas[[istn]]
      timelt <- as.POSIXlt(stn[['startTime']])
      fakeTime <- as.POSIXct(paste(fakeYear, timelt$mon + 1, timelt$mday, sep = '-'), tz = 'UTC')
      cm <- colormap(z = stn[['year']], zlim = zlim)
      points(x = fakeTime, 
             y = rep(istn, length(fakeTime)), 
             pch = 21, col = 'lightgrey', 
             bg = cm$zcol, 
             cex = 1.4)
    }
  }
  # draw palettes
  par(mfg=c(2,2)) # 4,2 (portrait) 3,5 (landscape)
  par(mar = palettemar)
  # draw palette
  drawPalette(zlim = zlim, zlab = '')
  cat('mai after drawPalette', sep = '\n')
  print(par('mai'))
  par(mfg=c(2,3)) # 4,4 (portrait)
  par(mar = palettemar)
  #par(mfg=c(2,1)) # 3,3 (landscape)
  # draw palette
  drawPalette(zlim = zlim, zlab = '')
  if(season == 'fall') mtext(text = 'Year', side = 4, line = 9.2, cex = 4/5)
  dev.off()
}

