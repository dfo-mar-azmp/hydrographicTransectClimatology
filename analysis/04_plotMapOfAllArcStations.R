rm(list=ls())
library(csasAtlPhys)
library(oce)
library(ocedata)
library(wesanderson)
data('coastlineWorldFine')
topoFile <- download.topo(west = -75, east = -50,
                          south = 38, north = 50,
                          resolution = 1)
ocetopo <- read.topo(topoFile)
source('00_setupFile.R')

load(paste(destDirData, 'ctd.rda', sep = '/'))

lon <- unlist(lapply(ctd, function(k) k[['longitude']][1]))
lat <- unlist(lapply(ctd, function(k) k[['latitude']][1]))

png(paste(destDirFigures, '04_mapOfAllArcStations.png', sep = '/'), width = 6, height = 4, unit = 'in', res = 250, pointsize = 10)

proj <- '+proj=merc'
fillcol <- 'lightgray'
lonlim <- c(-70, -56)
latlim <- c(41.5, 48)

par(mar = c(2.5, 2.5 , 1.5, 1))
mapPlot(coastlineWorldFine, 
        longitudelim = lonlim,
        latitudelim = latlim,
        col = fillcol, 
        proj = proj,
        grid = c(2,1))
levels <- c(50, 250, 1000) * -1
bathycol <- rev(gray.colors(n = length(levels)))
mapContour(longitude = ocetopo[['longitude']],
           latitude = ocetopo[['latitude']],
           z = ocetopo[['z']],
           levels = levels,
           lwd = 0.8, col = bathycol)
mapScalebar('topleft', length = 100)
mapPoints(lon,lat, pch = 20, col = 'black')
# polygons and transect labels
corelinecol <- wes_palette('Darjeeling1')[1]
ancillarycol <- wes_palette('Rushmore1')[3]
core <- c('brownsBank', 'northEastChannel', 'halifaxInshore', 'louisbourg', 'cabotStrait')
okcore <- names(polygons) %in% core
lapply(polygons[okcore], function(k) mapPolygon(k[['longitude']], k[['latitude']], border = corelinecol))
lapply(polygons[!okcore], function(k) mapPolygon(k[['longitude']], k[['latitude']], border = ancillarycol))

# label transects
lang <- 'en'
#mapText(ifelse(lang == 'en', -64.8, -64.5), 42.55, labels = gsub(' ', '\\1\n', getLocationName('Browns Bank')), col = corelinecol, cex = 4/5)
mapText(-66.15, 43.26, labels = gsub(' ', '\\1\n', getLocationName('Browns Bank')), col = corelinecol, cex = 4/5)
mapText(-62.0, 43.9, labels = getLocationName('Halifax'), col = corelinecol, cex = 4/5)
mapText(-58.2, 45.4, labels = getLocationName('Louisbourg'), col = corelinecol, cex = 4/5)
mapText(-60.35, 47.5, labels = gsub(' ', '\\1\n', getLocationName('Cabot Strait')), col = corelinecol, cex = 4/5)
mapText(-66.62, 41.64, labels = gsub(' ', '\\1\n', getLocationName('Northeast Channel')), col = corelinecol, cex = 4/5)

mapText(-68.48, 43.68, labels = getLocationName('Yarmouth'), col = ancillarycol, srt = 10, cex = 4/5)
mapText(-68.48, 42.58, labels = getLocationName('Portsmouth'), col = ancillarycol, srt = -11, cex = 4/5)
mapText(-64.40, 41.64, labels = 'Roseway', col = ancillarycol, cex = 4/5)
mapText(-62.4, 42.2, labels = 'La Have', col = ancillarycol, cex = 4/5)
mapText(-60.5, 44.60, labels = 'Sable Island \n Bank', col = ancillarycol, cex = 4/5)
mapText(-58.71, 43.56, labels = getLocationName('The Gully'), col = ancillarycol, cex = 4/5, srt = 20)
mapText(-58.21, 46.18, labels = 'St Anns \n Bank', col = ancillarycol, cex = 4/5)
mapText(-57.10, 45.15, labels = 'Laurentian \n Channel Mouth', col = ancillarycol, cex = 4/5, srt = 15)
mapText(-56.66, 44.1, labels = 'St.Pierre \n Bank', col = ancillarycol, cex = 4/5)

dev.off()

