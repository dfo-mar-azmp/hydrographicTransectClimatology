rm(list=ls())
library(csasAtlPhys)
library(oce)
source('00_setupFile.R')

CapStr <- function(y) {
  c <- strsplit(y, " ")[[1]]
  paste(toupper(substring(c, 1,1)), substring(c, 2),
        sep="", collapse=" ")
}

load(paste(destDirData, 'ctdFiltered.rda', sep = '/')) # from 01b_filterAzmpArchiveData

df <- data.frame(time = as.POSIXct(unlist(lapply(ctd, function(k) k[['startTime']])), origin = '1970-01-01', tz = 'UTC'),
                 cruiseNumber = unlist(lapply(ctd, function(k) k[['cruiseNumber']])),
                 station = unlist(lapply(ctd, function(k) k[['stationName']])),
                 transect = unlist(lapply(ctd, function(k) k[['transect']])))
# omit station = LL_01 and transect = 'stAnnsBank'
omit <- df[['transect']] == 'stAnnsBank' & df[['station']] == 'LL_01'
df <- df[!omit, ]
# characterize the season, crude : spring = month < 6, fall = month > 6
season <- vector(length = dim(df)[1])
month <- as.POSIXlt(df[['time']])$mon + 1
fall <- month > 6
spring <- month < 6
season[spring] <- 'spring'
season[fall] <- 'fall'
season[!spring & !fall] <- NA

yearRange <- range(as.POSIXlt(df[['time']])$year + 1900)
yearLevels <- seq(yearRange[1], yearRange[2], 1)
df <- data.frame(df, season = season, year = factor(as.POSIXlt(df[['time']])$year + 1900, levels = yearLevels))
dfsub <- df[, names(df) %in% c('transect', 'season', 'year')]
udfsub <- unique(dfsub)
udfsub <- udfsub[!is.na(udfsub[['transect']]), ]


transectOrder <- rev(c('cabotStrait', 'louisbourg', 'halifaxInshore', 'brownsBank', # core
                       'stAnnsBank', 'stPierreBank', 'laurentianChannelMouth', 'theGully', 'sableIslandBank', 'laHaveBank', 'roseway', 'northEastChannel', 'yarmouth', 'portsmouth')) # ancillary
# number of unique stations per transect per year per season
sdf <- split(df, df[['season']])
ssdf <- lapply(sdf, function(k) split(k, k[['transect']]))
ssdftbl <- lapply(ssdf, function(k) lapply(k, function(kk) table(kk[, names(kk) %in% c('station', 'year')])))
ssdftblcnt <- lapply(ssdftbl, function(k) lapply(k, function(kk) apply(kk, 2, function(x) length(which(x != 0)))))
ssdftblcntdo <- lapply(ssdftblcnt, function(k) do.call('rbind', k))
cnttbl <- lapply(ssdftblcntdo, function(k) {o <- unlist(lapply(transectOrder, function(kk) which(rownames(k) == kk )));
k[o, ]})
o <- unlist(lapply(c('spring', 'fall'), function(k) which(names(cnttbl) == k)))
cnttbl <- cnttbl[o]

ssub <- split(udfsub, udfsub[['season']])
tbl <- lapply(ssub, function(k) table(k[,names(k) %in% c('transect', 'year')]))

tbl <- lapply(tbl, function(k) {o <- unlist(lapply(transectOrder, function(kk) which(rownames(k) == kk )));
                                 k[o, ]})
o <- unlist(lapply(c('spring', 'fall'), function(k) which(names(tbl) == k)))
tbl <- tbl[o]

png(paste(destDirFigures, '06_transectSamplingFrequency.png', sep = '/'), width = 8, height = 4, units = 'in', pointsize = 9, res = 250)
par(mfrow = c(1,2), oma = c(0, 6.5, 0 , 1))
for(i in 1:length(tbl)){
  d <- tbl[[i]]
  dcnt <- cnttbl[[i]]
  # imagep of number of profiles per month per year
  cm <- colormap(z = d, breaks = c(0, 0.9, 1.9), col = c('white', 'grey'))
  x <- as.numeric(colnames(d)) # year
  y <- 1:dim(d)[1] # transect, but as numeric for now
  mar <- c(3.5, 1, 2.5, 1)
  imagep(x = x, y = y, z = t(d), 
         colormap = cm,
         drawPalette = FALSE,
         axes = FALSE,
         mar = mar)
  textlab <- expand.grid(x = x,
                         y = y)
  labels <- as.character(t(dcnt))
  labels[labels == '0'] <- ""
  text(x = textlab$x,
       y = textlab$y,
       labels = labels,
       cex = 4/5)
  # get total number of occupations of each transect
  totalOcc <- apply(d, 1, sum)
  mtext(text = totalOcc, side = 4, las = 1, line = 0.25, at = y)
  box()
  axis(1)
  abline(h = y + 0.5)
  abline(h = y[which(rownames(d) == 'brownsBank') - 1] + 0.5, lwd = 3) # to break between core and ancillary
  abline(v = x + 0.5)
  mtext(side = 1, text = 'Year', line = 2, cex = 4/5)
  if(i == 1){
    ynames <- unlist(lapply(rownames(d), getLocationName))
    mtext(side = 2, text = paste0(ynames, '  '), at = y, las = 1, cex = 0.7) 
  }
  mtext(paste0(LETTERS[i], '. ', CapStr(names(tbl)[i])), side = 3, line = 0.5, adj = 0)
}
dev.off()
