rm(list=ls())
library(oce)
source('00_setupFile.R')

CapStr <- function(y) {
  c <- strsplit(y, " ")[[1]]
  paste(toupper(substring(c, 1,1)), substring(c, 2),
        sep="", collapse=" ")
}


load(paste(destDirData, 'ctd.rda', sep = '/'))

df <- data.frame(time = as.POSIXct(unlist(lapply(ctd, function(k) k[['startTime']])), origin = '1970-01-01', tz = 'UTC'),
                 cruiseNumber = unlist(lapply(ctd, function(k) k[['cruiseNumber']])))
# characterize the season, crude : spring = month < 6, fall = month > 6
season <- vector(length = dim(df)[1])
month <- as.POSIXlt(df[['time']])$mon + 1
fall <- month > 6
spring <- month < 6
season[spring] <- 'spring'
season[fall] <- 'fall'
season[!spring & !fall] <- NA

df <- data.frame(df, season = season)
sdf <- split(df, df[['cruiseNumber']])
minTime <- as.POSIXct(unlist(lapply(sdf, function(k) min(k[['time']]))), origin = '1970-01-01')
maxTime <- as.POSIXct(unlist(lapply(sdf, function(k) max(k[['time']]))), origin = '1970-01-01')
season <- unlist(lapply(sdf, function(k) k[['season']][1]))
mission <- names(sdf)

dfAll <- data.frame(season = season,
                    mission = mission,
                    startTime = minTime,
                    endTime = maxTime)

seasonl <- split(dfAll, dfAll[['season']])
fakeYear <- 1990
seasons <- c('spring', 'fall')
filename <- '05_missionTimeRange_azmp.png'
png(paste(destDirFigures, filename, sep = '/'), width = 8, height = 3.5, units = 'in', res = 250, pointsize = 9)
par(mfrow=c(1,2))
ylim <- range(as.POSIXlt(dfAll[['startTime']])$year + 1900)
for(i in 1:length(seasonl)){
  ok <- names(seasonl) %in% seasons[i]
  d <- seasonl[ok][[1]]
  dd <- d[1, ]
  start <- as.POSIXlt(d[['startTime']], tz = 'UTC')
  end <- as.POSIXlt(d[['endTime']], tz = 'UTC')
  fakeStart <- as.POSIXct(paste(fakeYear, start$mon + 1, start$mday, sep = '-'), tz = 'UTC')
  fakeEnd <- as.POSIXct(paste(fakeYear, end$mon + 1, end$mday, sep = '-'), tz = 'UTC')
  xlim <- range(c(fakeStart, fakeEnd))
  par(mar = c(3.5, 3.5, 1.5, 1))
  plot(y = c(as.POSIXlt(dd[['startTime']])$year + 1900, as.POSIXlt(dd[['endTime']])$year + 1900),
       x = c(fakeStart[1], fakeEnd[1]),
       ylim = ylim, xlim = xlim,
       ylab = '', xlab = '',
       type = 'l')
  abline(v= pretty(xlim), col = 'grey', lty = 3)
  abline(h = pretty(ylim), col = 'grey', lty = 3)
  for(id in 1:dim(d)[1]){
    dd <- d[id, ]
    lines(y = c(as.POSIXlt(dd[['startTime']])$year + 1900, as.POSIXlt(dd[['endTime']])$year + 1900),
          x = c(fakeStart[id], fakeEnd[id]),
          lwd = 1.4)
  }
  mtext(paste('(', LETTERS[i], ')', CapStr(names(seasonl)[ok])), side = 3, line = 0.5, adj = 0)
  if(names(seasonl)[ok] == 'fall'){
    abline(v = as.POSIXct(c(paste(fakeYear, '09', '15', sep = '-'),paste(fakeYear, '10', '31', sep = '-'))),
           lty = 2, col = 'black')
  }
  if(names(seasonl)[ok] == 'spring'){
    abline(v = as.POSIXct(c(paste(fakeYear, '04', '01', sep = '-'),paste(fakeYear, '05', '15', sep = '-'))),
           lty = 2, col = 'black')
  }
}
dev.off()