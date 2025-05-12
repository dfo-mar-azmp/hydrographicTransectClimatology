rm(list=ls())
# logical that decides if missing stations should be filled with smoothed climatology profiles
# set to TRUE for 'final' climatology product
# set to FALSE for climatology metrics for stations with sufficient sampling
fillWithSmooth <- TRUE
library(csasAtlPhys)
data("transectPlotLimits")
library(oce)
library(ocedata)
data("ctd")
ghostctd <- ctd
source('00_setupFile.R')
# for rounding to get stations spacing
mround <- function(x,base, type = 'round'){
  if(type == 'round'){
    base*round(x/base)
  }
  if(type == 'ceiling'){
    base*ceiling(x/base)
  }
}

if(fillWithSmooth){
  load(paste(destDirData, 'newClimatologyFilled.rda', sep = '/'))
} else {
  load(paste(destDirData, 'newClimatology.rda', sep = '/'))
}


df <- data.frame(transect = unlist(lapply(climatology, function(k) k[['transect']])),
                 season = unlist(lapply(climatology, function(k) k[['season']])),
                 program = unlist(lapply(climatology, function(k) k[['program']])),
                 climStartYear = unlist(lapply(climatology, function(k) min(k[['climatologyYears']]))),
                 climEndYear = unlist(lapply(climatology, function(k) max(k[['climatologyYears']]))))
climatology81 <- climatology[df[['climStartYear']] == 1981]
climatology91 <- climatology[df[['climStartYear']] == 1991]

dftran <- unique(df[ , names(df) %in% c('transect', 'season', 'program')])
climatology91WAnom <- vector(mode = 'list', length = length(climatology91)) # it is going to be at least this big
climcnt <- 1
for(id in 1:dim(dftran)[1]){
  d <- dftran[id, ]
  okold <- which(unlist(lapply(climatology81, function(k) k[['transect']])) == d[['transect']] & 
                 unlist(lapply(climatology81, function(k) k[['season']])) == d[['season']] &
                 unlist(lapply(climatology81, function(k) k[['program']])) == d[['program']])
      
  oknew <- which(unlist(lapply(climatology91, function(k) k[['transect']])) == d[['transect']] & 
                   unlist(lapply(climatology91, function(k) k[['season']])) == d[['season']] &
                   unlist(lapply(climatology91, function(k) k[['program']])) == d[['program']])
  old <- climatology81[[okold]]
  new <- climatology91[[oknew]]
  if(!(is.na(old[['longitude0']]) | is.na(new[['longitude0']]))){
    nprofiles <- new[['avgProfiles']]
    oprofiles <- old[['avgProfiles']]
    newstns <- unlist(lapply(nprofiles, function(k) k[['stationName']]))
    oldstns <- unlist(lapply(oprofiles, function(k) k[['stationName']]))
    nprofilesWAnom <- vector(mode = 'list')
    # pdf(file = paste(destDirSuppFigures, 
    #                  paste0(paste('newClimatologyProfileComparison', d[['transect']], d[['season']], sep = '_'), ifelse(fillWithSmooth, '_Filled', ''), '.pdf'), sep = '/'),
    #     width = 6, height = 6)
    cnt <- 1
    for(is in 1:length(newstns)){
      okoldstn <- which(oldstns == newstns[is])
      if(length(okoldstn) == 0){
        cat(paste('A 1981 to 2010', d[['season']],'climatology',  
                  'was not created for the', d[['program']], 
                  d[['transect']], 'transect'), sep = '\n')
        next
      } else {
      okoldp <- oprofiles[[okoldstn[1]]][['pressure']] %in% nprofiles[[is]][['pressure']] # due to multiple HL_07's
      oknewp <- nprofiles[[is]][['pressure']] %in% oprofiles[[okoldstn]][['pressure']]
      # could probably be coded a bit better, but well, this is fine for now
      temperatureAnomaly <- nprofiles[[is]][['temperature']][oknewp] - oprofiles[[okoldstn]][['temperature']][okoldp]
      temperatureAnomaly[!oknewp] <- NA
      salinityAnomaly <- nprofiles[[is]][['salinity']][oknewp] - oprofiles[[okoldstn]][['salinity']][okoldp]
      salinityAnomaly[!oknewp] <- NA
      sigmaThetaAnomaly <- nprofiles[[is]][['sigmaThetaAvg']][oknewp] - oprofiles[[okoldstn]][['sigmaThetaAvg']][okoldp]
      sigmaThetaAnomaly[!oknewp] <- NA
      nprofilesWAnom[[cnt]] <- nprofiles[[is]]
      nprofilesWAnom[[cnt]] <- oceSetData(nprofilesWAnom[[cnt]],
                                'temperatureAnomaly',
                                temperatureAnomaly)
      nprofilesWAnom[[cnt]] <- oceSetData(nprofilesWAnom[[cnt]],
                                'salinityAnomaly',
                                salinityAnomaly)
      nprofilesWAnom[[cnt]] <- oceSetData(nprofilesWAnom[[cnt]],
                                'sigmaThetaAnomaly',
                                sigmaThetaAnomaly)
      # plots for comparison
      par(mfrow=c(3,2), oma = c(0, 0, 1.5, 0))
      Tlim <- range(unlist(lapply(c(nprofiles[is], oprofiles[okoldstn]), function(k) k[['temperature']])), na.rm = TRUE)
      Slim <- range(unlist(lapply(c(nprofiles[is], oprofiles[okoldstn]), function(k) k[['salinity']])), na.rm = TRUE)
      STlim <- range(unlist(lapply(c(nprofiles[is], oprofiles[okoldstn]), function(k) k[['sigmaThetaAvg']])), na.rm = TRUE)
      #plim <- rev(range(unlist(lapply(c(nprofiles[is], oprofiles[okoldstn]), function(k) k[['pressure']])), na.rm = TRUE))
      plim <- rev(new[['ylim']]) # what is plotted in the section plot
      # temperature
      plotProfile(ghostctd, xtype = 'temperature', Tlim = Tlim, plim = plim, col = 'white')
      lapply(nprofiles[is], function(k) lines(k[['temperature']], k[['pressure']], col = 'blue'))
      lapply(oprofiles[okoldstn], function(k) lines(k[['temperature']], k[['pressure']], col = 'red'))
      # salinity
      plotProfile(ghostctd, xtype = 'salinity', Slim = Slim, plim = plim, col = 'white')
      legend('bottomleft', col = c('blue', 'red'), lty = 1, legend = c('91', '81'))
      mtext(newstns[is], side = 3, outer = TRUE, line = 0)
      lapply(nprofiles[is], function(k) lines(k[['salinity']], k[['pressure']], col = 'blue'))
      lapply(oprofiles[okoldstn], function(k) lines(k[['salinity']], k[['pressure']], col = 'red'))
      # temperatureAnomaly
      plot(nprofilesWAnom[[cnt]][['temperatureAnomaly']], nprofilesWAnom[[cnt]][['pressure']], ylim = plim,
           xlab = '', ylab = '', mar = c(1.0,3.5, 3.5, 2.0))
      lines(nprofilesWAnom[[cnt]][['temperatureAnomaly']], nprofilesWAnom[[cnt]][['pressure']])
      grid()
      abline(v = 0)
      # salinityAnomaly
      plot(nprofilesWAnom[[cnt]][['salinityAnomaly']], nprofilesWAnom[[cnt]][['pressure']], ylim = plim,
           xlab = '', ylab = '', mar = c(1.0,3.5, 3.5, 2.0))
      lines(nprofilesWAnom[[cnt]][['salinityAnomaly']], nprofilesWAnom[[cnt]][['pressure']])
      grid()
      abline(v = 0)
      # sigmaTheta
      plotProfile(ghostctd, xtype = 'sigmaTheta', densitylim = STlim, plim = plim, col = 'white')
      lapply(nprofiles[is], function(k) lines(k[['sigmaThetaAvg']], k[['pressure']], col = 'blue'))
      lapply(oprofiles[okoldstn], function(k) lines(k[['sigmaThetaAvg']], k[['pressure']], col = 'red'))
      # T-S diagram
      plotTS(ghostctd, col = 'white', Tlim = Tlim, Slim = Slim)
      lapply(nprofiles[is], function(k) {lines(k[['salinity']], k[['temperature']], col = 'blue');
        points(k[['salinity']], k[['temperature']], col = 'blue')})
      lapply(oprofiles[okoldstn], function(k) {lines(k[['salinity']], k[['temperature']], col = 'red');
        points(k[['salinity']], k[['temperature']], col = 'red')})
      cnt <- cnt + 1
      }
    }
  #dev.off()
  
  # # create section and barnes interpolate now with anomalies
  s <- as.section(nprofilesWAnom)
  sg <- sectionGrid(s)
  factor <- ifelse(climatology91[[oknew]][['transect']] %in% c('halifaxInshore', 'halifaxExtended'), 2.5, 1)
  xr <- mround(median(diff(sg[['distance', 'byStation']]))/2, 5, type = 'ceiling') * factor
  yr <- 10
  xgrid <- seq(0, ceiling(max(sg[['distance', 'byStation']])) + 1.5*xr, by = xr/2)
  # have to add the is.na part to not interpolate farther down than measurements
  ygrid <- seq(5, ceiling(max(sg[['pressure']][!is.na(sg[['temperature']])])), by = yr/2)
  ygrid <- seq(5, ceiling(max(sg[['pressure']][!is.na(sg[['temperature']])])), by = yr)
  ss <- sectionSmooth(sg, method = 'barnes', xg = xgrid, yg = ygrid, xr = xr, yr = yr)
  
  climatology91WAnom[[climcnt]][['season']] <- climatology91[[oknew]][['season']]
  climatology91WAnom[[climcnt]][['transect']] <- climatology91[[oknew]][['transect']]
  climatology91WAnom[[climcnt]][['program']] <- climatology91[[oknew]][['program']]
  climatology91WAnom[[climcnt]][['longitude0']] <- climatology91[[oknew]][['longitude0']]
  climatology91WAnom[[climcnt]][['latitude0']] <- climatology91[[oknew]][['latitude0']]
  climatology91WAnom[[climcnt]][['ylim']] <- climatology91[[oknew]][['ylim']]
  climatology91WAnom[[climcnt]][['xlim']] <- climatology91[[oknew]][['xlim']]
  climatology91WAnom[[climcnt]][['avgProfiles']] <- nprofilesWAnom
  climatology91WAnom[[climcnt]][['section']] <- s
  climatology91WAnom[[climcnt]][['sectionSmooth']] <- ss
  climatology91WAnom[[climcnt]][['climatologyYears']] <- 'difference'
  climcnt <- climcnt + 1
  } # closes if there is a climatology for both periods
}

# remove the NULL values in climatology91WAnom
keep <- !unlist(lapply(climatology91WAnom, is.null))
climatology91WAnom <- climatology91WAnom[keep]

# combine the climatologies together again
climatology <- c(climatology81, climatology91, climatology91WAnom)
save(climatology, file = paste(destDirData, paste0('newClimatologyWAnomalies',ifelse(fillWithSmooth, 'Filled', ''),'.rda'), sep = '/'))

# # plot anomaly
# for(i in 1:length(climatology91WAnom)){
#   d <- climatology91WAnom[[i]]
#   Tlimanom <- range(unlist(lapply(d[['avgProfiles']], function(k) k[['temperatureAnomaly']])), na.rm = TRUE)
#   Slimanom <- range(unlist(lapply(d[['avgProfiles']], function(k) k[['salinityAnomaly']])), na.rm = TRUE)
#   STlimanom <- range(unlist(lapply(d[['avgProfiles']], function(k) k[['sigmaThetaAnomaly']])), na.rm = TRUE)
# }