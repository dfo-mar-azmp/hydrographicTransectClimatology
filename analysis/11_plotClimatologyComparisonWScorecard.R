rm(list=ls())
fillWithSmooth <- TRUE # logical that decides if missing stations filled with smoothed climatology profiles should be used
library(oce)
library(csasAtlPhys)
library(cmocean)
data("transectPlotLimits")
plotProfile <- oce::plotProfile
topoFile <- download.topo(west = -75, east = -50,
                          south = 38, north = 50,
                          resolution = 1)
ocetopo <- read.topo(topoFile)
ocetopo[['z']] <- ocetopo[['z']]*-1 # to work with `plot,section-method` `showBottom = ocetopo` param
source('00_setupFile.R')
if(fillWithSmooth){
  load(paste(destDirData, 'newClimatologyWAnomaliesFilled.rda', sep = '/'))
} else {
  load(paste(destDirData, 'newClimatologyWAnomalies.rda', sep = '/'))
}


df <- data.frame(transect = unlist(lapply(climatology, function(k) k[['transect']])),
                 season = unlist(lapply(climatology, function(k) k[['season']])),
                 program = unlist(lapply(climatology, function(k) k[['program']])),
                 climStartYear = unlist(lapply(climatology, function(k) min(k[['climatologyYears']]))),
                 climEndYear = unlist(lapply(climatology, function(k) max(k[['climatologyYears']]))))
dftran <- unique(df[ , names(df) %in% c('transect', 'season', 'program')])

vars <- list(c('temperature', 'salinity', 'sigmaThetaAvg'),
             c('temperature', 'salinity', 'sigmaThetaAvg'),
             c('temperatureAnomaly', 'salinityAnomaly', 'sigmaThetaAnomaly'))

for(it in 1:dim(dftran)[1]){
  ok <- df[['transect']] %in% dftran[['transect']][it] & df[['season']] %in% dftran[['season']][it] & df[['program']] %in% dftran[['program']][it]
  d <- climatology[ok]
  keep <- unlist(lapply(d, function(k) !is.na(k[['longitude0']])))
  d <- d[keep]
  if(length(d) == 0){
    cat(paste('No climatology created for the', dftran[['season']][it],
              dftran[['program']][it], dftran[['transect']][it], 'transect.'), sep = '\n')
    next
  } else {
    png(filename = paste(destDirFigures, 
                         paste0(paste('11_climatologyComparison', 
                                      dftran[['transect']][it], 
                                      dftran[['season']][it], 
                                      dftran[['program']][it], sep = '_'), ifelse(fillWithSmooth, '_Filled', ''), '.png'), sep = '/'),
        width = 11, height = ifelse(length(d) == 3, 6, 2.8), units = 'in',
        res = 200, pointsize = 10)
    mfrow <- c(length(d),3)
    oma <- c(2, 2, 2, 0)
    mar <- c(1.5, 3.5, 1.5, 2)
    par(mfrow = mfrow, mar = mar, oma = oma)
    for(i in 1:ifelse(length(d) == 3, 2, length(d))){
      s <- d[[i]][['sectionSmooth']]
      sctd <- d[[i]][['avgProfiles']]
      lon0 <- d[[i]][['longitude0']]
      lat0 <- d[[i]][['latitude0']]
      ylims <- d[[i]][['ylim']]
      xlims <- d[[i]][['xlim']]
      transectName <- d[[i]][['transect']]
      climYear <- d[[i]][['climatologyYears']][[1]] # use min year, but special case for difference
      
      
      # for use by contour, resulting barnes interp 'stations'
      clx <- geodDist(longitude1 = s[['longitude', 'byStation']], 
                      latitude1 = s[['latitude', 'byStation']],
                      longitude2 = lon0 , latitude2 = lat0)
      clxd <- geodDist(longitude1 = unlist(lapply(sctd, function(k) k[['longitude']][1])),
                       latitude1 = unlist(lapply(sctd, function(k) k[['latitude']][1])),
                       longitude2 = lon0,
                       latitude2 = lat0)
      cly <- s[['station',1]][['pressure']]
      # for anomaly scorecard plot, add half of the diff between last two stations
      xlims[2] <- xlims[2] + diff(clxd[(length(clxd)-1):length(clxd)])/2
      #xlim <- c(0, max(clxd))
      # set up for plotting
      dataNames <- names(sctd[[1]]@data)
      
      for(var in vars[[i]]){
        if(!(var %in% dataNames) & var != 'sigmaTheta'){
          next
        } else {
          # set up various plotting parameters
          zlim <- transectPlotLimits[['limits']][[var]]
          levels <- transectPlotLimits[['contourLevels']][[var]]
          levelLimits <- transectPlotLimits[['contourLevelLimits']][[var]]
          if(var == 'temperatureAnomaly'){
            zlim <- range(seq(-7,7,1)/8)
            levels <- levels/8
            levelLimits <- levelLimits/8
          }
          if(var == 'salinityAnomaly' | var == 'sigmaThetaAnomaly'){
            zlim <- range(anomalyColors$breaks[anomalyColors$breaks < 4])/4
            levels <- levels/4
            levelLimits <- levelLimits/4
          }
          if(var == 'sigmaThetaAvg'){
            levels <- transectPlotLimits[['contourLevels']][['sigmaTheta']]
            levelLimits <- transectPlotLimits[['contourLevelLimits']][['sigmaTheta']]
          }
          if(var == 'sigmaThetaAnomaly'){
            zlim <- range(anomalyColors$breaks[anomalyColors$breaks < 4])/4
          }
          axes <- switch(var,
                         'temperature' = FALSE,
                         'temperatureAnomaly' = TRUE,
                         'salinity' = FALSE,
                         'salinityAnomaly' = TRUE,
                         'sigmaThetaAvg' = FALSE,
                         'sigmaThetaAnomaly' = TRUE)
          axes <- ifelse(i == length(d), TRUE, FALSE)
          title <- switch(var,
                          'temperature' = TRUE,
                          'temperatureAnomaly' = FALSE,
                          'salinity' = FALSE,
                          'salinityAnomaly' = FALSE,
                          'sigmaThetaAvg' = FALSE,
                          'sigmaThetaAnomaly' = FALSE)
          R <- ']'
          L <- '['
          zlab <- switch(var,
                         'temperature'= bquote(bold(.(gettext('Temperature', domain = 'R-oce')) * .(L) * degree * "C" * .(R))),
                         'temperatureAnomaly' = getAnomalyLabel('temperatureAnomaly', bold = TRUE),
                         'salinity' = bquote(bold(.(gettext('Practical Salinity', domain = 'R-oce')))),
                         'salinityAnomaly' = getAnomalyLabel('salinityAnomaly', bold = TRUE),
                         'sigmaThetaAvg' = bquote(bold(sigma[theta] *' '* .(L) * kg/m^3 * .(R))),
                         'sigmaThetaAnomaly' = getAnomalyLabel('sigmaThetaAnomaly', bold = TRUE))
          zcol <- switch(var,
                         'temperature' = cmocean::cmocean("thermal"),
                         'temperatureAnomaly'= head(anomalyColors$colors, -4),
                         'salinity' = cmocean::cmocean("haline"),
                         'salinityAnomaly' = head(anomalyColors$colors, -4),
                         'sigmaThetaAvg' = cmocean::cmocean("dense"),
                         'sigmaThetaAnomaly' = head(anomalyColors$colors, -4))
          zbreaks <- switch(var,
                            'temperature'= NULL, #seq(Tlim[1], Tlim[2],1),
                            'temperatureAnomaly'= seq(-7,7,1)/8,
                            'salinity'= NULL, #seq(Slim[1], Slim[2]),
                            'salinityAnomaly' = head(anomalyColors$breaks, -4)/4,
                            'sigmaThetaAvg' = NULL, #seq(STlim[1], STlim[2],1),
                            'sigmaThetaAnomaly' = head(anomalyColors$breaks, -4)/4)
          
          par(cex = 0.8)
          {if(!axes){
            plot(s, which = var, ztype = 'image', 
                 ylim = rev(ylims), zlim = zlim, xlim = xlims,
                 zcol = zcol, zbreaks = zbreaks,
                 showBottom = FALSE,
                 legend.loc = '',
                 axes = axes, xlab = '', mar = mar,
                 longitude0 = lon0, latitude0 = lat0)
          } else{
            plot(s, which = var, ztype = 'image', 
                 ylim = rev(ylims), zlim = zlim, xlim = xlims,
                 zcol = zcol, zbreaks = zbreaks,
                 showBottom = FALSE,
                 legend.loc = '',
                 mar = mar, stationTicks = FALSE,
                 longitude0 = lon0, latitude0 = lat0)
          }
          }
          clz <- matrix(s[[var]], byrow = TRUE, nrow = length(s[['station']]))
          contour(clx, cly, clz, levels = levels[levels > levelLimits[1] & levels < levelLimits[2] ], 
                  col = 'black', add = TRUE, #labcex = 1, 
                  vfont = c('sans serif', 'bold'),
                  xlim = xlims, ylim = ylim)
          contour(clx, cly, clz, levels = levels[levels <= levelLimits[1] | levels >= levelLimits[2]],
                  col = 'white', add = TRUE, #labcex = 1, 
                  vfont = c('sans serif', 'bold'),
                  xlim = xlims, ylim = ylim)
          # get the bottom that `plot, section-method` created (pulled from oce/sections.R)
          # some modifications were made to work with my code/variable names
          topoResolution <- geodDist(0, 0, 0, diff(ocetopo[["latitude"]][1:2]))
          slon <- c(lon0, s[["longitude", "byStation"]])
          slat <- c(lat0, s[["latitude", "byStation"]])
          sectionSpan <- geodDist(min(slon, na.rm=TRUE), min(slat, na.rm=TRUE),
                                  max(slon, na.rm=TRUE), max(slat, na.rm=TRUE))
          nin <- length(slon)
          ## double up on resolution, although perhaps not needed
          nout <- as.integer(1 + 2 * sectionSpan / topoResolution)
          blon <- approx(1:nin, slon, n=nout)$y
          blat <- approx(1:nin, slat, n=nout)$y
          bottom.y <- topoInterpolate(blon, blat, ocetopo) * -1
          bottom.x <- approx(1:nin, c(0, clx), n=nout)$y
          bottom.x <- c(bottom.x[1], bottom.x, tail(bottom.x, 1))
          usr3 <- par('usr')[3] * -1
          bottom.y <- c(usr3, bottom.y, usr3)
          polygon(bottom.x, bottom.y * -1, col="grey49")
          #polygon(bottom$xpoly, bottom$ypoly, col = 'grey49')
          plotStationLocations(distance = clxd, plabel = -10)
          legend('bottomleft', legend = zlab, 
                 bg = 'n', bty = 'n',
                 text.col = 'white', cex = 1)
          # add x and y-axis
          {if(!axes){
            axis(side = 1, at = pretty(xlims), labels = FALSE) # not working for browns bank
            axis(side = 2, at = pretty(ylims))
            
          } else{
            axis(side = 1, at = pretty(xlims), labels = FALSE, tcl = -0.01)
            axis(side = 2, at = pretty(ylims), labels = FALSE)
          }
          }
          #axis(side = 3, at = clx, labels = FALSE, tcl = -0.01)
          # add y-axis on other side
          axis(side = 4, at = pretty(ylims), labels = FALSE, line = -3.45)
          # # add title
          # if(title & i == 1){
          #   titlet <- paste(getLocationName(dftran[['transect']][it]), dftran[['season']][it], sep = ' , ')
          #   mtext(titlet, side = 3, font = 2, cex = 1, outer = TRUE, line = 0)
          # }
          # add station number on top of triangles
          if(i == 1){
            if(transectName == 'halifaxInshore'){
              line <- rep(0.5, length(sctd))
              line[seq(2, length(sctd), 2)] <- 1.2
            } else {
              line <- 0.5
            }
            mtext(at = clxd, text = gsub('^\\w+\\_(\\d+)', '\\1', unlist(lapply(sctd, '[[', 'stationName'))),
                  line = line, cex = 4/5)
          }
          if(var %in% c('temperature', 'temperatureAnomaly')){
            mtext(text = ifelse(climYear == 'difference', '1991 - 1981', climYear),
                  side = 2, line = 3.5, font = 2)
          }
        } # closes else part of if statement
      } # closes variable
    } # closes i
    # construct scorecard of differences
    if(length(d) != 3){
      dev.off()
      next
    }
    i <- 3
    s <- d[[i]][['sectionSmooth']]
    sctd <- d[[i]][['avgProfiles']]
    lon0 <- d[[i]][['longitude0']]
    lat0 <- d[[i]][['latitude0']]
    
    # calculate x, distance along section
    x <- geodDist(longitude1 = unlist(lapply(sctd, function(k) k[['longitude']][1])), 
                    latitude1 = unlist(lapply(sctd, function(k) k[['latitude']][1])),
                    longitude2 = lon0 , latitude2 = lat0)
    # define y, unique pressure values
    allP <- unlist(lapply(sctd, '[[', 'pressure'))
    y <- unique(allP)
    # now iterate through each variable
    for(var in vars[[i]]){
      # define z and fill matrix with data
      z <- matrix(data = NA, nrow = length(y), ncol = length(x))
      for(is in 1:length(sctd)){
        pidx <- unlist(lapply(sctd[[is]][['pressure']], function(k) which(y == k)))
        z[pidx, is] <- sctd[[is]][[var]]
      }
      cm <- colormap(z = z, 
                     zlim = max(abs(z), na.rm = TRUE) * c(-1, 1), 
                     col = cmocean('balance'),
                     missingColor = 'grey49')
      imagep(x = x, y = y, z = t(z),
             colormap = cm, 
             ylim = rev(ylims), xlim = xlims,
             axes = FALSE, drawPalette=TRUE, mar = mar)
      box()
      # add station locations
      plotStationLocations(distance = clxd, plabel = -10)
      # add y-axis
      axis(2)
      mtext(text = resizableLabel('depth'), side = 2, line = 2, cex = 4/5)
      axis(side = 4, labels = FALSE)
      # add x-axis
      axis(1)
      mtext(text = 'Distance [km]', side = 1, line = 2, cex = 4/5)
      #axis(1, at = x, labels = gsub('^\\w+\\_(\\d+)', '\\1', unlist(lapply(sctd, '[[', 'stationName'))))
      
    }
    dev.off()
  }
}
