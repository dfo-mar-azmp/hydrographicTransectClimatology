library(csasAtlPhys)
# check if it exists (might be defined in other scripts that are sourcing it)
if(!exists('makeDirs')){
  makeDirs <- TRUE
}
# load file that defines arcPath
## define file
pathToArcFile <- ifelse(basename(getwd()) == 'analysis', './', '../analysis') # to avoid errors when building report
arcPathFile <- paste(pathToArcFile, '00_arcPath.R', sep = '/')
## function to check
checkArcPath <- function(file){
  if(file.exists(file)){
    source(file)
    if(!exists('arcPath')){
      stop(paste("Please define 'arcPath' in", file))
    }
  } else {
    stop(paste(file, 'does not exist.',
               "Please create the file and define 'arcPath'.",
               "See README.md for details."))
  }
}
## check
checkArcPath(file = arcPathFile)
# set climatology ranges for various programs
fakeYear <- 2020
# azmp
## spring
df <- data.frame(program = 'azmp',
                 season = 'spring',
                 start = as.Date(paste0(fakeYear, "-04-01")),
                 end = as.Date(paste0(fakeYear, "-05-15")))
## fall
dfadd <- data.frame(program = 'azmp',
                    season = 'fall',
                    start = as.Date(paste0(fakeYear, '-09-15')), 
                    end = as.Date(paste0(fakeYear, '-10-31')))
df <- rbind(df, dfadd)

# check if directory for saving various data files has been created
destDirData <- './data'
destDirFigures <- './figures'
destDirNetCDF <- './netCDFclimatology'

dirsToMake <- c(destDirData,
                destDirFigures,
                destDirNetCDF)
if(makeDirs){
  for(i in 1:length(dirsToMake)){
    if(!dir.exists(dirsToMake[i])) dir.create(dirsToMake[i], recursive = TRUE)
  }
}


# some useful things that will be used in multiple scripts when looking for data.

# these files will cause issues due to their long header
badfiles <- c('\\\\ent.dfo-mpo.ca/ATLShares/Science/BIODataSvc/ARC/Archive/ctd/2006/CTD_TEL2006615_092_288086_DN.ODF',
              '\\\\ent.dfo-mpo.ca/ATLShares/Science/BIODataSvc/ARC/Archive/ctd/2003/CTD_NED2003003_000_258853_DN.ODF',
              '\\\\ent.dfo-mpo.ca/ATLShares/Science/BIODataSvc/ARC/Archive/ctd/2004/CTD_TEM2004004_088_263653_DN.ODF',
              '\\\\ent.dfo-mpo.ca/ATLShares/Science/BIODataSvc/ARC/Archive/ctd/2013/CTD_HUD2013037_042_1_DN.ODF',
              "\\\\ent.dfo-mpo.ca/ATLShares/Science/BIODataSvc/ARC/Archive/ctd/2015/CTD_HUD2015030_127_1_DN.ODF",
              "\\\\ent.dfo-mpo.ca/ATLShares/Science/BIODataSvc/ARC/Archive/ctd/1996/CTD_96999_003_001_DN.ODF", # variable names bad
              "\\\\ent.dfo-mpo.ca/ATLShares/Science/BIODataSvc/ARC/Archive/ctd/1996/CTD_96999_003_002_DN.ODF" # variable names bad
              )
# load all the transect polygons and put them together
data("brownsBankPolygon")
data("northeastChannelPolygon")
data("halifaxPolygon")
data("louisbourgPolygon")
data("cabotStraitPolygon")
data("stAnnsBankPolygon")
data("yarmouthPolygon")
data("portsmouthPolygon")
data("laurentianChannelMouthPolygon")
data("stPierreBankPolygon")
data("sableIslandBankPolygon")
data("laHaveBankPolygon")
data("rosewayPolygon")
data("theGullyPolygon")


polygons <- list(brownsBank = brownsBankPolygon,
                 northEastChannel = northeastChannelPolygon,
                 halifaxInshore = halifaxPolygon,
                 louisbourg = louisbourgPolygon,
                 cabotStrait = cabotStraitPolygon,
                 stAnnsBank = stAnnsBankPolygon,
                 yarmouth = yarmouthPolygon,
                 portsmouth = portsmouthPolygon,
                 laurentianChannelMouth = laurentianChannelMouthPolygon,
                 stPierreBank = stPierreBankPolygon,
                 sableIslandBank = sableIslandBankPolygon,
                 laHaveBank = laHaveBankPolygon,
                 roseway = rosewayPolygon,
                 theGully = theGullyPolygon)

# load all of the station polygons and put them together
data("brownsBankStationPolygons")
data("northeastChannelStationPolygons")
data("halifaxStationPolygons")
data("louisbourgStationPolygons")
data("cabotStraitStationPolygons")
data("stAnnsBankStationPolygons")
data("yarmouthStationPolygons")
data("portsmouthStationPolygons")
data("laurentianChannelMouthStationPolygons")
data("stPierreBankStationPolygons")
data("sableIslandBankStationPolygons")
data("laHaveBankStationPolygons")
data("rosewayStationPolygons")
data("theGullyStationPolygons")

stations <- c(brownsBankStationPolygons,
              northeastChannelStationPolygons,
              halifaxStationPolygons,
              louisbourgStationPolygons,
              cabotStraitStationPolygons,
              stAnnsBankStationPolygons,
              yarmouthStationPolygons,
              portsmouthStationPolygons,
              laurentianChannelMouthStationPolygons,
              stPierreBankStationPolygons,
              sableIslandBankStationPolygons,
              laHaveBankStationPolygons,
              rosewayStationPolygons,
              theGullyStationPolygons)