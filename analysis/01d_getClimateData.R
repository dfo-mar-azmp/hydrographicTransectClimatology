rm(list = ls())
source('00_setupFile.R')

# source functions to extract from database
source("../R/.Rprofile")
source("../R/Run_Database_Query.R")
source("../R/Write_Database_Table.R")
source("../R/Remove_Database_Table.R")
source("../R/Read_SQL.R")

# get the data
sql_file <- "extractAZMPdatesFromClimate.sql"
data <- Run_Database_Query(sql_file = sql_file, 
                           host = my.env$host, 
                           port = my.env$port, 
                           sid = my.env$sid, 
                           username = my.env$username, 
                           password = my.env$password)

# remove temporary table
Remove_Database_Table("TMP", my.env$host, my.env$port, my.env$sid, my.env$username, my.env$password)

sql_str <- data[[1]]
df_data <- data[[2]]

# save to .csv file
names(df_data) <- tolower(names(df_data))
filename <- paste(destDirData, paste0("climate_",format(Sys.Date(), "%Y%m%d"),".csv"), sep = '/')
write.csv(df_data, file = filename, row.names = FALSE)