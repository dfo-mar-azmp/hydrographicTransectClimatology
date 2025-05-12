#' Function to extract data from a database (e.g. Biochem).  The function reads a sql query, opens a
#' connection to a database, extracts the data and returns the extracted data and the associated
#' sql code.
#' @param sql_file sql filename to be read
#' @param sql_file_lines sql code that may have been constructed or already read in using
#' Read_SQL.
#' @param host host name for database
#' @param port port for database
#' @param sid sid for database
#' @param username personal username to access database
#' @param password personal password associated with username to access database
#' @return data which is a list of two elements, data[[1]] is the sql code for the query in
#' a string and data[[2]] is the data extracted from the database in a dataframe
#' @author Benoit Casault \email{Benoit.Casault@dfo-mpo.gc.ca}
Run_Database_Query <- function(sql_file = NULL, sql_file_lines = NULL, host, port, sid, username, password) {
  # Last update: 20141015
  
  # load odbc library
  library(ROracle)
  
  # source custom functions
  # source("./R/Read_SQL.R") # 20180809 source in R file not here due to directory restructuring
  
  # declare empty list to store outputs
  data <- list()
  
  if(!is.null(sql_file) & !is.null(sql_file_lines)){
    message('arguments sql_file and sql_files_lines have both been provided
            using sql_file over sql_file_lines')
  }
  # read sql file
  if(!is.null(sql_file)){
    data[[1]] <- Read_SQL(sql_file)
  }
  if(!is.null(sql_file_lines)){
    data[[1]] <- sql_file_lines
  }

  
  # create an Oracle Database instance and create connection
  drv <- dbDriver("Oracle")
  
  # connect string specifications
  connect.string <- paste(
    "(DESCRIPTION=",
    "(ADDRESS=(PROTOCOL=tcp)(HOST=", host, ")(PORT=", port, "))",
    "(CONNECT_DATA=(SID=", sid, ")))", sep = "")
  
  # use username/password authentication
  conn <- ROracle::dbConnect(drv, username=username, password=password, dbname = connect.string)
  
  # run SQL statement by creating first a resultSet object
  rs <- dbSendQuery(conn, data[[1]])
  
  # fetch records from the resultSet into a dataframe
  data[[2]] <- fetch(rs)
  
  # close database connection
  conn <- dbDisconnect(conn)
  
  return(data)
}
