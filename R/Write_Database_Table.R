#' Function to write data from a database (e.g. Biochem).  The function  opens a
#' connection to a database, writes the data with a table name to the database.
#' @param data data as a dataframe to be written to a database
#' @param table_name name of table to be written to database
#' @param host host name for database
#' @param port port for database
#' @param sid sid for database
#' @param username personal username to access database
#' @param password personal password associated with username to access database
#' @return data which is a list of two elements, data[[1]] is the sql code for the query in
#' a string and data[[2]] is the data extracted from the database in a dataframe
#' @author Benoit Casault \email{Benoit.Casault@dfo-mpo.gc.ca}
Write_Database_Table <- function(data, table_name, host, port, sid, username, password) {
  # Last update: 20141015

  # load odbc library
  library(ROracle)
  
  # create an Oracle Database instance and create connection
  drv <- dbDriver("Oracle")
  
  # connect string specifications
  connect.string <- paste(
    "(DESCRIPTION=",
    "(ADDRESS=(PROTOCOL=tcp)(HOST=", host, ")(PORT=", port, "))",
    "(CONNECT_DATA=(SID=", sid, ")))", sep = "")

  # use username/password authentication
  conn <- ROracle::dbConnect(drv, username=username, password=password, dbname = connect.string)
  
  ## check if table already exixts
  tf <- dbExistsTable(conn, name=table_name, schema = NULL)
  if(tf){
    status <- dbRemoveTable(conn, name=table_name, purge=TRUE, schema=NULL)
  }
  
  ## write data to table
  status <- dbWriteTable(conn, name=table_name, value=data, row.names=FALSE, overwrite=TRUE,
                         append=FALSE, ora.number=TRUE, schema=NULL)
    
  # close database connection
  conn <- dbDisconnect(conn)

  return()
}
