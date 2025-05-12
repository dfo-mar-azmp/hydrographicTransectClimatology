#' Function to remove a table from a database (e.g. Biochem).  The function  opens a
#' connection to a database and removes the table from the database.
#' @param table_name name of table to be removed from database
#' @param host host name for database
#' @param port port for database
#' @param sid sid for database
#' @param username personal username to access database
#' @param password personal password associated with username to access database
#' @return data which is a list of two elements, data[[1]] is the sql code for the query in
#' a string and data[[2]] is the data extracted from the database in a dataframe
#' @author Benoit Casault \email{Benoit.Casault@dfo-mpo.gc.ca}
Remove_Database_Table <- function(table_name, host, port, sid, username, password) {

  # Function to ...
  #
  # Input: table_name : name of table to remove - string
  #        ....
  #        data_source : data source name for database - string
  #        username : username - string
  #        password: user password - string
  #
  # Last update: 20141015
  # Benoit.Casault@dfo-mpo.gc.ca
  
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
  
  ## check if table exists
  tf <- dbExistsTable(conn, name=table_name, schema = NULL)
  if(tf){
    status <- dbRemoveTable(conn, name=table_name, purge=TRUE, schema=NULL)
  }
    
  # close database connection
  conn <- dbDisconnect(conn)

  return()
}
