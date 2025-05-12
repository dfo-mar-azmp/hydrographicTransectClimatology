#' Function to read a sql query from a text file.
#' 
#' @param sql_file sql filename to be read
#' @return sql text string
#' @author Benoit Casault \email{Benoit.Casault@dfo-mpo.gc.ca}
Read_SQL <- function(sql_file) {
  # Last update: 20141015
  
  # read sql query
  sql <- scan(file=sql_file, what=ls(), sep="\n")
  sql <- paste(sql, collapse=" ")

  return(sql)
}