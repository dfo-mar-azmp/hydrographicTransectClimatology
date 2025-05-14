# create new environment for [un]visibility of local variables
my.env <- new.env()
databaseInfoFile <- '00_databaseInfo.R'
checkDatabaseFile <- function(file){
  checkvars <- c('host', 'port', 'sid', 'username', 'password')
  if(file.exists(file)){
    source(file)
    varExists <- unlist(lapply(checkvars, exists))
    if(any(!varExists)){
      stop(paste("A required field for connecting to database is missing.",
                 "Please add values for", 
                 paste(checkvars[!varExists], collapse = ', '),
                 "to",
                 file)
      )
    }
  } else {
    stop(paste(file, "does not exist,", 
         "please create the file and define variables",
         paste("'", checkvars, "'", sep = "", collapse = ', '),
         ". See README.md for details."))
  }
}
checkDatabaseFile(file = databaseInfoFile)

# set up data source, username and password for database access
assign("host", host, envir=my.env)
assign("port", port, envir=my.env)
assign("sid", sid, envir=my.env)
assign("username", username, envir=my.env)
assign("password", password, envir=my.env)
