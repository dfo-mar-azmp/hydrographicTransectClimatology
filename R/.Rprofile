# create new environment for [un]visibility of local variables
my.env <- new.env()
host <- NULL
port <- NULL
sid <- NULL
username <- NULL
password <- NULL
check <- list(host = host,
              port = port,
              sid = sid,
              username = username,
              password = password)
if(any(is.null(unlist(check)))){
  cat("A required field for connecting to database is missing", sep = '\n')
  cat(paste("Please add non NULL values for ", paste(names(check)[which(unlist(lapply(check, is.null)))], collapse = ', ')), sep = '\n')
}
# set up data source, username and password for database access
assign("host", host, envir=my.env)
assign("port", port, envir=my.env)
assign("sid", side, envir=my.env)
assign("username", username, envir=my.env)
assign("password", password, envir=my.env)
