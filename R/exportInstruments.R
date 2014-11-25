exportInstruments <- function(rcon, ...) UseMethod("exportInstruments")

exportInstruments.redcapDbConnection <- function(rcon, ...){
  message("Please accept my apologies.  The exportInstruments method for redcapDbConnection objects\n",
          "has not yet been written.  Please consider using the API.")
}

exportInstruments.redcapApiConnection <- function(rcon, ...){
  x <- httr::POST(url=rcon$url, 
                  body=list(token=rcon$token, content='instrument', format='csv'),
                  config=rcon$config)
  if (x$status_code == "200")
    read.csv(textConnection(as.character(x)), stringsAsFactors=FALSE) 
  else if (x$status_code == "400" & as.character(x) == "The value of the parameter \"content\" is not valid") 
    paste0(x$status_code, ": ", as.character(x))
  else stop(paste0(x$status_code, ": ", as.character(x)))
}
