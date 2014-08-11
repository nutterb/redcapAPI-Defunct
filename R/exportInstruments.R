exportInstruments <- function(rcon, ...) UseMethod("exportInstruments")

exportInstruments.redcapDbConnection <- function(rcon, ...){
  message("Please accept my apologies.  The exportInstruments method for redcapDbConnection objects\n",
          "has not yet been written.  Please consider using the API.")
}

exportInstruments.redcapApiConnection <- function(rcon, ..., proj=NULL){
  v <- if (is.null(proj$version)) exportVersion(rcon) else proj$version
  if (compareRedcapVersion(proj$version, "5.9.0") == -1) 
    return("'exportInstruments' requires REDCap version 5.9.0 or higher")
  x <- httr::POST(url=rcon$url, body=list(token=rcon$token, content='instrument', format='csv'))
  if (x$status_code == "200")
    read.csv(textConnection(as.character(x)), stringsAsFactors=FALSE) 
  else stop(paste(x$status_code, ": ", as.character(x), sep=""))
}
