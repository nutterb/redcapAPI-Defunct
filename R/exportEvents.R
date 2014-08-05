exportEvents <- function(rcon, arms, ...) UseMethod("exportEvents")

exportEvents.redcapDbConnection <- function(rcon, arms, ...){
  message("Please accept my apologies.  The exportUsers method for redcapDbConnection objects\n",
          "has not yet been written.  Please consider using the API.")
}

exportEvents.redcapApiConnection <- function(rcon, arms, ...){
  .params <- list(token=rcon$token, content='event', format='csv', returnFormat='csv')
  if (!missing(arms)) .params[['arms']] <- paste(arms, collapse=',')
  
  x <- httr::POST(url=rcon$url, body=.params)
  if (x$status_code == "200")
    read.csv(textConnection(as.character(x)), stringsAsFactors=FALSE) 
  else if (x$status_code == "400" & as.character(x) == "You cannot export events for classic projects") 
    paste(x$status_code, ": ", as.character(x), sep="")
  else stop(paste(x$status_code, ": ", as.character(x), sep=""))
}
