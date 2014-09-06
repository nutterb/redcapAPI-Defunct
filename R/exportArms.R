exportArms <- function(rcon, arms, ...) UseMethod("exportArms")

exportArms.redcapDbConnection <- function(rcon, arms, ...){
  message("Please accept my apologies.  The exportUsers method for redcapDbConnection objects\n",
          "has not yet been written.  Please consider using the API.")
}

exportArms.redcapApiConnection <- function(rcon, arms, ...){
  .params <- list(token=rcon$token, content='arm', format='csv', returnFormat='csv')
  if (!missing(arms)) .params[['arms']] <- paste(arms, collapse=',')
  x <- httr::POST(url=rcon$url, body=.params, config=rcon$config)
  
  if (x$status_code == "200")
    return(read.csv(textConnection(as.character(x)), stringsAsFactors=FALSE))
  #*** For classic projects, we want to avoid throwing a disruptive error. Instead, we 
  #*** return the message that indicates this is a classic project.
  else if (x$status_code == "400" & as.character(x) == "You cannot export arms for classic projects") 
    paste(x$status_code, ": ", as.character(x), sep="")
  else stop(paste(x$status_code, ": ", as.character(x), sep=""))
}
