exportMappings <- function(rcon, arms, ...) UseMethod("exportMappings")

exportMappings.redcapDbConnection <- function(rcon, arms, ...){
  message("Please accept my apologies.  The exportMappings method for redcapDbConnection objects\n",
          "has not yet been written.  Please consider using the API.")
}

exportMappings.redcapApiConnection <- function(rcon, arms, ...){
  .params <- list(token=rcon$token, content='formEventMapping', format='csv')
  if (!missing(arms)) .params[['arms']] <- paste(arms, collapse=',')
  x <- httr::POST(url=rcon$url, body=.params)
  
  if (x$status_code == "200")
    return(read.csv(textConnection(as.character(x)), stringsAsFactors=FALSE))
  else (stop(paste(x$status_code, ": ", as.character(x), sep="")))
}
