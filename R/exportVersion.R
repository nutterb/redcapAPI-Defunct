exportVersion <- function(rcon, ...) UseMethod("exportVersion")

exportVersion.redcapDbConnection <- function(rcon, ...){
  message("Please accept my apologies.  The exportVersion method for redcapDbConnection objects\n",
          "has not yet been written.  Please consider using the API.")
}

exportVersion.redcapApiConnection <- function(rcon, ...){
  .params <- list(token=rcon$token, content='version', returnFormat='csv')
  x <- httr::POST(url=rcon$url, body=.params, config=rcon$config)
  
  if (x$status_code == "200")
    return(as.character(x$content))
  #*** When this API function isn't available (ie, before version 6.0),
  #*** we want to avoid throwing a disruptive error. Instead, we 
  #*** return the message that indicates the version is unknown.
  else if (as.character(x) ==  "The value of the parameter \"content\" is not valid") return("Version Unknown")
  else return(as.character(x))
}
