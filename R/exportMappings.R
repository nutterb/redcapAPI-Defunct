exportMappings <- function(rcon, arms, ...) UseMethod("exportMappings")

exportMappings.redcapDbConnection <- function(rcon, arms, ...){
  message("Please accept my apologies.  The exportMappings method for redcapDbConnection objects\n",
          "has not yet been written.  Please consider using the API.")
}

exportMappings.redcapApiConnection <- function(rcon, arms, ...){
  .params <- list(token=rcon$token, content='formEventMapping', format='csv')
  if (!missing(arms)) .params[['arms']] <- paste(arms, collapse=',')
  x <- postForm(uri=rcon$url,.params=.params,
                .opts=curlOptions(ssl.verifyhost=FALSE))
  x <- read.csv(textConnection(x), stringsAsFactors=FALSE)
  return(x)
}
