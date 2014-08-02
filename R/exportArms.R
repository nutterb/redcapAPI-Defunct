exportArms <- function(rcon, arms, ...) UseMethod("exportArms")

exportArms.redcapDbConnection <- function(rcon, arms, ...){
  message("Please accept my apologies.  The exportUsers method for redcapDbConnection objects\n",
          "has not yet been written.  Please consider using the API.")
}

exportArms.redcapApiConnection <- function(rcon, arms, ...){
  .params <- list(token=rcon$token, content='arm', format='csv')
  if (!missing(arms)) .params[['arms']] <- paste(arms, collapse=',')
  x <- tryCatch(postForm(uri=rcon$url,.params=.params,
                         .opts=curlOptions(ssl.verifyhost=FALSE)),
                error = function(cond) if (grepl("Bad Request", cond[1])) return("No arms returned"))
  
  if (x != "No arms returned") x <- read.csv(textConnection(x), stringsAsFactors=FALSE)
  return(x)
}
