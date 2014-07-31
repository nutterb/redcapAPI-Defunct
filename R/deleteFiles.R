deleteFiles <- function(rcon, record, field, event, ...)
  UseMethod("deleteFiles")

deleteFiles.redcapDbConnection <- function(rcon, record, field, event, ...){
  message("Please accept my apologies.  The deleteFiles method for redcapDbConnection objects\n",
          "has not yet been written.  Please consider using the API.")
}

deleteFiles.redcapApiConnection <- function(rcon, record, field, event, ...){
  #* stop the function if arguments do not specify a unique record-event
  if (any(sapply(list(record, field, event), length) > 1)){
    stop("The arguments 'record', 'field', and 'event' may each only have length 1")
  }
  
  .params <- list(token=rcon$token, content='file',
                  action='delete', record=record,
                  field=field)
  if (!missing(event)) .params[['event']] <- event
  
  #* Delete the file
  postForm(uri=rcon$url, .params=.params,
           .opts=curlOptions(ssl.verifyhost=FALSE))
  
  message("The file was successfully deleted")
}
