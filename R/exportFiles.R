exportFiles <- function(rcon, records, field, event, dir, filePrefix, ...)
  UseMethod("exportFiles")

exportFiles.redcapDbConnection <- function(rcon, records, field, event=NULL, dir, filePrefix){
  message("Please accept my apologies.  The exportFiles method for redcapDbConnection objects\n",
          "has not yet been written.  Please consider using the API.")
}

exportFiles.redcapApiConnection <- function(rcon, records, field, event=NULL, dir, filePrefix=TRUE){
  #* I may allow multiple fields, events, and records to be provided for convenience.
  if (length(field) > 1) stop ("only one field allowed")
  if (length(event) > 1) stop ("only one event allowed")

  .params <- list(token=rcon$token, content='file',
                  action='export', record=records,
                  field=field)
  if (!is.null(event)) .params[['event']] <- event
  file <- postForm(uri=rcon$url, .params=.params,
                   .opts=curlOptions(ssl.verifyhost=FALSE))
  filename <- gsub("\"", "", attributes(file)$'Content-Type'['name'])
  writeBin(as.vector(file), file.path(dir, paste(records, "_", event, "_", filename, sep="")), 
           useBytes=TRUE)
}
