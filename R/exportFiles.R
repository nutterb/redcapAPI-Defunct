exportFiles <- function(rcon, records, field, event, dir, ...)
  UseMethod("exportFiles")

exportFiles.redcapDbConnection <- function(rcon, records, field, event=NULL, dir){
  message("Please accept my apologies.  The exportFiles method for redcapDbConnection objects\n",
          "has not yet been written.  Please consider using the API.")
}

exportFiles.redcapApiConnection <- function(rcon, records, field, event=NULL, dir){
  if (length(field) > 1) stop ("only one field allowed")
  if (length(event) > 1) stop ("only one event allowed")
  .params <- list(token=rcon$token, content='file',
                  action='export', record=records,
                  field=field, event=event)
  file <- postForm(uri=rcon$url, .params=.params,
                   .opts=curlOptions(ssl.verifyhost=FALSE))
  write(file, file.path(dir, gsub("\"", "", attributes(file)$'Content-Type'['name'])))
}
