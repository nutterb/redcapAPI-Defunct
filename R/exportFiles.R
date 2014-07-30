exportFiles <- function(rcon, record, field, event, dir, filePrefix=TRUE, ...)
  UseMethod("exportFiles")

exportFiles.redcapDbConnection <- function(rcon, record, field, event, dir, filePrefix=TRUE, ...){
  message("Please accept my apologies.  The exportFiles method for redcapDbConnection objects\n",
          "has not yet been written.  Please consider using the API.")
}

exportFiles.redcapApiConnection <- function(rcon, record, field, event, dir, filePrefix=TRUE, ...){
  #* Use working directory if 'dir' is not specified
  if (missing(dir)) dir <- getwd()
  
  #* stop the function if arguments do not specify a unique record-event
  if (any(sapply(list(record, field, event), length) > 1)){
    stop("The arguments 'record', 'field', and 'event' may each only have length 1")
  }

  .params <- list(token=rcon$token, content='file',
                  action='export', record=record,
                  field=field)
  if (!missing(event)) .params[['event']] <- event
  
  #* Export the file
  file <- postForm(uri=rcon$url, .params=.params,
                   .opts=curlOptions(ssl.verifyhost=FALSE))
                   
  #* Get the filename
  filename <- gsub("\"", "", attributes(file)$'Content-Type'['name'])
  if (filePrefix) filename <- paste(record, "-", event, "-", filename, sep="")
  
  #* Write the file to a directory
  writeBin(as.vector(file), file.path(dir, filename), 
           useBytes=TRUE)
  message(paste("The file was saved to '", filename, "'", sep=""))
}
