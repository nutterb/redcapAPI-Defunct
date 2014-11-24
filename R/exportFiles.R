exportFiles <- function(rcon, record, field, event, dir, filePrefix=TRUE, ...,
                        proj=NULL)
  UseMethod("exportFiles")

exportFiles.redcapDbConnection <- function(rcon, record, field, event, dir, filePrefix=TRUE, ..., 
                        proj=NULL){
  message("Please accept my apologies.  The exportFiles method for redcapDbConnection objects\n",
          "has not yet been written.  Please consider using the API.")
}

exportFiles.redcapApiConnection <- function(rcon, record, field, event, dir, filePrefix=TRUE, ...,
                        proj=NULL){
  #* Use working directory if 'dir' is not specified
  if (missing(dir)) dir <- getwd()
  
  #* stop the function if arguments do not specify a unique record-event
  if (missing(event)) event <- ""
  if (any(sapply(list(record, field, event), length) > 1)){
    stop("The arguments 'record', 'field', and 'event' may each only have length 1")
  }
  
  #* make sure 'field' exist in the project and are 'file' fields
  if (is.null(proj$meta_data)) meta_data <- exportMetaData(rcon)
  if (!field %in% meta_data$field_name) stop(paste("'", field, "' does not exist in the project.", sep=""))
  if (meta_data$field_type[meta_data$field_name == field] != "file")
      stop(paste0("'", field, "' is not of field type 'file'"))
      
  #* make sure 'event' exists in the project
  if (is.null(proj$events)) events_list <- exportEvents(rcon)
  if (class(events_list) == 'data.frame'){
    if (!event %in% events_list$unique_event_name) 
      stop(paste0("'", event, "' is not a valid event name in this project."))
  }

  .params <- list(token=rcon$token, content='file',
                  action='export', returnFormat='csv',
                  record=record,
                  field=field)
  if (event != "") .params[['event']] <- event
  
  #* Export the file
  x <- httr::POST(url=rcon$url, body=.params, config=rcon$config)
  if (x$status_code == 200){
    #* strip the returned character string to just the file name.
    filename = sub("[[:print:]]+; name=", "", x$headers$'content-type')
    filename = gsub("\"", "", filename)
    filename <- sub(";charset[[:print:]]+", "", filename)
    
    #* Add the prefix
    if (filePrefix) filename <- paste(record, "-", event, "-", filename, sep="")
    
    #* Write to a file
    writeBin(as.vector(x$content), file.path(dir, filename), 
             useBytes=TRUE)
    message(paste("The file was saved to '", filename, "'", sep=""))
  }
  else{                 
   stop(paste0(x$status_code, ": ", as.character(x)))
  }
}
