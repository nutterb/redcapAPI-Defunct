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
  
  #* make sure 'field' exist in the project and are 'file' fields
  if (is.null(meta_data)) meta_data <- exportMetaData(rcon)
  if (!field %in% meta_data$field_name) stop(paste("'", field, "' does not exist in the project.", sep=""))
  if (meta_data$field_type[meta_data$field_name == field] != "file")
      stop(paste("'", field, "' is not of field type 'file'", sep=""))
      
  #* make sure 'event' exists in the project
  if (missing(event)) event <- ""
  if (is.null(events_list)) events_list <- exportEvents(rcon)
  if (!is.null(events_list)){
    if (!event %in% events_list$unique_event_name) 
      stop(paste("'", event, "' is not a valid event name in this project.", sep=""))
  }
  
  .params <- list(token=rcon$token, content='file',
                  action='delete', record=record,
                  field=field)
  if (event == "") .params[['event']] <- event
  
  #* Delete the file
  noFile <- tryCatch(postForm(uri=rcon$url, .params=.params,
                           .opts=curlOptions(ssl.verifyhost=FALSE)),
                  error = function(cond) if (grepl("Bad Request", cond[1])) return(TRUE))         
  if (noFile) message("There is no file to delete")
    else message("The file was successfully deleted")
}
