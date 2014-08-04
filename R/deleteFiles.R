deleteFiles <- function(rcon, record, field, event, ...)
  UseMethod("deleteFiles")

deleteFiles.redcapDbConnection <- function(rcon, record, field, event, ...){
  message("Please accept my apologies.  The deleteFiles method for redcapDbConnection objects\n",
          "has not yet been written.  Please consider using the API.")
}

deleteFiles.redcapApiConnection <- function(rcon, record, field, event, 
                        meta_data=getOption('redcap_project_info')$meta_data, 
                        events_list = getOption('redcap_project_info')$events, ...){
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
  if (class(events_list) != "data.frame"){
    if (!event %in% events_list$unique_event_name) 
      stop(paste("'", event, "' is not a valid event name in this project.", sep=""))
  }
  
  .params <- list(token=rcon$token, content='file',
                  action='delete', record=record,
                  field=field, returnFormat='csv')
  if (event != "") .params[['event']] <- event
  
  #* Delete the file
  x <- tryCatch(httr::POST(url=rcon$url, body=.params),
                error=function(cond) list(status_code=200))
  if (x$status_code != "200")stop(paste(x$status_code, ": ", as.character(x), sep=""))
  else message("The file was successfully deleted")
  
}
