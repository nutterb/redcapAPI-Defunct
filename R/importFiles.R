importFiles <- function(rcon, file, record, field, event, overwrite=TRUE, ...,
                        proj=NULL)
  UseMethod("importFiles")

importFiles.redcapDbConnection <- function(rcon, file, record, field, event, overwrite=TRUE, ..., 
                                           proj=NULL){
  message("Please accept my apologies.  The importFiles method for redcapDbConnection objects\n",
          "has not yet been written.  Please consider using the API.")
}

importFiles.redcapApiConnection <- function(rcon, file, record, field, event, overwrite=TRUE, ...,
                                            proj=NULL){
  #* Use working directory if 'dir' is not specified
  if (!file.exists(file)) stop(paste0("No file found at '", file, "'"))
  
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
  
  if (!overwrite){
    fileThere <- exportRecords(rcon, records=record, fields=field, events=event)
    if (!is.na(fileThere[field])) stop("A file exists and overwrite=FALSE")
  }
  
  .params <- list(token=rcon$token, content='file',
                  action='import', record=record,
                  field=field, file=httr::upload_file(file), returnFormat='csv')
  if (event != "") .params[['event']] <- event
  
  #* Export the file
  file <- tryCatch(httr::POST(url=rcon$url, body=.params, config=rcon$config),
            error=function(cond) list(status_code = "200"))
  if (file$status_code != "200") stop(paste0(file$status_code, ": ", as.character(file)))
  else message("The file was successfully uploaded")
}
