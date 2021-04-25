#' @name exportFiles
#' 
#' @title Exports a File attached to a Record
#' @description A single file from a single record is retrieved.  The behavior 
#' of this function is consistent with the behavior of the API, which only 
#' allows one file to be downloaded at a time
#' 
#' @param rcon A \code{redcapApiConnection} object.
#' @param record \code{character(1)} or \code{integerish(1)} The record ID in 
#'   which the desired file is stored.
#' @param field \code{character(1)} The field name in which the file is stored. 
#' @param event \code{character(1)} The event name for the file.    
#'   This applies only to longitudinal projects.  If the event is not
#'   supplied for a longitudinal project, the API will return an error message
#' @param dir \code{character} A directory/folder to which the file will be saved. 
#'   By default, the working directory is used. 
#' @param filePrefix \code{logical(1)}.  Determines if a prefix is appended to the file 
#'   name.  The prefix takes the form [record_id]-[event_name]-[file_name].  
#'   The file name is always the same name of the file as it exists in REDCap
#' @param bundle A \code{redcapBundle} object as created by \code{exportBundle}.
#' @param error_handling \code{character(1)}.  One of \code{c("null", "error")}.
#'   An option for how to handle errors returned by the API.
#'   see \code{\link{redcapError}}
#' @param config \code{list} Additional configuration parameters to pass to 
#'   \code{\link[httr]{POST}}. These are appended to any parameters in 
#'   \code{rcon$config}.
#' @param api_param \code{list} Additional API parameters to pass into the
#'   body of the API call. This provides users to execute calls with options
#'   that may not otherwise be supported by \code{redcapAPI}.
#' 
#' @details The function may only export a single file.  
#' See the examples for suggestions on exporting multiple files.
#' 
#' Note that the name of the file can not be changed.  Whatever name exists in 
#' REDCap is the name that will be used, although the record ID and event name 
#' may be appended as a prefix
#' 
#' @section REDCap API Documentation (6.5.0):
#' This method allows you to download a document that has been attached to an 
#' individual record for a File Upload field. Please note that this method may also 
#' be used for Signature fields (i.e. File Upload fields with "signature" validation type).
#' 
#' Note about export rights: Please be aware that Data Export user rights will be 
#' applied to this API request. For example, if you have "No Access" data export rights 
#' in the project, then the API file export will fail and return an error. And if you 
#' have "De-Identified" or "Remove all tagged Identifier fields" data export rights, 
#' then the API file export will fail and return an error *only if* the File Upload 
#' field has been tagged as an Identifier field. To make sure that your API request 
#' does not return an error, you should have "Full Data Set" export rights in the project.
#' 
#' @section REDCap Version:
#' 5.8.2+ 
#' 
#' @section Known REDCap Limitations:
#' None
#' 
#' @author Benjamin Nutter
#' 
#' @references
#' Please refer to your institution's API documentation.
#' 
#' @export

exportFiles <- function(rcon, 
                        record, 
                        field, 
                        event = character(0), 
                        dir = character(0), 
                        filePrefix = TRUE,
                        error_handling = getOption("redcap_error_handling", "error"), 
                        bundle = getOption("redcap_bundle"),
                        config = list(), 
                        api_param = list()){
  
  # Argument Validation ---------------------------------------------
  coll <- checkmate::makeAssertCollection()
  
  checkmate::assert_class(x = rcon, 
                          classes = "redcapApiConnection", 
                          add = coll)
  
  if (!checkmate::test_integerish(x = record, len = 1) &&
      !checkmate::test_character(x = record, len = 1)){
    coll$push("`record` must be either a `character(1)` or `integerish(1)`")
  }
  
  checkmate::assert_character(x = field, 
                              len = 1, 
                              add = coll)
  
  checkmate::assert_character(x = event, 
                              max.len = 1, 
                              add = coll)
  
  checkmate::assert_character(x = dir, 
                              max.len = 1, 
                              add = coll)
  
  checkmate::assert_logical(x = filePrefix, 
                            len = 1, 
                            add = coll)
  
  error_handling <- checkmate::matchArg(error_handling, 
                                        choices = c("null", "error"), 
                                        add = coll)
  
  checkmate::assert_class(x = bundle, 
                          classes = "redcapBundle", 
                          null.ok = TRUE, 
                          add = coll)
  
  checkmate::assert_list(x = config, 
                         names = "named", 
                         add = coll)
  
  checkmate::assert_list(x = api_param, 
                         names = "named", 
                         add = coll)
  
  checkmate::reportAssertions(coll)
  
  MetaData <- .exportFiles_getMetaData(rcon, bundle)
  
  .exportFiles_validateFieldArgument(field, MetaData, coll)
  
  .exportFiles_validateEventArgument(event, rcon, bundle, coll)
  
  body <- list(content = 'file',
               action = 'export', 
               returnFormat = 'csv',
               record = record,
               field = field)
  
  if (length(event) > 0){
    body[['event']] <- event
  }
  
  response <- makeApiCall(rcon, 
                          body = c(body, api_param), 
                          config = config)
  
  redcapError(response, error_handling)
  
  #* strip the returned character string to just the file name.
  filename <- sub(pattern = "[[:print:]]+; name=", 
                  replacement = "", 
                  x = response$headers$'content-type')
  filename <- gsub(pattern = "\"", 
                   replacement = "", 
                   filename)
  filename <- sub(pattern = ";charset[[:print:]]+", 
                  replacement = "", 
                  x = filename)
  
  #* Add the prefix
  if (filePrefix){
    filename <- sprintf("%s-%s-%s", 
                        record, 
                        event, 
                        filename)
  }
  
  if (length(dir) == 0){
    dir <- NULL
  }
  
  #* Write to a file
  writeBin(object = as.vector(response$content), 
           con = file.path(dir, filename), 
           useBytes = TRUE)
  
  message("The file was saved to '", filename, "'")
  
}

# Unexported --------------------------------------------------------

.exportFiles_getMetaData <- function(rcon, bundle){
  if (is.null(bundle$meta_data)){
    exportMetaData(rcon)
  } else{ 
    bundle$meta_data
  }
}

.exportFiles_validateFieldArgument <- function(field, MetaData, coll){
  #* make sure 'field' exist in the project and are 'file' fields
  if (!field %in% MetaData$field_name) 
  {
    coll$push(paste("'", field, "' does not exist in the project.", sep=""))
  }
  else if (MetaData$field_type[MetaData$field_name == field] != "file")
  {
    coll$push(paste0("'", field, "' is not of field type 'file'"))
  }
  
  checkmate::reportAssertions(coll)
}

.exportFiles_getEventsList <- function(rcon, bundle){
  if (is.null(bundle$events)){
    exportEvents(rcon)
  } else {
    bundle$events
  }
}

.exportFiles_validateEventArgument <- function(event, rcon, bundle, coll){
  EventsList <- .exportFiles_getEventsList(rcon, bundle)
  
  if (inherits(EventsList, "data.frame"))
  {
    if (!event %in% EventsList$unique_event_name) 
      coll$push(paste0("'", event, "' is not a valid event name in this project."))
  }
  
  checkmate::reportAssertions(coll)
}

# ALIASES -----------------------------------------------------------

export_files <- exportFiles
