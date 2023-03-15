#' @name exportLogging
#' 
#' @title Export Logging Records
#' @description Export the logging (audit trail) of all changes made to 
#'   a project, including data exports, data changes, project 
#'   metadata changes, modification of user rights, etc.
#'   
#' @param rcon A REDCap connection object as created by \code{redcapConnection}.
#' @param logtype \code{character} with maximum length of 1. The log event types to export. By 
#'   default, all event types are exported. Must be one of
#'   \code{c("export", "manage", "user", "record", "record_add", "record_edit", "record_delete", "lock_record", "page_view")}
#' @param user \code{character} with maximum length of 1. Users for whom to return logs. By default
#'   logs for all users are returned.
#' @param record \code{character} with maximum length of 1. Record ID for which logs are to be returned.
#'   By default, logs are returned for all records.
#' @param dag \code{character} with maximum length of 1. Data access group ID for which to return logs. 
#'   By default, logs are returned for all data access groups.
#' @param beginTime \code{POSIXct} with maximum length 1. When given, only 
#'   logs recorded after this time will be returned.
#' @param endTime \code{POSIXct} with maximum length 1. When given, only logs
#'   recorded before this time will be returned. 
#' @param ... Additional arguments to be passed between methods
#' @param error_handling An option for how to handle errors returned by the API.
#'   see \code{\link{redcap_error}}
#' @param config \code{list} Additional configuration parameters to pass to 
#'   \code{\link[httr]{POST}}. These are appended to any parameters in 
#'   \code{rcon$config}.
#' @param api_param \code{list} Additional API parameters to pass into the
#'   body of the API call. This provides users to execute calls with options
#'   that may not otherwise be supported by \code{redcapAPI}.
#'   
#' @author Benjamin Nutter
#' 
#' @examples 
#' \dontrun{
#' url <- "Enter your API URL here"
#'   token <- "Enter your API token here"
#'   
#'   rcon <- redcapConnection(url = url, 
#'                            token = token)
#'                     
#'   # Export all of the logging events       
#'   exportLogging(rcon)
#'   
#'   # Export all of the events for record '2'
#'   exportLogging(rcon, 
#'                 record = "2")
#'                 
#'   #Export all of the events where a record was deleted
#'   exportLoging(rcon, 
#'                logtype = "record_delete")
#' }
#' @export

exportLogging <- function(rcon, 
                          logtype = character(0), 
                          user = character(0), 
                          record = character(0), 
                          dag = character(0), 
                          beginTime = .POSIXct(character(0)), 
                          endTime = .POSIXct(character(0)), 
                          ...){
  
  UseMethod("exportLogging")
  
}

#' @rdname exportLogging
#' @export

exportLogging.redcapDbConnection <- function(rcon, 
                                             logtype = character(0), 
                                             user = character(0), 
                                             record = character(0), 
                                             dag = character(0), 
                                             beginTime = .POSIXct(character(0)), 
                                             endTime = .POSIXct(character(0)), 
                                             ...){
  message("Please accept my apologies.  The exportLogging method for redcapDbConnection objects\n",
          "has not yet been written.  Please consider using the API.")
}

#' @rdname exportLogging
#' @export

exportLogging.redcapApiConnection <- function(rcon, 
                                              logtype = character(0), 
                                              user = character(0), 
                                              record = character(0), 
                                              dag = character(0), 
                                              beginTime = as.POSIXct(character(0)), 
                                              endTime = as.POSIXct(character(0)), 
                                              ...,
                                              error_handling = getOption("redcap_error_handling"),
                                              config = list(), 
                                              api_param = list()){
  
  # Argument checks -------------------------------------------------
  coll <- checkmate::makeAssertCollection()
  
  checkmate::assert_class(x = rcon,
                          classes = "redcapApiConnection",
                          add = coll)
  
  if (length(logtype) == 1){
    logtype <- checkmate::matchArg(x = logtype, 
                                   choices = c("export", "manage", "user", "record", 
                                               "record_add", "record_edit", "record_delete", 
                                               "lock_record", "page_view"),
                                   add = coll, 
                                   .var.name = "logtype")
  }
  
  checkmate::assert_character(x = user,
                              max.len = 1,
                              add = coll)
  
  checkmate::assert_character(x = record, 
                              max.len = 1,
                              add = coll)
  
  checkmate::assert_character(x = dag, 
                              max.len = 1,
                              add = coll)
  
  checkmate::assert_posixct(x = beginTime, 
                            max.len = 1, 
                            add = coll)
  
  checkmate::assert_posixct(x = endTime, 
                            max.len = 1, 
                            add = coll)
  
  error_handling <- checkmate::matchArg(x = error_handling,
                                        choices = c("null", "error"),
                                        add = coll)
  
  checkmate::reportAssertions(coll)
  
  # Build the Body List ---------------------------------------------
  
  body <- list(content = 'log', 
               format = 'csv', 
               returnFormat = 'csv', 
               logtype = logtype, 
               user = user, 
               record = record, 
               dag = dag, 
               beginTime = format(beginTime, 
                                  format = "%Y-%m-%d %H:%M"), 
               endTime = format(endTime, 
                                format= "%Y-%m-%d %H:%M"))
  
  body <- body[lengths(body) > 0]
  
  # Call to the API -------------------------------------------------
  response <- makeApiCall(rcon, 
                          body = c(body, api_param), 
                          config = config)
  
  if (response$status_code != 200){
    redcap_error(response, 
                 error_handling = error_handling)
  } 
  
  Log <- utils::read.csv(text = as.character(response),
                         stringsAsFactors = FALSE,
                         na.strings = "")
  
  # Format and return data ------------------------------------------
  Log$timestamp <- as.POSIXct(Log$timestamp, 
                              format = "%Y-%m-%d %H:%M")
  
  Log
}

# Tests to perform --------------------------------------------------
# * Return an error when rcon is not a redcapConnection object
# * Return an error when logtype is not one of c("export", "manage", "user", "record", "record_add", "record_edit", "record_delete", "lock_record", "page_view")
# * Return an error when user is not character
# * Return an error when dag is not a character
# * Return an error when beginTime is not POSIXct
# * Return an error when beginTime has length > 1
# * Return an error when endTime is not POSIXct
# * Return an error when endTime has length > 1
# * Return logs with all default arguments
# * Return logs for logtype = "export"
# * Return logs for logtype = "manage"
# * Return logs for logtype = "user"
# * Return logs for logtype = "record"
# * Return logs for logtype = "record_add"
# * Return logs for logtype = "record_edit"
# * Return logs for logtype = "record_delete"
# * Return logs for logtype = "lock_record"
# * Return logs for logtype = "page_view"
# * Return logs for an existing user
# * Return empty logs for a non-existent user
# * Return logs for an existing record
# * Return empty logs for a non-existent record
# * Return logs for a dag
# * Return empty logs for a non-existent dag
# * Return logs after a beginTime
# * Return logs before an endTime

