#' @name redcap_error
#' @title Handle Errors from the REDCap API
#' 
#' @description Determine the proper way to handle errors returned from the API.
#'   Not all errors should be fatal.  See Details for more
#'   
#' @param x Object returned by \code{\link[httr]{POST}}.
#' @param error_handling Direction for how to handle errors.  May be either 
#'   \code{"null"} or \code{"error"}. See Details.
#' 
#' @details Maintaining consistent functionality for all types of REDCap projects 
#'   requires that errors be handled delicately.  It is not always desirable for an
#'   error from the API to terminate the program.  One example of such a case is when
#'   a user executes the \code{exportEvents} function for a classic project; 
#'   doing so returns an error from the API that events cannot be exported for
#'   classic projects.  In REDCap versions earlier than 6.5.0, there is no way to
#'   determine if a project is classic or longitudinal without attempting to export
#'   the events.  
#'   
#'   For this reason, it is often preferable to have these kinds of errors return 
#'   \code{NULL} so that the program doesn't crash if it doesn't need to (one such 
#'   instance where it doesn't need to crash is when \code{exportEvents} is called
#'   within \code{exportRecords}; the \code{events} argument is irrelevant to a 
#'   classic project and the error can safely be ignored.
#'   
#'   The other common type of error that does not need to be fatal is when a 
#'   \code{redcapAPI} method is sent to a REDCap instance that does not support the 
#'   method.  For example, the \code{exportVersion} method is not supported in 
#'   REDCap instances earlier than 6.0.0.  In these cases, we may prefer not to cast
#'   a hard error.
#'   
#'   These two types of errors may be handled in one of two ways.  When the 
#'   error handler is set to \code{"null"}, a \code{NULL} is returned.  When the 
#'   error handler is set to \code{"error"}, the error is returned.  The option 
#'   is set globally using \code{options(redcap_error_handler = "null")} and is
#'   set to \code{"null"} by default.
#'   
#' @section Handled Errors:
#' Only the errors listed below are handled.  All others throw a hard error.
#' 
#' "ERROR: The value of the parameter \"content\" is not valid"\cr
#' "ERROR: You cannot export arms for classic projects"\cr
#' "ERROR: You cannot export events for classic projects"
#'   
#' @author Benjamin Nutter
#' 

redcap_error <- function(x, error_handling)
{
  handle <- c("ERROR: The value of the parameter \"content\" is not valid",
              "ERROR: You cannot export arms for classic projects",
              "ERROR: You cannot export events for classic projects",
              "ERROR: You cannot export form/event mappings for classic projects")
  if (as.character(x) %in% handle && error_handling == "null") return(NULL)
  else stop(paste0(x$status_code, ": ", as.character(x)))
}