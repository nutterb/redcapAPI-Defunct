#' @name exportVersion
#'
#' @title Exports the REDCap Version Number
#' @description Version numbers are returned as a character string.
#'   This feature is available for REDCap 6.0.0 and higher.
#'
#' @param rcon A \code{redcapConnection} object.
#' @param error_handling \code{character}, one of \code{c("null", "error")}. 
#'   An option for how to handle errors returned by the API.
#'   see \code{\link{redcapError}}
#' @param config \code{list}. Additional parameters to pass to 
#'   \code{\link[httr]{POST}}.
#' @param api_param \code{list}. Additional parameters to pass to the 
#'   REDCap API. This provides users to execute calls with options
#'   that may not otherwise be supported by \code{redcapAPI}.
#'
#' @details If this function is used in a version of REDCap that does not
#'   support the Export Version Number function, the character string
#'   \code{'5.12.2'} is returned. This is done solely for the convenience 
#'   of always returning a value that can be compared against other versions.
#'   
#' @section REDCap API Documentation (6.5.0):
#' This method returns the current REDCap version number as plain text 
#' (e.g., 4.13.18, 5.12.2, 6.0.0).
#' 
#' @section REDCap Version:
#' 6.0.0+ 
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

exportVersion <- function(rcon,
                          error_handling = getOption("redcap_error_handling", "null"), 
                          config = list(), 
                          api_param = list()){
  coll <- checkmate::makeAssertCollection()
  
  checkmate::assert_class(x = rcon,
                          classes = "redcapApiConnection",
                          add = coll)
  
  error_handling <- checkmate::matchArg(x = error_handling,
                                        choices = c("null", "error"),
                                        add = coll)
  
  checkmate::assert_list(x = config, 
                         add = coll)
  
  checkmate::assert_list(x = api_param, 
                         add = coll)
  
  checkmate::reportAssertions(coll)
  
  body <- list(token=rcon$token, 
               content='version')
  body <- c(body, api_param)
  
  version <- makeApiCall(rcon = rcon, 
                         body = body, 
                         config = config)
  
  handled <- redcapError(version, error_handling)
  
  if (is.null(handled)){
    version <- "5.12.2"
  }
  
  as.character(version)
}

# ALIASES -----------------------------------------------------------

export_version <- exportVersion