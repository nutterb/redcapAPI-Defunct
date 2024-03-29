#' @name exportInstruments
#' @aliases exportInstruments.redcapApiConnection
#' @aliases exportInstruments.redcapDbConnection
#' @export exportInstruments
#' @importFrom httr POST
#'
#' @title Exports the REDCap Instruments
#' @description Returns a data frame of instruments, names, etc.
#'   
#' @section REDCap Version:
#' 6.5.0 + 
#'
#' @param rcon A REDCap connection object as generated by \code{redcapConnection}
#' @param ... Arguments to be passed to other methods.
#' @param error_handling An option for how to handle errors returned by the API.
#'   see \code{\link{redcap_error}}
#' 
#' @section REDCap API Documentation:
#' This function allows you to export a list of the data collection instruments 
#' for a project. This includes their unique instrument name as seen in the second 
#' column of the Data Dictionary, as well as each instrument's corresponding 
#' instrument label, which is seen on a project's left-hand menu when entering data. 
#' The instruments will be ordered according to their order in the project.
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
#' Additional details on API parameters are found on the package wiki at
#' \url{https://github.com/vubiostat/redcapAPI/wiki/REDCap-API-Parameters}
#'


exportInstruments <- function(rcon, ...) UseMethod("exportInstruments")

#' @rdname exportInstruments
#' @export

exportInstruments.redcapDbConnection <- function(rcon, ...){
  message("Please accept my apologies.  The exportVersion method for redcapDbConnection objects\n",
          "has not yet been written.  Please consider using the API.")
}

#' @rdname exportInstruments
#' @export

exportInstruments.redcapApiConnection <- function(rcon, ...,
                                                  error_handling = getOption("redcap_error_handling"))
{
  coll <- checkmate::makeAssertCollection()
  
  checkmate::assert_class(x = rcon,
                          classes = "redcapApiConnection",
                          add = coll)
  
  error_handling <- checkmate::matchArg(x = error_handling, 
                                        choices = c("null", "error"),
                                        add = coll)
  
  checkmate::reportAssertions(coll)
  
  body <- list(token = rcon$token, 
               content = 'instrument',
               format = 'csv')
  
  x <- httr::POST(url = rcon$url, 
                  body = body, 
                  config = rcon$config)
  
  if (x$status_code != 200) return(redcap_error(x, error_handling))
  
  utils::read.csv(text = as.character(x), 
                  stringsAsFactors = FALSE, 
                  na.strings = "")
}
