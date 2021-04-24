#' @name redcapConnection
#' @title Connect to a REDCap Database
#' 
#' @description Creates an object of class \code{redcapApiConnection} for 
#' using the REDCap API
#' 
#' @param url \code{character(1)} The URL for a REDCap API connection.
#'   Check your institution's REDCap documentation for this address.
#' @param token \code{character(1)} REDCap API token. It is expected that 
#'   this will be exactly 32 characters (after removing leading and 
#'   trailing whitespace)
#' @param config \code{list} of configuration parameters to pass to 
#'   \code{\link[httr]{config}}. This allows the 
#'   user to set additional configurations for the API calls, such as 
#'   certificates, ssl version, etc.
#' @param x A \code{redcapApiConnection} object to be printed. 
#' @param ... Additional arguments to pass to other methods.
#'   
#' @details To obtain an API token for a project, do the following:\cr
#' Enter the 'User Right' section of a project\cr
#' Select a user\cr
#' Check the box for 'API Data Export' or 'API Data Import,' as appropriate. 
#' 
#' Tokens are specific to a project, and a token must be created for each 
#' project for which you wish to use the API.
#' 
#' Additional Curl option can be set in the \code{config} argument.  See the documentation
#' for \code{\link[httr]{config}} and \code{\link[httr]{httr_options}} for more Curl options.
#' 
#' @author Jeffrey Horner, Benjamin Nutter
#' 
#' @references 
#' This functionality were originally developed by Jeffrey Horner in the 
#' \code{redcap} package.
#' \url{https://github.com/vubiostat/redcap}
#' 
#' @section Functional Requirements:
#' \enumerate{
#'  \item Return an object of class \code{redcapApiConnection}
#'  \item Leading and trailing white space are removed from \code{token}
#'  \item Throw an error if \code{url} is not a \code{character(1)}
#'  \item Throw an error if \code{token} is not a \code{character(1)}
#'  \item Throw an error if \code{token} does not have either 32 or 64 characters
#'  \item Throw an error if \code{config} is not a \code{list}
#' }
#' 
#' @export

redcapConnection <- function(url = getOption("redcap_api_url"), 
                             token, 
                             config = list()){
  # Argument Validation ---------------------------------------------
  coll <- checkmate::makeAssertCollection()
  
  checkmate::assert_character(x = url, 
                              len = 1, 
                              add = coll)
  
  checkmate::assert_character(x = token, 
                              len = 1, 
                              add = coll)
  
  checkmate::assert_list(x = config,
                         add = coll)
  
  checkmate::reportAssertions(coll)
  
  token <- trimws(token)
  
  if (!nchar(token) %in% c(32, 64)){
    coll$push(
      sprintf("`token` must have either 32 or 64 characters. (It currently has %s characters)", 
              nchar(token))
    )
  }
  
  checkmate::reportAssertions(coll)
  
  # Functional Code -------------------------------------------------
  structure(list(url = url, 
                 token = token, 
                 config = config), 
            class = "redcapApiConnection")
}

# ALIASES -----------------------------------------------------------

#' @rdname redcapConnection
#' @export

redcap_connection <- redcapConnection

# PRINT METHOD ------------------------------------------------------

#' @rdname redcapConnection
#' @export

print.redcapApiConnection <- function(x, ...){
  cat("REDCap API Connection", 
      sprintf("URL:     %s", x$url), 
      "Token:   Shhh...it's a secret", 
      "Config:", 
      sep = "\n")
  print(x$config)
}
