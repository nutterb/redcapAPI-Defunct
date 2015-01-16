#' @name apiCall
#' @importFrom httr POST
#' 
#' @title Execute a Call to the REDCap API
#' @description \code{apiCall} is a wrapper for attempting to access the 
#' API via \code{httr::POST}, and then via \code{RCurl::postForm} if a 
#' particular error occurs.  This prevents a particular kind of error for 
#' which I haven't   found a proper solution, but at least allows the 
#' expected behavior of the package.  Since writing this, I have found
#' a solution to set the config option \code{encoding='identity'}.  I 
#' may remove this function at some point in the future.
#' 
#' @param url URL of the REDCap API
#' @param body List of parameters to be passed to \code{httr::POST}'s 
#'   \code{body} argument or \code{RCurl::postForm}'s \code{.param} argument.
#' @param config A list of options to be passed to \code{httr::POST}'s 
#'   \code{config} argument or \code{RCurl::postForm}'s \code{.opts} argument.
#'   
#' @details Somewhere in the middle of an upgrade to RStudio, R 3.1.1, and 
#' various other system changes, I began seeing the error
#' 'GnuTLS recv error (-9): A TLS packet with unexpected length was received.'
#'  I still don't know what this error means, but it only occurs when using 
#'  \code{httr} on Linux.  The \code{RCurl} equivalents appear to work just fine.
#'  
#'  In order to prevent this error from occurring, and making the package 
#'  rather useless, \code{apiCall} wraps \code{httr::POST} into a 
#'  \code{tryCatch} call.  If the GnuTLS error is thrown, \code{apiCall} 
#'  then resorts to using the \code{RCurl} equivalent call.
#'  
#'  Since originally writing this function, I've determined that the problem 
#'  occurs due to weird characters being exported from REDCap that cannot be
#'  properly escaped in R.  It can be resolved by using the config option
#'  \code{encoding = 'identity'}.  Making this a default could make this
#'  function unnecessary.
#'  
#' @author Benjamin Nutter
#' 
#' @references
#' Please refer to your institution's API documentation.
#' 
#' Additional details on API parameters are found on the package wiki at
#' \url{https://github.com/nutterb/redcapAPI/wiki/REDCap-API-Parameters}
#' 

apiCall <- function(url, body, config){
  x <- tryCatch(httr::POST(url=url, body=body, config=config),
                error = function(cond){
                  if (grepl("GnuTLS recv error [(]-9[)]", cond)){
                    m <- httr::POST(url=url, body=body,
                                         config=c(list(encoding='identity'), config))
                    return(m)
                  }
                })
  return(x)
}
