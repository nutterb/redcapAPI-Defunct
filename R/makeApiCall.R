#' @name makeApiCall
#' @title Make REDCap API Calls
#' 
#' @description Constructs and executes API calls to the REDCap API. These
#'   are left deliberately abstract in order to be flexible enough to 
#'   support the \code{redcapAPI} functions, but also allow users to 
#'   execute calls for new REDCap features that are not yet implemented.
#'   
#' @param rcon \code{redcapApiConnection} object.
#' @param body \code{list} List of parameters to be passed to \code{\link[httr]{POST}}'s 
#'   \code{body} argument
#' @param config \code{list} A list of options to be passed to \code{\link[httr]{POST}}.
#'   These will be appended to the \code{config} options included in the 
#'   \code{rcon} object.
#'   
#' @author Benjamin Nutter
#' 
#' @references
#' Please refer to your institution's API documentation.
#' 
#' @section Functional Requirements:
#' \enumerate{
#'  \item Throw an error if \code{rcon} is not a \code{redcapApiConnection}
#'  \item Throw an error if \code{body} is not a list.
#'  \item Throw an error if \code{config} is not a list.
#' }
#' 
#' @export

makeApiCall <- function(rcon, body = list(), config = list()){
  # Argument Validation ---------------------------------------------
  coll <- checkmate::makeAssertCollection()
  
  checkmate::assert_class(x = rcon, 
                          classes = "redcapApiConnection", 
                          add = coll)
  
  checkmate::assert_list(x = body, 
                         add = coll)
  
  checkmate::assert_list(x = config, 
                         add = coll)
  
  checkmate::reportAssertions(coll)
  
  # Functional Code -------------------------------------------------
  
  httr::POST(
    url = rcon$url, 
    body = c(list(token = rcon$token), 
              body),
    config = c(rcon$config, 
               config) 
  )
}

# ALIASES -----------------------------------------------------------

#' @rdname makeApiCall
#' @export

make_api_call <- makeApiCall

