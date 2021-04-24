#' @name exportEvents
#' @title Export the Events for a Project
#' 
#' @description Retrieve a data frame giving the users, expiration dates,
#' and data access privileges for each user.
#'
#' @param rcon A \code{redcapApiConnection} object.
#' @param arms \code{integerish} vector of arm numbers to retrieve.
#' @param error_handling An option for how to handle errors returned by the API.
#'   see \code{\link{redcapError}}. One of \code{c("null", "error")}.
#' @param config \code{list} of parameters to pass to \code{\link[httr]{POST}}.
#'   These will be appended to any parameters in \code{rcon$config}.
#' @param api_param \code{list} Additional API parameters to pass into the
#'   body of the API call. This provides users to execute calls with options
#'   that may not otherwise be supported by \code{redcapAPI}.
#' 
#' @details
#' It is not sufficient to make the project a longitudinal project. The
#' project must satisfy one of two conditions: 1) have at least two arms and
#' one event defined; or 2) have one arm and at least two events defined. If 
#' neither of these conditions are satisfied, the API will return a message
#' such as \code{ERROR: You cannot export arms for classic projects}, an 
#' error message that isn't as descriptive of the nature of the problem as 
#' we might like.
#' 
#' @section REDCap API Documentation:
#' This function allows you to export the events for a project
#' 
#' NOTE: this only works for longitudinal projects.
#' 
#' @section REDCap Version:
#' 5.8.2+ 
#' 
#' @section Known REDCap Limitations:
#' None 
#' 
#' @return Returns a data frame with six columns
#' \itemize{
#'   \item{\code{event_name} }{The desciptive name of the event.}
#'   \item{\code{arm_num} }{The arm number in which the event occurs.}
#'   \item{\code{day_offset} }{The days offset from the first event.}
#'   \item{\code{offset_min} }{The minimum offset value.}
#'   \item{\code{offset_max} }{The maximium offset value.}
#'   \item{\code{unique_event_name} }{A unique event identifying name.}
#' }
#'
#' @author Benjamin Nutter
#'
#' @references
#' Please refer to your institution's API documentation.
#' 
#' @export

exportEvents <- function(rcon, 
                         arms = numeric(0), 
                         error_handling = getOption("redcap_error_handling"), 
                         config = list(), 
                         api_param = list()){
  coll <- checkmate::makeAssertCollection()
  
  checkmate::assert_class(x = rcon,
                          classes = "redcapApiConnection",
                          add = coll)
  
  checkmate::assert_integerish(x = arms,
                               null.ok = TRUE,
                               add = coll)
  
  error_handling <- checkmate::matchArg(x = error_handling, 
                                        choices = c("null", "error"),
                                        add = coll)
  
  checkmate::assert_list(x = config, 
                         add = coll)
  
  checkmate::assert_list(x = api_param, 
                         add = coll)
  
  checkmate::reportAssertions(coll)
  
  body <- list(content = 'event', 
               format = 'csv', 
               returnFormat = 'csv')
  
  if (length(arms) > 0){
    body <- c(body, 
              vectorToApiBodyList(arms, 
                                  parameter_name = "arms"))
  }
  
  response <- makeApiCall(rcon = rcon, 
                          body = body, 
                          config = config)
  
  
  redcapError(response, error_handling)
  
  utils::read.csv(text = as.character(response),
                  stringsAsFactors = FALSE,
                  na.strings = "")
}

# ALIASES -----------------------------------------------------------

#' @rdname exportEvents
#' @export

export_events <- exportEvents