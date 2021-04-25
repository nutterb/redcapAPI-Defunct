#' @name vectorToApiBodyList
#' @title Convert R Vector To List for the API Call Body
#' 
#' @description Converts an R vector to a list that will be suitable for 
#'   \code{makeApiCall}.
#'   
#' @param vector An \code{atomic} vector.
#' @param parameter_name \code{character(1)}. The REDCap API parameter name.
#' 
#' @author Benjamin Nutter with thanks to Philip Chase for showing me how 
#'   these need to be formatted.
#'   
#' @section Functional Requirements: 
#' \enumerate{
#'  \item Return a named list. 
#'  \item The name of each element in the list starts with the value of \code{parameter_name}
#'  \item Throw an error if \code{x} is not an atomic vector
#'  \item Throw an error if \code{parameter_name} is not a \code{character(1)}
#' }
#'
#' @export

vectorToApiBodyList <- function(vector, parameter_name){
  # Argument Validation ---------------------------------------------
  coll <- checkmate::makeAssertCollection()
  
  checkmate::assert_atomic(x = vector, 
                           add = coll)
  
  checkmate::assert_character(x = parameter_name, 
                              len = 1, 
                              add = coll)
  
  checkmate::reportAssertions(coll)
  
  # Functional Code -------------------------------------------------
  rv <- lapply(vector, identity)
  names(rv) <- sprintf("%s[%s]", 
                       parameter_name, 
                       seq_along(rv))
  rv
}
