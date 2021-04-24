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
