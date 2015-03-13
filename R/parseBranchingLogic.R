#' @name parseBranchingLogic
#' @export parseBranchingLogic
#' 
#' @title Parse Branching Logic
#' @description Branching logic from the REDCap Data Dictionary is parsed into
#'   R Code and returned as expressions.  These can be evaluated if desired
#'   and allow the user to determine if missing values are truly missing or
#'   not required because the branching logic prevented the variable from being
#'   presented.
#'   
#' @param l A vector of REDCap branching logic statements.  These are usually
#'   passed as the vector \code{meta_data$branching_logic}.  
#'   
#' @details For a study, I was asked to identify which subjects had missing 
#'   values so that remaining data could be collected.  The initial pass of 
#'   \code{is.na} produced a lot of subjects missing values where there was no
#'   need to collect data because they did not qualify for some variables in 
#'   the branching logic.  Parsing the logic allowed me to determine which 
#'   values we expected to be missing and narrow the search to just those 
#'   subjects with legitimately missing values.
#'   
#' @return Returns a list of unevaluated expressions.
#' 
#' @author Benjamin Nutter
#' 

parseBranchingLogic <- function(l){
  l <- gsub(" or ", " | ", l)
  l <- gsub(" and ", " & ", l)
  l <- gsub("[(]", "___", l)
  l <- gsub("[)]", "", l)
  l <- gsub("([[]|[]])", "", l)
  l <- gsub(" [=] ", " == ", l)
  lapply(l, function(x) ifelse(x=="", NA, parse(text=x)))
}