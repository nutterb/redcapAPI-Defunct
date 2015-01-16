#' @name redcapFactorFlip
#' @export redcapFactorFlip
#' 
#' @title Convert REDCap factors between labelled and coded
#' @description Factors exported from REDCap can be toggled between the coded 
#'   and labelled values with the use of the attributes assigned to the 
#'   factors during export.
#'   
#' @param v A factor exported from REDCap.  The REDCap type may be radio, 
#'   dropdown, check, yesno, etc.
#'   
#' @details Each factor type variable in REDCap is given the attributes 
#' \code{redcapLabels} and \code{redcapLevels}.  With these attached to the 
#' vector, switching between the coded and labelled values can be done with 
#' ease.  This may be helpful when the coded value has importance, 
#' such as 0/1 for death, or if a yes is worth 6 points (instead of 1).
#' 
#' @author Benjamin Nutter
#' 

redcapFactorFlip <- function(v){
  #* extract attributes to be applied later
  redcapLabels <- attributes(v)$redcapLabels
  redcapLevels <- attributes(v)$redcapLevels
  
  if (!is.null(redcapLabels) | is.null(redcapLevels))
    stop("This does not appear to be a REDCap factor.")
  
  #* labelled to coded
  if ("factor" %in% class(v)){
    v <- factor(as.character(v), 
                redcapLabels, 
                redcapLevels)
    v <- as.character(v)
    if (is.numeric(redcapLevels)) v <- as.numeric(v)
  }
  
  #* coded to labelled
  else{
    v <- factor(v, 
                attributes(v)$redcapLevels,
                attributes(v)$redcapLabels)
  }
  
  #* reapply attributes
  attr(v, 'redcapLabels') <- redcapLabels
  attr(v, 'redcapLevels') <- redcapLevels
  return(v)
}
