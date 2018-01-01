#' @name recodeCheck
#' @export recodeCheck
#' 
#' @title Change labelling of \code{checkbox} variables
#' @description Rewrites the labelling of \code{checkbox} variables from 
#'   Checked/Unchecked to Yes/No (or some other user-specified labelling).
#'   
#' @param df A data frame, presumably retrieved from REDCap, though not a 
#'   strict requirement.
#' @param vars Optional character vector of variables to convert.  If left 
#'   missing, all of the variables in \code{df} that are identified as 
#'   \code{checkbox} variables are relabelled.  See 'Details' for more about 
#'   identifying \code{checkbox} variables.
#' @param old A character vector to be passed to \code{factor}.  
#'   This indicates the levels to be replaced and their order.
#' @param new A character vector of labels to replace the values in 
#'   \code{levels}.  The first value becomes the reference value.
#' @param reverse For convenience, if the user would prefer to reverse the 
#'   order of the elements in \code{levels} and \code{labels}, 
#'   simply set this to \code{TRUE}.
#'   
#' @details 
#' \code{checkbox} variables are \emph{not} identified using the metadata 
#' from the REDCap database.  Instead, variables are scanned, and those 
#' variables in which every value is in \code{levels} are assumed to be 
#' \code{checkbox} variables.  
#' 
#' Realistically, this could be used to relabel any set of factors with 
#' identical labels, regardless of the data source.  The number of labels is 
#' not limited, but \code{levels} and \code{labels} should have the same length.
#' 
#' The actual code to perform this is not particularly difficult 
#' (\code{df[checkbox] <- lapply(df[checkbox], factor, levels=levels, labels=labels)}), 
#' but \code{checkbox} variables are common enough in REDCap 
#' (and the Checked/Unchecked scheme so unpalatable) that a quick way to 
#' replace the labels was highly desirable
#' 
#' @author Benjamin Nutter

recodeCheck <- function(df, vars, 
                        old=c("Unchecked", "Checked"), new=c("No", "Yes"), 
                        reverse=FALSE){
  # If no variable names are provided, check the data frame for all variables in which all values 
  # are either "Checked" or "Unchecked"
  if (missing(vars)){
    checkbox <- vapply(X = df, 
                       FUN = function(x) all(attributes(x)$redcapLabels %in% old),
                       FUN.VALUE = logical(1))
  }
  
  #* If variable names are given, ensure that they are checkbox variables.  Ignore them if anything
  #* other than "Checked" or "Unchecked" appears in the values.
  else {
    vars_are_check <- vapply(X = df, 
                             FUN = function(x) all(attributes(x)$redcapLabels %in% old),
                             FUN.VALUE = logical(1))
    
    vars_not_check <- vars[!vars_are_check]
    if (any(!vars_are_check)) warning(paste0("'", paste(vars[!vars_are_check], collapse = "', '"), 
                                             "' do not appear to be 'checkbox' variables.",
                                             "\nThese variables were not recoded."))
    checkbox <- vars[vars_are_check]
  }
  
  var.label <- 
    vapply(X = df,
           FUN = labelVector::get_label,
           FUN.VALUE = character(1))
  
  #* Utility function for recoding check variables
  recodeFn <- function(v, old=old, new=new, reverse=reverse){
    if (is.factor(v)) v <- redcapFactorFlip(v)
    attributes(v)$redcapLabels <- if (reverse) rev(new) else new
    if (reverse) attributes(v)$redcapLevels <- rev(attributes(v)$redcapLevels)
    return(redcapFactorFlip(v))
  }
  
  #* Apply the new labels
  df[checkbox] <- lapply(X = df[checkbox], 
                         FUN = recodeFn, 
                         old, new, reverse)
  
  df[checkbox] <-
    mapply(nm = checkbox,
           lab = var.label,
           FUN = function(nm, lab){
             labelVector::set_label(df[[nm]], lab)
           },
           SIMPLIFY = FALSE)
  
  df
}