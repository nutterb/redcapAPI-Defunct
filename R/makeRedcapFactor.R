#' @name makeRedcapFactor
#' @title Factors from REDCap Variables
#' 
#' @description Convert variables to objects with the class \code{redcapFactor}.
#'   These factors have attributes for the levels and the labels from the 
#'   data dictionary.  This allows for the factor to be converted to and 
#'   from its original coding.  This can be useful if the levels are 
#'   non-sequential numeric values that have meaning, such as scores on 
#'   a quality of life survey.
#'   
#' @param x An \code{atomic} vector.
#' @param coding \code{character(1)} with the coding to be applied to the 
#'   factor.
#' @param factors \code{logical(1)}. Determines if the result will have the
#'   \code{factor} class.
#' @param var_name \code{character(1)}. The name of the variable being converted.
#' @param suffix \code{character(1)}, the suffix of the checkbox variable.
#' @param checkboxLabels \code{logical(1)}. Determines the behavior for how 
#'   checkbox labels are applied to the data.
#'   
#' @section Functional Requirements:
#' 
#' \enumerate{
#'  \item Return an object that has class \code{redcapFactor} 
#'  \item When \code{factors = TRUE} the object returns also inherits \code{factor}
#'  \item Throw an error if \code{x} is not an atomic vector
#'  \item Throw an error if \code{coding} is not a \code{character(1)}
#'  \item Throw an error if \code{factors} is not a \code{logical(1)}
#'  \item Throw an error if \code{suffix} is not a \code{character(1)}
#'  \item Throw an error if \code{checkboxLabels} is not a \code{logical(1)}
#'  \item Throw an error if \code{var_name} is not a \code{character(1)}
#' }

makeRedcapFactor <- function(x, coding, factors, var_name){
  # Argument Validation ---------------------------------------------
  
  coll <- checkmate::makeAssertCollection()
  
  checkmate::assert_atomic_vector(x = x, 
                                  add = coll)
  
  checkmate::assert_character(x = coding, 
                              len = 1, 
                              add = coll)
  
  checkmate::assert_logical(x = factors, 
                            len = 1, 
                            add = coll)
  
  checkmate::assert_character(x = var_name, 
                              len = 1, 
                              add = coll)
  
  checkmate::reportAssertions(coll)
  
  # Functional Code -------------------------------------------------
  
  if (is.na(coding)){
    warning(sprintf("- No coding available for variable `%s`. Data is left in raw form.\n      This may indicate an problem in the Data Dictionary.\n", var_name))
    return(x)
  }
  
  # parses the string "0, Birth \\n 1, Death \\n 2, Unknown" into a
  # character vector for creating a factor
  coding <- unlist(strsplit(coding,"[\n|]"))
  if (length(coding) > 0) 
  {
    coding <- regmatches(coding, regexpr(",", coding), invert = TRUE)
    coding <- do.call("rbind", coding)
    coding <- trimws(coding)
    
    redcapLabels <- coding[, 2]
    redcapLevels <- suppressWarnings(tryCatch(as.integer(coding[, 1]),
                                              warning = function(cond) coding[, 1]))    
    if (factors)
    {
      x <- factor(x,
                  levels=coding[, 1],
                  labels=coding[, 2])

      
      x <- structure(x, 
                     redcapLabels = redcapLabels, 
                     redcapLevels = redcapLevels, 
                     class = c("redcapFactor", class(x)))
    }
    else
    {
      x <- suppressWarnings(tryCatch(as.integer(x), 
                                     warning = function(cond) as.character(x)))
      
      x <- structure(x, 
                     redcapLabels = redcapLabels, 
                     redcapLevels = redcapLevels, 
                     class = c("redcapFactor", class(x)))
    }
  }
  else 
  {
    # Create integer since the meta data about choices are bungled.
    x <- suppressWarnings(as.integer(x))
  }
  x
}

#' @rdname makeRedcapFactor

makeRedcapYN <- function(x, factors){
  coll <- checkmate::makeAssertCollection()
  
  checkmate::assert_atomic_vector(x = x, 
                                  add = coll)
  
  checkmate::assert_logical(x = factors, 
                            len = 1, 
                            add = coll)
  
  checkmate::reportAssertions(coll)
  
  if (factors){
    x <- factor(x, 0:1, c("No", "Yes"))
  }
  
  structure(x, 
            redcapLabels = c("No", "Yes"), 
            redcapLevels = 0:1, 
            class = c("redcapFactor", class(x)))
}

#' @rdname makeRedcapFactor

makeRedcapCheckbox <- function(x, suffix, coding, factors, checkboxLabels)
{
  # Argument Validation ---------------------------------------------
  coll <- checkmate::makeAssertCollection()
  
  checkmate::assert_atomic_vector(x = x, 
                                  add = coll)
  
  checkmate::assert_character(x = suffix, 
                              len = 1, 
                              add = coll)
  
  checkmate::assert_character(x = coding, 
                              len = 1, 
                              add = coll)
  
  checkmate::assert_logical(x = factors, 
                            len = 1, 
                            add = coll)
  
  checkmate::assert_logical(x = checkboxLabels, 
                            len = 1, 
                            add = coll)
  
  checkmate::reportAssertions(coll)
  
  # Functional Code -------------------------------------------------
  
  # parses the string "0, Birth \\n 1, Death \\n 2, Unknown" into a
  # character vector for creating a factor
  coding <- unlist(strsplit(coding,"[\n|]"))
  if (length(coding) > 0) 
  {
    coding <- regmatches(coding, regexpr(",", coding), invert = TRUE)
    coding <- do.call("rbind", coding)
    coding <- trimws(coding)
    coding <- coding[coding[, 1] == suffix, ]
    
    
    use_labels <-
      if (!factors && !checkboxLabels){
        c("0", "1")
      } else if (!factors && checkboxLabels) {
        c("", coding[1])
      } else if (factors && !checkboxLabels) {
        c("Unchecked", "Checked")
      } else if (factors && checkboxLabels) {
        c("", coding[2])
      }
    
    
    if (!factors){
      if (checkboxLabels){
        x <- use_labels[x+1]
      }
      # no else needed.  If checkboxLabels = FALSE, leave as 0/1
      
      x <- structure(x, 
                     redcapLabels = use_labels, 
                     redcapLevels = 0:1, 
                     class = c("redcapFactor", class(x)))
    } else {
      if (!checkboxLabels){
        x <- factor(x,
                    levels = 0:1,
                    labels = c("Unchecked", "Checked"))
      } else { 
        x <- factor(x, 
                    levels = 0:1,
                    labels = use_labels)
      }
      
      x <- structure(x, 
                     redcapLabels = use_labels, 
                     redcapLevels = 0:1, 
                     class = c("redcapFactor", class(x)))
    }
  }
  else 
  {
    # Create integer since the meta data about choices are bungled.
    x <- suppressWarnings(as.integer(x))
  }
  x
}
