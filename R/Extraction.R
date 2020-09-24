#' @name Extraction
#' @title Extraction and Assignment for \code{redcapFactor}s
#' 
#' @description Extract elements and make assignments to \code{redcapFactor}s
#' 
#' @param x an object of class \code{redcapFactor}
#' @param drop \code{logical}. If \code{TRUE}, unused levels are dropped.
#' @param ... additional arguments to pass to other methods
#' 
#' @export

`[.redcapFactor` <- function(x, ..., drop = FALSE){
  redcap_labels <- attr(x, "redcapLabels")
  redcap_levels <- attr(x, "redcapLevels")
  
  has_label <- labelVector::is.labelled(x)
  
  if (has_label)
    label <- labelVector::get_label(x)
  
  class(x) <- class(x)[!class(x) %in% c("labelled", "redcapFactor")]
  
  x <- x[..., drop = drop]
  
  attr(x, "redcapLabels") <- redcap_labels
  attr(x, "redcapLevels") <- redcap_levels
  if (has_label)
    x <- labelVector::set_label(x, label)
  x
}

#' @rdname Extraction
#' @export

print.redcapFactor <- function(x, ...){
  attr(x, "redcapLabels") <- NULL
  attr(x, "redcapLevels") <- NULL
  
  print.factor(x)
}

