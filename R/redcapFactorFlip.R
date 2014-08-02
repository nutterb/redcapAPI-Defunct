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
