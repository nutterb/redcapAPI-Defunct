makeRedcapFactor <- function(x, coding, factors)
{
  # parses the string "0, Birth \\n 1, Death \\n 2, Unknown" into a
  # character vector for creating a factor
  coding <- unlist(strsplit(coding,"[\n|]"))
  if (length(coding) > 0) 
  {
    coding <- strsplit(coding,',',perl=TRUE)
    coding <- do.call("rbind", coding)
    coding <- trimws(coding)

    if (factors)
    {
      x <- factor(x,
                  levels=coding[, 1],
                  labels=coding[, 2])
      class(x) <- c("redcapFactor", "factor")
      attr(x,'redcapLabels') <- coding[, 2]
      attr(x,'redcapLevels') <- 
        suppressWarnings(tryCatch(as.integer(coding[, 1]),
                                  warning = function(cond) coding[, 1]))
    }
    else
    {
      x <- suppressWarnings(tryCatch(as.integer(x), 
                                     warning = function(cond) as.character(x)))
      class(x) <- c("redcapFactor", class(x))
      attr(x,'redcapLabels') <- coding[, 2]
      attr(x,'redcapLevels') <- 
        suppressWarnings(tryCatch(as.integer(coding[, 1]),
                                  warning = function(cond) coding[, 1]))
    }
  }
  else 
  {
    # Create integer since the meta data about choices are bungled.
    x <- suppressWarnings(as.integer(x))
  }
  x
}

makeRedcapYN <- function(x, factors)
{
  if (factors)
    x <- factor(x, 0:1, c("No", "Yes"))
  
  class(x) <- c("redcapFactor", class(x))
  attr(x,'redcapLabels') <- c("No", "Yes")
  attr(x,'redcapLevels') <- 0:1
  x
}

makeRedcapCheckbox <- function(x, suffix, coding, factors, checkboxLabels)
{
  # parses the string "0, Birth \\n 1, Death \\n 2, Unknown" into a
  # character vector for creating a factor
  coding <- unlist(strsplit(coding,"[\n|]"))
  if (length(coding) > 0) 
  {
    coding <- strsplit(coding,',',perl=TRUE)
    coding <- do.call("rbind", coding)
    coding <- trimws(coding)
    coding <- coding[coding[, 1] == suffix, ]
    
    
    use_labels <- 
      if (factors & checkboxLabels) 
        c("", coding[2])
      else if (!factors & checkboxLabels)
        c("", coding[1])
    
    if (!factors){
      if (checkboxLabels)
        x <- use_labels[x+1]
      # no else needed.  If checkboxLabels = FALSE, leave as 0/1
      
      class(x) <- c("redcapFactor", class(x))
    }
    else {
      if (!checkboxLabels)
        x <- factor(x,
                    levels = 0:1,
                    labels = c("Unchecked", "Checked"))
      else 
        x <- factor(x, 
                    levels = 0:1,
                    labels = use_labels)
      
      class(x) <- c("redcapFactor", "factor")
    }

    attr(x,'redcapLabels') <- use_labels
    attr(x,'redcapLevels') <- 0:1
  }
  else 
  {
    # Create integer since the meta data about choices are bungled.
    x <- suppressWarnings(as.integer(x))
  }
  x
}
