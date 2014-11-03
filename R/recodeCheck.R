recodeCheck <- function(df, vars, 
                        old=c("Unchecked", "Checked"), new=c("No", "Yes"), 
                        reverse=FALSE){
    #Hlabel <- require(Hmisc)
    #if (!Hlabel) stop("Please install the 'Hmisc' package.")

  #* If no variable names are provided, check the data frame for all variables in which all values 
  #* are either "Checked" or "Unchecked"
  if (missing(vars)){
    checkbox <- sapply(df, function(x) all(attributes(x)$redcapLabels %in% old))
  }

  #* If variable names are given, ensure that they are checkbox variables.  Ignore them if anything
  #* other than "Checked" or "Unchecked" appears in the values.
  else {
    vars_are_check <- sapply(df, function(x) all(attributes(x)$redcapLabels %in% old))
    vars_not_check <- vars[!vars_are_check]
    if (any(!vars_are_check)) warning(paste0("'", paste(vars[!vars_are_check], collapse = "', '"), 
                                            "' do not appear to be 'checkbox' variables.",
                                            "\nThese variables were not recoded."))
    checkbox <- vars[vars_are_check]
  }

  var.label <- Hmisc::label(df[checkbox])

  #* Utility function for recoding check variables
  recodeFn <- function(v, old=old, new=new, reverse=reverse){
    if (is.factor(v)) v <- redcapFactorFlip(v)
    attributes(v)$redcapLabels <- if (reverse) rev(new) else new
    if (reverse) attributes(v)$redcapLevels <- rev(attributes(v)$redcapLevels)
    return(redcapFactorFlip(v))
  }

  #* Apply the new labels
  df[checkbox] <- lapply(df[checkbox], recodeFn, old, new, reverse)
  Hmisc::label(df[, checkbox], self=FALSE) <- var.label
  return(df)
}
