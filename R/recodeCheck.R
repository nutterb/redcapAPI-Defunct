recodeCheck <- function(df, vars, 
                        levels=c("Unchecked", "Checked"), labels=c("No", "Yes"), 
                        reverse=FALSE){
  #* reverse the labels.  
  if (reverse){
    levels <- rev(levels)
    labels <- rev(labels)
  }
  
  #* If not variable names are provided, check the data frame for all variables in which all values 
  #* are either "Checked" or "Unchecked"
  if (missing(vars)){
    checkbox <- sapply(df, function(x) all(x %in% levels))
  }
  
  #* If variable names are given, ensure that they are checkbox variables.  Ignore them if anything
  #* other than "Checked" or "Unchecked" appears in the values.
  else {
    vars_are_check <- sapply(df, function(x) all(x %in% levels))
    vars_not_check <- vars[!vars_are_check]
    if (any(!vars_are_check)) warning(paste("'", paste(vars[!vars_are_check], collapse = "', '"), 
                                            "' do not appear to be 'checkbox' variables.",
                                            "\nThese variables were not recoded.", sep=""))
    checkbox <- vars[vars_are_check]
  }
  
  var.label <- label(df[checkbox])
  
  #* Apply the new labels
  df[checkbox] <- lapply(df[checkbox], factor, levels=levels, labels=labels)
  label(df[, checkbox], self=FALSE) <- var.label
  return(df)
}
