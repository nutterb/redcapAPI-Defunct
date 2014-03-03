syncUnderscoreCodings <- function(records, meta_data){
  #* Deterimine if there are any underscores in checkbox codings
  .checkbox <- subset(meta_data, field_type %in% c('checkbox'))
  codings <- strsplit(.checkbox$select_choices_or_calculations, "[|]")
  codings <- lapply(codings, function(x) gsub(",[[:print:]]+", "", x))
  codings <- lapply(codings, function(x) sub(" ", "", x))
  metaUnderscore <- any(sapply(codings, function(x) any(grepl("_", x))))
  
  #* If there are no underscores in checkbox codings, return meta_data.
  #* No futher work needed.
  if (!metaUnderscore) return(meta_data)
  
  
  #* If the function reaches this point, there were underscores in the codings
  #* Now check the variable names in the exported records for underscores in the coding suffixes
  ptrn <- paste("(", paste(.checkbox$field_name, collapse="|"), ")", sep="")
  ptrn_suff <- paste("(", paste(.checkbox$field_name, "___", sep="", collapse="|"), ")", sep="")
  checkNames <- names(records)[grepl(ptrn, names(records))]
  checkNames <- gsub(ptrn_suff, "", checkNames)
  recordUnderscore <- any(grepl("_", checkNames))
  
  #* if underscores are found in the meta_data codings and the records suffixes, return meta_data
  #* No further work needed
  if (metaUnderscore & recordUnderscore) return(meta_data)
  
  
  #* If the function reaches this point, the meta_data codings do not match the record suffixes.
  #* This will remove underscores from the meta_data codings and return the 
  #* meta_data so that it matches the records suffixes.
  newCoding <- strsplit(.checkbox$select_choices_or_calculations, "[|]")
  newCoding <- lapply(newCoding, function(x) do.call("rbind", strsplit(x, ", ")))
  newCoding <- lapply(newCoding, function(x){ x[, 1] <- gsub("_", "", x[,1]); return(x)})
  newCoding <- lapply(newCoding, apply, 1, paste, collapse=", ")
  newCoding <- sapply(newCoding, paste, collapse = "|")
  meta_data$select_choices_or_calculations[meta_data$field_type == "checkbox"] <- newCoding
  return(meta_data)
}
    
  
