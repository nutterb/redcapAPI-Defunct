syncUnderscoreCodings <- function(records, meta_data, export=TRUE){
  #* Deterimine if there are any underscores in checkbox codings
  .checkbox <- subset(meta_data, meta_data$field_type %in% c('checkbox'))
  if (nrow(.checkbox) == 0) return(meta_data)
  codings <- strsplit(.checkbox$select_choices_or_calculations, "[|]")
  codings <- lapply(codings, function(x) gsub(",[[:print:]]+", "", x))
  codings <- lapply(codings, function(x) sub(" ", "", x))
  metaUnderscore <- any(sapply(codings, function(x) any(grepl("_", x))))
  
  #* If there are no underscores in checkbox codings, return meta_data.
  #* No futher work needed.
  if (!metaUnderscore) return(meta_data)
  
  
  #* If the function reaches this point, there were underscores in the codings
  #* Now check the variable names in the exported records for underscores in the coding suffixes
  ptrn <- paste0("(", paste(.checkbox$field_name, collapse="|"), ")")
  ptrn_suff <- paste0("(", paste(.checkbox$field_name, "___", sep="", collapse="|"), ")")
  checkNames <- names(records)[grepl(ptrn, names(records))]
  checkNames <- gsub(ptrn_suff, "", checkNames)
  recordUnderscore <- any(grepl("_", checkNames))
  
  #* if underscores are found in the meta_data codings and the records suffixes, return meta_data
  #* No further work needed
  if (metaUnderscore & recordUnderscore) return(meta_data)
  
  
  #* If the function reaches this point, the meta_data codings do not match the record suffixes.
  #* This will remove underscores from the meta_data codings and return the 
  #* meta_data so that it matches the records suffixes.
  oldCoding <- strsplit(.checkbox$select_choices_or_calculations, " [|] ")
  newCoding <- lapply(oldCoding, function(x) do.call("rbind", strsplit(x, ", ")))
  newCoding <- lapply(newCoding, function(x){ x[, 1] <- gsub("_", "", x[,1]); return(x)})
  newCoding <- lapply(newCoding, apply, 1, paste, collapse=", ")
  newCodingStr <- sapply(newCoding, paste, collapse = " | ")
  if (export) meta_data$select_choices_or_calculations[meta_data$field_type == "checkbox"] <- newCodingStr
  
  field_names <- cbind(rep(meta_data$field_name[meta_data$field_type == "checkbox"], sapply(oldCoding, length)), 
                       gsub(",[[:print:]]+", "", unlist(oldCoding)), 
                       gsub(",[[:print:]]+", "", unlist(newCoding)))
  field_names <- cbind(paste(field_names[, 1], field_names[, 2], sep="___"),
                       paste(field_names[, 1], field_names[, 3], sep="___"))
  attr(meta_data, "checkbox_field_name_map") <- field_names
  return(meta_data)
}
