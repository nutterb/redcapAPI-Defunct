#' @name syncUnderscoreCodings
#' 
#' @title Sychronize coding of checkbox variables between meta data and 
#'   records field names.
#' @description Due to a bug in the REDCap export module, underscores in 
#'   checkbox codings are not retained in the suffixes of the field names 
#'   in the exported records.  For example, if variable \code{chk} is a 
#'   checkbox with a coding 'a_b, A and B', the field name in the data 
#'   export becomes \code{chk___ab}.  The loss of the underscore causes
#'   \code{fieldToVar} to fail as it can't match variable names to the 
#'   meta data.  \code{syncUnderscoreCodings} rectifies this problem by 
#'   searching the suffixes and meta data for underscores.  If a 
#'   discrepancy is found, the underscores are removed from the metadata 
#'   codings, restoring harmony to the universe.  This bug was fixed in 
#'   REDCap version 5.5.21 and this function does not apply to that and 
#'   later versions.
#'   
#' @param records The data frame object returned from the API export 
#'   prior to applying factors, labels, and dates via the \code{fieldToVar} 
#'   function.
#' @param meta_data Metadata export from \code{exportMetaData}
#' @param export Logical.  Specifies if data are being synchronized for 
#'   import or export
#'
#' @details 
#' \code{syncUnderscoreCodings} performs a series of evaluations.  First, it 
#' determines if any underscores are found  in the checkbox codings.  
#' If none are found, the function terminates without changing anything.
#' 
#' If the checkbox codings have underscores, the next evaluation is to 
#' determine if the variable names suffixes have matching underscores.  
#' If they do, then the function terminates with no changes to the meta data.
#' 
#' For data exports, if the prior two checks find underscores in the meta data 
#' and no underscores in the suffixes, the underscores are removed from the 
#' meta data and the new meta data returned.
#' 
#' For data imports, the meta data are not altered and the 
#' \code{checkbox_field_name_map} attribute is used to synchronize field 
#' names to the meta data and the expectations of REDCap (for import, 
#' REDCap expects the underscore codings to be used.
#' 
#' @author Benjamin Nutter
#' 

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
