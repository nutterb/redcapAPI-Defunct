validateImport <- function(field, meta_data, records, ids,
                           logfile=""){
  x <- records[, field]
  field_meta <- sub("___[a-z,A-Z,0-9,_]+", "", field)
  meta_data <- subset(meta_data, meta_data$field_name == field_meta)
  
  printLog <- function(x, file=logfile){
    suppressWarnings(write.table(x, file, append=TRUE, sep="   ", row.names=FALSE, col.names=FALSE, quote=FALSE))
  }
    
  #*** Form complete fields
  if (nrow(meta_data) == 0){
    if (is.character(x) | is.factor(x)){
      w <- which(!grepl("(0|1|2|Incomplete|Unverified|Complete)", x))
      if (length(w) > 0){
        complt_fld_msg <- records[w, c(ids, field), drop=FALSE]
        complt_fld_msg$msg <- paste("Entry in '", field, "' must be either 0, 1, 2, Incomplete, Unverified, or Complete. Value not imported", sep="")
        x[w] <- NA
      }
      x <- gsub("Incomplete", "0", x)
      x <- gsub("Unverified", "1", x)
      x <- gsub("Complete", "2", x)
    }
    if (is.numeric(x)){
      w <- which(!x %in% 0:2)
      if (length(w) > 0){
        complt_fld_msg <- records[w, c(ids, field), drop=FALSE]
        complt_fld_msg$msg <- paste("Entry in '", field, "' must be either 0, 1, 2, Incomplete, Unverified, or Complete. Value not imported", sep="")
        x[w] <- NA  
      }
    }
    return(x)    
  }
  
  #****************************
  #* Date variables
  else if (grepl("date_", meta_data$text_validation_type_or_show_slider_number)){
    if (is.character(x)){
      w <- which(!grepl("(\\d{1,2}/\\d{1,2}/\\d{2,4}|\\d{4}[/,-]\\d{1,2}[/,-]\\d{1,2}|\\d{4}\\d{2}\\d{2})", x) & !is.na(x))
      if (length(w) > 0){
        date_fmt_msg <- records[w, c(ids, field), drop=FALSE]
        date_fmt_msg$msg <- paste("Date entry in '", field, "' were NOT imported!  \n      These should be either POSIXct class, Date class, ",
                                  "or character class with format mm/dd/YYYY, YYYY/mm/dd, YYYY-mm-dd, or YYYYmmdd.", sep="")
        printLog(date_fmt_msg, logfile)
        x[w] <- NA
      }
      x <- ifelse(grepl("\\d{1,2}/\\d{1,2}/\\d{4}", x), as.POSIXct(x, format="%m/%d/%Y"),
            ifelse(grepl("\\d{1,2}/\\d{1,2}/\\d{2}", x), as.POSIXct(x, format="%m/%d/%y"),
             ifelse(grepl("\\d{4}/\\d{1,2}/\\d{1,2}", x), as.POSIXct(x, format="%Y/%m/%d"),
              ifelse(grepl("\\d{4}-\\d{1,2}-\\d{1,2}", x), as.POSIXct(x, format="%Y-%m-%d"),
               ifelse(grepl("\\d{4}\\d{2}\\d{2}", x), as.POSIXct(x, format="%Y%m%d"), NA)))))
      x <- as.POSIXct(x, origin=as.POSIXct("1/1/1970", format="%m/%d/%Y"))
    }
    
    if ("Date" %in% class(x)){
      x <- as.POSIXct(format(x, format="%m/%d/%Y"), format="%m/%d/%Y") 
    }
    
    if (!is.na(meta_data$text_validation_min)){
      w <- which(x < as.POSIXct(meta_data$text_validation_min, format="%Y-%m-%d") & !is.na(x))
      if (length(w) > 0){
        date_min_msg <- records[w, c(ids, field), drop=FALSE]
        date_min_msg$msg <- paste("Entry for '", field, "' is before the acceptable minimum.  Please confirm.", sep="")
        printLog(date_min_msg, logfile)
      }
    }
    
    if (!is.na(meta_data$text_validation_max)){
      w <- which(x < as.POSIXct(meta_data$text_validation_max, format="%Y-%m-%d") & !is.na(x))
      if (length(w) > 0){
        date_max_msg <- records[w, c(ids, field), drop=FALSE]
        date_max_msg$msg <- paste("Entry for '", field, "' is after the acceptable maximum.  Please confirm.", sep="")
        printLog(date_max_msg, logfile)
      }
    }
    
    x <- format(x, format="%Y-%m-%d")
    
    return(x)
  }
  
  #****************************************************
  #* Continuous variables
  else if (grepl("(float|integer|number|number_1dp)", meta_data$text_validation_type_or_show_slider_number) || 
             grepl("calc", meta_data$field_type)){
    if (grepl("integer", meta_data$text_validation_type_or_show_slider_number)){
      w <- which(!is.integer(x) & !is.na(x))
      if (length(w) > 0){
        not_int_msg <- records[w, c(ids, field), drop=FALSE]
        not_int_msg$msg <- paste("Entry for '", field, "' is not an integer and was coerced to an integer.", sep="")
        printLog(not_int_msg, logfile)
        x <- as.integer(x)
      }
    }
    w <- which(!is.numeric(x) & !is.na(x))
    if (length(w) > 0){
      not_num_msg <- records[w, c(ids, field), drop=FALSE]
      not_num_msg$msg <- paste("Entry for '", field, "' was not numeric and was coerced to a numeric.", sep="")
      printLog(not_num_msg, logfile)
      x <- as.numeric(x)
    }
    if (grepl("number_1dp", meta_data$text_validation_type_or_show_slider_number)){
      x <- round(x, 1)
    }
    
    return(x)
  }
  
  
  #*****************************************
  #** ZIP codes
  else if (grepl("zipcode", meta_data$text_validation_type_or_show_slider_number)){
    x <- as.character(x)
    w <- which(!grepl("(\\d{5}|\\d{5}-\\d{4})", x) & !is.na(x))
    if (length(w) > 0){
      bad_zip_msg <- records[w, c(ids, field), drop=FALSE]
      bad_zip_msg$msg <- paste("Entry for '", field, "' is not a valid ZIP code and was not imported.")
      printLog(bad_zip_msg, logfile)
      x[w] <- ""
    }

    return(x)
  }
  
  
  #********************************************************
  #* yesno fields
  else if (grepl("yesno", meta_data$field_type)){
    if (is.character(x) || is.factor(x)){
      x <- as.character(x)
      x <- tolower(x)
      w <- which(!x %in% c("no", "yes", "0", "1") & !is.na(x))
      if (length(w) > 0){
        bad_yn_msg <- records[w, c(ids, field), drop=FALSE]
        bad_yn_msg$msg <- paste("Entry for '", field, "' must be either no, yes, 0, or 1.  No value was imported.", sep="")
        printLog(bad_yn_msg, logfile)
        x[w] <- NA
      }
      x[x %in% "no"] <- "0"
      x[x %in% "yes"] <- "1"
    }
    else if (is.numeric(x)){
      w <- which(!x %in% 0:1 & !is.na(x))
      if (length(w) > 0){
        bad_yn_msg <- records[w, c(ids, field), drop=FALSE]
        bad_yn_msg$msg <- paste("Entry for '", field, "' must be either no, yes, 0, or 1.", sep="")
        printLog(bad_yn_msg, logfile)
        x[!x %in% 0:1] <- NA
      }
    }
    return(x)  
  }
  
  
  #********************************************************
  #* truefalse fields
  else if (grepl("truefalse", meta_data$field_type)){
    if (is.character(x) || is.factor(x)){
      x <- as.character(x)
      x <- tolower(x)
      w <- which(!x %in% c("true", "false") & !is.na(x))
      if (length(w) > 0){
        bad_yn_msg <- records[w, c(ids, field), drop=FALSE]
        bad_yn_msg$msg <- paste("Entry for '", field, "' must be either no, yes, 0, or 1.  No value was imported.", sep="")
        printLog(bad_yn_msg, logfile)
        x <- as.character(factor(x, c("no", "yes"), 0:1))
      }
    }
    else if (is.numeric(x)){
      w <- which(!x %in% 0:1 & !is.na(x))
      if (length(w) > 0){
        bad_yn_msg <- records[w, c(ids, field), drop=FALSE]
        bad_yn_msg$msg <- paste("Entry for '", field, "' must be either no, yes, 0, or 1.", sep="")
        printLog(bad_yn_msg, logfile)
        x[!x %in% 0:1] <- NA
      }
    }
    else if (is.logical(x)){
      x <- as.numeric(x) 
    }
    return(x)  
  }
  
  
  #********************************************************
  #* radio and dropdown fields
  else if (grepl("(select|radio|dropdown)", meta_data$field_type)){
    x <- as.character(x)
    mapping <- do.call("rbind", strsplit(unlist(strsplit(meta_data$select_choices_or_calculations, " [|] ")), ", "))
    
    w <- which(!x %in% mapping[, 2] & !is.na(x))
    if (length(w) > 0){
      radio_msg <- records[w, c(ids, field), drop=FALSE]
      radio_msg$msg <- paste("Entry for '", field, "' must be either one of: ", paste(mapping[, 1], collapse=", "), ".", sep="")
      printLog(radio_msg, logfile)
      x[w] <- NA
    }
    
    x <- as.character(factor(x, mapping[, 2], mapping[, 1]))
    return(x)
  }
  
  
  #********************************************************
  #* checkbox fields
  else if (grepl("checkbox", meta_data$field_type)){
    x <- as.character(x)

    
    w <- which(!x %in% c("Checked", "Unchecked", "0", "1") & !is.na(x))
    if (length(w) > 0){
      check_msg <- records[w, c(ids, field), drop=FALSE]
      check_msg$msg <- paste("Entry for '", field, "' must be either one of: 0, 1, Checked, Unchecked.", sep="")
      printLog(check_msg, logfile)
    }
    x[x %in% "Checked"] <- "1"
    x[x %in% "Unchecked"] <- "0"
    x[!x %in% c("0", "1")] <- NA
    return(x)
  }
  
  return(x)
  
}

