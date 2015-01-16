#' @name validateImport
#' @importFrom chron times
#' 
#' @title Validate Data Frames for Import
#' @description Validates the variables in a data frame prior to attempting 
#'   an import to REDCap
#'   
#' @param field Character(1) naming the variable to be validated.
#' @param meta_data REDCap database meta data.
#' @param records The data frame to be validated.
#' @param ids Character vector giving the names of fields that uniquely 
#'   identify a record.  Usually the study id and \code{redcap_event_name}.
#' @param logfile A character string giving the filepath to which the 
#'   results of the validation are printed.  If \code{""}, the results 
#'   are printed in the console.
#'   
#' @details
#' \code{validateImport} is called internally by \code{importRecords} and is 
#' not available to the user.
#' 
#' Each variable is validated by matching they type of variable with the type 
#' listed in the REDCap database.  
#' 
#' Although the log messages will indicate a preference for dates to be in 
#' mm/dd/yyyy format, the function will accept mm/dd/yy, yyyy-mm-dd, 
#' yyyy/mm/dd, and yyyymmdd formats as well.  When possible, pass dates as 
#' Date objects or POSIXct objects to avoid confusion.  Dates are also compared 
#' to minimum and maximum values listed in the data dictionary.  Records where 
#' a date is found out of range are allowed to import and a message 
#' is printed in the log.
#' 
#' For continuous/numeric variables, the values are checked against the 
#' minimum and maximum allowed in the data dictionary. Records where a value 
#' is found out of range are allowed to import and a message is printed 
#' in the log.
#' 
#' ZIP codes are tested to see if they fit either the 5 digit or 
#' 5 digit + 4 format.  When these conditions are not met, the data point is 
#' deleted and a message printed in the log.
#' 
#' YesNo fields permit any of the values 'yes', 'no', '0', '1' to be imported 
#' to REDCap with 0=No, and 1=Yes.  The values are converted to lower case 
#' for validation, so any combination of lower and upper case values 
#' will pass (ie, the data frame is not case-sensitive).
#' 
#' TrueFalse fields will accept 'TRUE', 'FALSE', 0, 1, and logical values 
#' and are also not case-sensitive.
#' 
#' Radio and dropdown fields may have either the coding in the data 
#' dictionary or the labels in the data dictionary. The validation will use 
#' the meta data to convert any matching values to the appropriate coding 
#' before importing to REDCap.  Values that cannot be reconciled are 
#' deleted with a message printed in the log.  Currently, these variables
#' \emph{are} case-sensitive.
#' 
#' Checkbox fields require a value of "Checked", "Unchecked", "0", or "1".  
#' These are currently case sensitive.  Values that do not match these are 
#' deleted with a warning printed in the log.
#' 
#' Phone numbers are required to be 10 digit numbers.  The phone number is 
#' broken into three parts: 1) a 3 digit area code, 2) a 3 digit exchange code, 
#' and 3) a 4 digit station code.  The exchange code must start with a number 
#' from 2-9, followed by 0-8, and then any third digit.  
#' The exchange code starts with a number from 2-9, followed by any two 
#' digits. The station code is 4 digits with no restrictions.
#' 
#' E-mail addresses are considered valid when they have three parts. The first 
#' part comes before the @@ symbol, and may be number of characters from 
#' a-z, A-Z, a period, underscore, percent, plus, or minus.  The second part
#' comes after the @@, but before the period, and may consist of any 
#' number of letters, numbers, periods, or dashes.  Finally, the string ends 
#' with a period then anywhere from 2 to 6 letters.
#' 
#' @author Benjamin Nutter
#' 
#' @references
#' See the REDCap Help and FAQ page's section on 'Text Validation Types'
#' 
#' Validating e-mail addresses
#' \url{http://www.regular-expressions.info/email.html}
#' 

validateImport <- function(field, meta_data, records, ids,
                           logfile=""){
  x <- records[, field]
  field_meta <- sub("___[a-z,A-Z,0-9,_]+", "", field)
  meta_data <- subset(meta_data, meta_data$field_name == field_meta)
  
  printLog <- function(x, file=logfile){
    suppressWarnings(write.table(x, file, append=TRUE, sep="   ", row.names=FALSE, col.names=FALSE, quote=FALSE))
  }
  
  #*** fields with all missing values are not altered
  #*** NA's are imported as blanks, and a field of all NA's
  #*** has the potential to cause errors in other validations
  #*** (especially with date and date/time fields(
  if (all(is.na(x))) return(rep('', length(x)))
  
  #*** Form complete fields
  else if (nrow(meta_data) == 0){
    if (is.character(x) | is.factor(x)){
      w <- which(!grepl("(0|1|2|Incomplete|Unverified|Complete)", x))
      if (length(w) > 0){
        complt_fld_msg <- records[w, c(ids, field), drop=FALSE]
        complt_fld_msg$msg <- paste0("Entry in '", field, "' must be either 0, 1, 2, Incomplete, Unverified, or Complete. Value not imported")
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
        complt_fld_msg$msg <- paste0("Entry in '", field, "' must be either 0, 1, 2, Incomplete, Unverified, or Complete. Value not imported")
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
        date_fmt_msg$msg <- paste0("Date entry in '", field, "' were NOT imported!  \n      These should be either POSIXct class, Date class, ",
                                  "or character class with format mm/dd/YYYY, YYYY/mm/dd, YYYY-mm-dd, or YYYYmmdd.")
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
        date_min_msg$msg <- paste0("Entry for '", field, "' is before the acceptable minimum.  Please confirm.")
        printLog(date_min_msg, logfile)
      }
    }
    
    if (!is.na(meta_data$text_validation_max)){
      w <- which(x > as.POSIXct(meta_data$text_validation_max, format="%Y-%m-%d") & !is.na(x))
      if (length(w) > 0){
        date_max_msg <- records[w, c(ids, field), drop=FALSE]
        date_max_msg$msg <- paste0("Entry for '", field, "' is after the acceptable maximum.  Please confirm.")
        printLog(date_max_msg, logfile)
      }
    }
    
    x <- format(x, format="%Y-%m-%d")
    
    return(x)
  }
  
  #****************************************************
  #* Date Time Variables
  else if (grepl("datetime_dmy", meta_data$text_validation_type_or_show_slider_number)){
    if (is.character(x)){
      w <- which(!grepl(paste("(\\d{1,2}/\\d{1,2}/\\d{2,4} \\d{2}[:]\\d{2}",
                               "\\d{4}[/,-]\\d{1,2}[/,-]\\d{1,2} \\d{2}[:]\\d{2}",
                               "\\d{4}\\d{2}\\d{2} \\d{2}[:]\\d{2}", 
                               "\\d{1,2}/\\d{1,2}/\\d{2,4} \\d{2}[:]\\d{2} \\w{2}",
                               "\\d{4}[/,-]\\d{1,2}[/,-]\\d{1,2} \\d{2}[:]\\d{2} \\w{2}",
                               "\\d{4}\\d{2}\\d{2} \\d{2}[:]\\d{2} \\w{2})",  sep="|"),                       
                              x) & !is.na(x))
      if (length(w) > 0){
        date_fmt_msg <- records[w, c(ids, field), drop=FALSE]
        date_fmt_msg$msg <- paste0("Date entry in '", field, "' were NOT imported!  \n      These should be either POSIXct class, Date class, ",
                                  "or character class with format mm/dd/YYYY HH:MM, YYYY/mm/dd HH:MM, YYYY-mm-dd HH:MM, or YYYYmmdd HH:MM",
                                  "with an optional AM/PM on the end.")
        printLog(date_fmt_msg, logfile)
        x[w] <- NA
      }
      x <- ifelse(grepl("\\d{1,2}/\\d{1,2}/\\d{4} \\d{2}[:]\\d{2}", x), as.POSIXct(x, format="%m/%d/%Y %I:%M"),
             ifelse(grepl("\\d{1,2}/\\d{1,2}/\\d{2} \\d{2}[:]\\d{2}", x), as.POSIXct(x, format="%m/%d/%y %I:%M"),
               ifelse(grepl("\\d{4}/\\d{1,2}/\\d{1,2} \\d{2}[:]\\d{2}", x), as.POSIXct(x, format="%Y/%m/%d %I:%M"),
                 ifelse(grepl("\\d{4}-\\d{1,2}-\\d{1,2} \\d{2}[:]\\d{2}", x), as.POSIXct(x, format="%Y-%m-%d %I:%M"),
                   ifelse(grepl("\\d{4}\\d{2}\\d{2} \\d{2}[:]\\d{2}", x), as.POSIXct(x, format="%Y%m%d %I:%M"), 
                     ifelse(grepl("\\d{1,2}/\\d{1,2}/\\d{4} \\d{2}[:]\\d{2} \\w{2}", x), as.POSIXct(x, format="%m/%d/%Y %H:%M %p"),
                       ifelse(grepl("\\d{1,2}/\\d{1,2}/\\d{2} \\d{2}[:]\\d{2} \\w{2}", x), as.POSIXct(x, format="%m/%d/%y %H:%M %p"),
                         ifelse(grepl("\\d{4}/\\d{1,2}/\\d{1,2} \\d{2}[:]\\d{2} \\w{2}", x), as.POSIXct(x, format="%Y/%m/%d %H:%M %p"),
                           ifelse(grepl("\\d{4}-\\d{1,2}-\\d{1,2} \\d{2}[:]\\d{2} \\w{2}", x), as.POSIXct(x, format="%Y-%m-%d %H:%M %p"),
                             ifelse(grepl("\\d{4}\\d{2}\\d{2} \\d{2}[:]\\d{2} \\w{2}", x), as.POSIXct(x, format="%Y%m%d %H:%M %p"),
                                    NA))))))))))
      x <- as.POSIXct(x, origin=as.POSIXct("1/1/1970", format="%m/%d/%Y %I:%M"))
    }
    
    if ("Date" %in% class(x)){
      x <- as.POSIXct(format(x, format="%m/%d/%Y %I:%M"), format="%m/%d/%Y %I:%M") 
    }
    
    if (!is.na(meta_data$text_validation_min)){
      w <- which(x < as.POSIXct(meta_data$text_validation_min, format="%Y-%m-%d %I:%M") & !is.na(x))
      if (length(w) > 0){
        date_min_msg <- records[w, c(ids, field), drop=FALSE]
        date_min_msg$msg <- paste0("Entry for '", field, "' is before the acceptable minimum.  Please confirm.")
        printLog(date_min_msg, logfile)
      }
    }
    
    if (!is.na(meta_data$text_validation_max)){
      w <- which(x > as.POSIXct(meta_data$text_validation_max, format="%Y-%m-%d %I:%M") & !is.na(x))
      if (length(w) > 0){
        date_max_msg <- records[w, c(ids, field), drop=FALSE]
        date_max_msg$msg <- paste0("Entry for '", field, "' is after the acceptable maximum.  Please confirm.")
        printLog(date_max_msg, logfile)
      }
    }
    
    x <- format(x, format="%Y-%m-%d %I:%M")
    
    return(x)
  }
  
  #****************************************************
  #* Date Time Variables with Seconds
  else if (grepl("datetime_seconds", meta_data$text_validation_type_or_show_slider_number)){
    if (is.character(x)){
      w <- which(!grepl(paste("(\\d{1,2}/\\d{1,2}/\\d{2,4} \\d{2}[:]\\d{2}",
                              "\\d{4}[/,-]\\d{1,2}[/,-]\\d{1,2} \\d{2}[:]\\d{2}",
                              "\\d{4}\\d{2}\\d{2} \\d{2}[:]\\d{2}", 
                              "\\d{1,2}/\\d{1,2}/\\d{2,4} \\d{2}[:]\\d{2} \\w{2}",
                              "\\d{4}[/,-]\\d{1,2}[/,-]\\d{1,2} \\d{2}[:]\\d{2} \\w{2}",
                              "\\d{4}\\d{2}\\d{2} \\d{2}[:]\\d{2} \\w{2})",  sep="|"),                       
                        x) & !is.na(x))
      if (length(w) > 0){
        date_fmt_msg <- records[w, c(ids, field), drop=FALSE]
        date_fmt_msg$msg <- paste0("Date entry in '", field, "' were NOT imported!  \n      These should be either POSIXct class, Date class, ",
                                  "or character class with format mm/dd/YYYY HH:MM:SS, YYYY/mm/dd HH:MM:SS, YYYY-mm-dd HH:MM:SS, or YYYYmmdd HH:MM:SS",
                                  "with an optional AM/PM on the end.")
        printLog(date_fmt_msg, logfile)
        x[w] <- NA
      }
      x <- ifelse(grepl("\\d{1,2}/\\d{1,2}/\\d{4} \\d{2}[:]\\d{2}:\\{2}", x), as.POSIXct(x, format="%m/%d/%Y %I:%M:%S"),
             ifelse(grepl("\\d{1,2}/\\d{1,2}/\\d{2} \\d{2}[:]\\d{2}:\\{2}", x), as.POSIXct(x, format="%m/%d/%y %I:%M:%S"),
               ifelse(grepl("\\d{4}/\\d{1,2}/\\d{1,2} \\d{2}[:]\\d{2}:\\{2}", x), as.POSIXct(x, format="%Y/%m/%d %I:%M:%S"),
                 ifelse(grepl("\\d{4}-\\d{1,2}-\\d{1,2} \\d{2}[:]\\d{2}:\\{2}", x), as.POSIXct(x, format="%Y-%m-%d %I:%M:%S"),
                   ifelse(grepl("\\d{4}\\d{2}\\d{2} \\d{2}[:]\\d{2}:\\{2}", x), as.POSIXct(x, format="%Y%m%d %I:%M:%S"), 
                     ifelse(grepl("\\d{1,2}/\\d{1,2}/\\d{4} \\d{2}[:]\\d{2}:\\{2} \\w{2}", x), as.POSIXct(x, format="%m/%d/%Y %H:%M:%S %p"),
                       ifelse(grepl("\\d{1,2}/\\d{1,2}/\\d{2} \\d{2}[:]\\d{2}:\\{2} \\w{2}", x), as.POSIXct(x, format="%m/%d/%y %H:%M:%S %p"),
                         ifelse(grepl("\\d{4}/\\d{1,2}/\\d{1,2} \\d{2}[:]\\d{2}:\\{2} \\w{2}", x), as.POSIXct(x, format="%Y/%m/%d %H:%M:%S %p"),
                           ifelse(grepl("\\d{4}-\\d{1,2}-\\d{1,2} \\d{2}[:]\\d{2}:\\{2} \\w{2}", x), as.POSIXct(x, format="%Y-%m-%d %H:%M:%S %p"),
                             ifelse(grepl("\\d{4}\\d{2}\\d{2} \\d{2}[:]\\d{2}:\\{2} \\w{2}", x), as.POSIXct(x, format="%Y%m%d %H:%M:%S %p"),
                                                                                 NA))))))))))
      x <- as.POSIXct(x, origin=as.POSIXct("1/1/1970", format="%m/%d/%Y %I:%M:%S"))
    }
    
    if ("Date" %in% class(x)){
      x <- as.POSIXct(format(x, format="%m/%d/%Y %I:%M:%S"), format="%m/%d/%Y %I:%M:%S") 
    }
    
    if (!is.na(meta_data$text_validation_min)){
      w <- which(x < as.POSIXct(meta_data$text_validation_min, format="%Y-%m-%d %I:%M:%S") & !is.na(x))
      if (length(w) > 0){
        date_min_msg <- records[w, c(ids, field), drop=FALSE]
        date_min_msg$msg <- paste0("Entry for '", field, "' is before the acceptable minimum.  Please confirm.")
        printLog(date_min_msg, logfile)
      }
    }
    
    if (!is.na(meta_data$text_validation_max)){
      w <- which(x > as.POSIXct(meta_data$text_validation_max, format="%Y-%m-%d %I:%M:%S") & !is.na(x))
      if (length(w) > 0){
        date_max_msg <- records[w, c(ids, field), drop=FALSE]
        date_max_msg$msg <- paste0("Entry for '", field, "' is after the acceptable maximum.  Please confirm.")
        printLog(date_max_msg, logfile)
      }
    }
    
    x <- format(x, format="%Y-%m-%d %I:%M:%S")
    
    return(x)
  }
  
  #****************************************************
  #* Time variables (MM:SS)
  else if (grepl("time_mm_ss", meta_data$text_validation_type_or_show_slider_number)){
  #* convert times to character to ensure valid format
    x <- as.character(x)
    w <- which(!grepl("(00:\\d{2}:\\d{2}|\\d{2}:\\d{2})", x) & !is.na(x))
    x <- sapply(strsplit(x, ":"), tail, 2)
    x <- sapply(x, paste, collapse=":")
    if (length(w) > 0){
      not_time_mmss_msg <- records[w, c(ids, field), drop=FALSE]
      not_time_mmss_msg$msg <- paste0("Entry for '", field, "' is not in 00:MM:SS or MM:SS format.")
      printLog(not_time_mmss_msg, logfile)
      x[w] <- NA
    }
    x[x == "NA"] <- NA

    # reconvert to times objects to compare to meta data
    x <- suppressWarnings(chron::times(paste("00", x, sep=":"), format=c(times="h:m:s")))
    
    if (!is.na(meta_data$text_validation_min)){
      w <- which(x < chron::times(paste("00", meta_data$text_validation_min, sep=":")) & !is.na(x))
      if (length(w) > 0){
        time_mmss_msg <- records[w, c(ids, field), drop=FALSE]
        time_mmss_msg$msg <- paste0("Entry for '", field, "' is before the acceptable minimum.  Please confirm.")
        printLog(time_mmss_msg, logfile)
      }
    }
    
    if (!is.na(meta_data$text_validation_max)){
      w <- which(x > chron::times(paste("00", meta_data$text_validation_min, sep=":")) & !is.na(x))
      if (length(w) > 0){
        time_mmss_msg <- records[w, c(ids, field), drop=FALSE]
        time_mmss_msg$msg <- paste0("Entry for '", field, "' is after the acceptable maximum.  Please confirm.")
        printLog(time_mmss_msg, logfile)
      }
    }
    
    # Convert to character again for upload.
    x <- as.character(x)
    x <- sapply(strsplit(x, ":"), tail, 2)
    x <- sapply(x, paste, collapse=":")
    x[x == "NA"] <- NA
    
    return(x)
  }
  
  #****************************************************
  #* Time variables (HH:MM)
  else if ("time" %in% meta_data$text_validation_type_or_show_slider_number){
    #* convert times to character to ensure valid format
    x <- as.character(x)
    w <- which(!grepl("(\\d{2}:\\d{2}:00|\\d{2}:\\d{2})", x) & !is.na(x))
    x <- sapply(strsplit(x, ":"), head, 2)
    x <- sapply(x, paste, collapse=":")
    if (length(w) > 0){
      not_time_mmss_msg <- records[w, c(ids, field), drop=FALSE]
      not_time_mmss_msg$msg <- paste0("Entry for '", field, "' is not in HH:MM:00 or HH:MM format.")
      printLog(not_time_mmss_msg, logfile)
      x[w] <- NA
    }
    x[x == "NA"] <- NA
    
    # reconvert to times objects to compare to meta data
    x <- suppressWarnings(chron::times(paste(x, "00", sep=":"), format=c(times="h:m:s")))
    
    if (!is.na(meta_data$text_validation_min)){
      w <- which(x < chron::times(paste(meta_data$text_validation_min, "00", sep=":")) & !is.na(x))
      if (length(w) > 0){
        time_hhmm_msg <- records[w, c(ids, field), drop=FALSE]
        time_hhmm_msg$msg <- paste0("Entry for '", field, "' is before the acceptable minimum.  Please confirm.")
        printLog(time_hhmm_msg, logfile)
      }
    }
    
    if (!is.na(meta_data$text_validation_max)){
      w <- which(x > chron::times(paste(meta_data$text_validation_min, "00", sep=":")) & !is.na(x))
      if (length(w) > 0){
        time_hhmm_msg <- records[w, c(ids, field), drop=FALSE]
        time_hhmm_msg$msg <- paste0("Entry for '", field, "' is after the acceptable maximum.  Please confirm.")
        printLog(time_hhmm_msg, logfile)
      }
    }
    
    # Convert to character again for upload.
    x <- as.character(x)
    x <- sapply(strsplit(x, ":"), head, 2)
    x <- sapply(x, paste, collapse=":")
    x[x == "NA"] <- NA
    
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
        not_int_msg$msg <- paste0("Entry for '", field, "' is not an integer and was coerced to an integer.")
        printLog(not_int_msg, logfile)
        x <- as.integer(x)
      }
    }
    w <- which(!is.numeric(x) & !is.na(x))
    if (length(w) > 0){
      not_num_msg <- records[w, c(ids, field), drop=FALSE]
      not_num_msg$msg <- paste0("Entry for '", field, "' was not numeric and was coerced to a numeric.")
      printLog(not_num_msg, logfile)
      x <- as.numeric(x)
    }
    if (grepl("number_1dp", meta_data$text_validation_type_or_show_slider_number)){
      x <- round(x, 1)
    }
    
    if (!is.na(meta_data$text_validation_min)){
      w <- which(x < meta_data$text_validation_min & !is.na(x))
      if (length(w) > 0){
        num_min_msg <- records[w, c(ids, field), drop=FALSE]
        num_min_msg$msg <- paste0("Entry for '", field, "' is smaller than the acceptable minimum.  Please confirm.")
        printLog(num_min_msg, logfile)
      }
    }
    
    if (!is.na(meta_data$text_validation_max)){
      w <- which(x > meta_data$text_validation_max & !is.na(x))
      if (length(w) > 0){
        num_max_msg <- records[w, c(ids, field), drop=FALSE]
        num_max_msg$msg <- paste0("Entry for '", field, "' is larger than the acceptable maximum.  Please confirm.")
        printLog(num_max_msg, logfile)
      }
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
        bad_yn_msg$msg <- paste0("Entry for '", field, "' must be either no, yes, 0, or 1.  No value was imported.")
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
        bad_yn_msg$msg <- paste0("Entry for '", field, "' must be either no, yes, 0, or 1.")
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
        bad_yn_msg$msg <- paste0("Entry for '", field, "' must be either no, yes, 0, or 1.  No value was imported.")
        printLog(bad_yn_msg, logfile)
        x <- as.character(factor(x, c("no", "yes"), 0:1))
      }
    }
    else if (is.numeric(x)){
      w <- which(!x %in% 0:1 & !is.na(x))
      if (length(w) > 0){
        bad_yn_msg <- records[w, c(ids, field), drop=FALSE]
        bad_yn_msg$msg <- paste0("Entry for '", field, "' must be either no, yes, 0, or 1.")
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
    mapping <- stringr::str_split_fixed(unlist(strsplit(meta_data$select_choices_or_calculations, " [|] ")), ", ", 2)
    
    #* Return labeled values to coded values
    for (i in 1:nrow(mapping)){
      x[x==mapping[i, 2]] <- mapping[i, 1]  
    }
    
    
    w <- which(!x %in% mapping[, 1] & !is.na(x))
    if (length(w) > 0){
      radio_msg <- records[w, c(ids, field), drop=FALSE]
      radio_msg$msg <- paste0("Entry for '", field, "' must be either one of: ", paste(c(mapping[, 1], mapping[, 2]), collapse=", "), ".")
      printLog(radio_msg, logfile)
      x[w] <- NA
    }
    
    return(x)
  }
  
  
  #********************************************************
  #* checkbox fields
  else if (grepl("checkbox", meta_data$field_type)){
    x <- as.character(x)
    
    #* Select the labeled string from the options as a valid input for the import.
    checkChoice <- stringr::str_split_fixed(unlist(strsplit(meta_data$select_choices_or_calculations, " [|] ")), ", ", 2)
    checkChoice <- checkChoice[checkChoice[, 1] == unlist(strsplit(field, "___"))[2], 2]
    
    w <- which(!x %in% c("Checked", "Unchecked", "0", "1", checkChoice) & !is.na(x))
    if (length(w) > 0){
      check_msg <- records[w, c(ids, field), drop=FALSE]
      check_msg$msg <- paste0("Entry for '", field, "' must be either one of: 0, 1, Checked, Unchecked, ", 
                             checkChoice, ".")
      printLog(check_msg, logfile)
    }
    x[x %in% "Checked"] <- "1"
    x[x %in% "Unchecked"] <- "0"
    x[x %in% checkChoice] <- "1"
    x[!x %in% c("0", "1")] <- NA
    return(x)
  }
  
  #*********************************************************
  #* phone number fields
  else if (grepl("phone", meta_data$text_validation_type_or_show_slider_number)){
    x <- as.character(x)
    x <- gsub("[[:punct:][:space:]]", "", x)
    
    w <- which(nchar(x) != 10 & !is.na(x))
    if (length(w) > 0){
      phone_msg <- records[w, c(ids, field), drop=FALSE]
      phone_msg$msg <- paste0("Entry for '", field, "' must be a 10 digit phone number.", 
                             "This value was not uploaded.")
      printLog(phone_msg, logfile)
      x[w] <- NA
    }

    x.area <- substr(x,1,3)
    x.exchange <- substr(x, 4, 6)
    x.station <- substr(x, 7, 10)
    
    w <- which(!is.na(x) & !(grepl("[2-9][0-8][0-9]", x.area) & grepl("[2-9]\\d{2}", x.exchange) & 
                           grepl("\\d{4}", x.station)))
    if (length(w) > 0){
      phone_msg <- records[w, c(ids, field), drop=FALSE]
      phone_msg$msg <- paste0("Entry for '", field, "' is not a valid 10 digit phone number ",
                         "and is not imported.")
      printLog(phone_msg, logfile)
      x[w] <- NA
    }
    
    return(x)
  }
  
  #*********************************************************
  #* phone number fields
  else if (grepl("email", meta_data$text_validation_type_or_show_slider_number)){
    x <- as.character(x)
    w <- which(!grepl("[A-Za-z0-9._%+-]+@[A-Za-z0-9.-]+[.][A-Za-z]{2,6}$", x) & !is.na(x))
    if (length(w) > 0){
      email_msg <- records[w, c(ids, field), drop=FALSE]
      email_msg$msg <- paste0("Entry for '", field,"' is not a valid e-mail address ",
                         "and is not imported.")
      printLog(email_msg, logfile)
    }
    x[w] <- NA
    return(x)
  }
  
  return(x)
  
}
