#' @name fieldToVar
#' @importFrom chron times
#' 
#' @title Convert a REDCap Data Field to an R Vector
#' @description Converts a field exported from REDCap into a valid R vector
#' 
#' @details This function is called internally by \code{exportRecords} and 
#'   \code{exportReports}.  it is not available to the user.
#'   
#' @author Jeffrey Horner

fieldToVar <- 
  # function(m,d,factors=TRUE, dates=TRUE, checkboxLabels=FALSE, vname)
  function(records, meta_data, 
           factors = TRUE, dates = TRUE, checkboxLabels = FALSE)
{ 
  for (i in seq_along(records))
  {
    field_base <- gsub(pattern = "___.+$",
                       replacement = "",
                       x = names(records)[i])
    
    
    
    field_type <- meta_data$text_validation_type_or_show_slider_number[meta_data$field_name == field_base]
    #* If the variable isn't in the data dictionary (usually it's a field added by REDCap,
    #* such as redcap_event_name, instrument_complete, etc), give it a generic name to
    #* pass to switch.
    if (!length(field_type)) field_type <- "unrecognized field type"
    
    field_type[is.na(field_type)] <- meta_data$field_type[meta_data$field_name == field_base]
    field_type <- gsub(pattern = "_(dmy|mdy|ymd)$", 
                       replacement = "_",
                       x = field_type)

    
    records[[i]] <- 
      switch(field_type,
             "date_" = 
               {
                 if (dates) 
                   as.POSIXct(records[[i]], format = "%Y-%m-%d") 
                 else 
                   records[[i]]
                },
             "datetime_" = 
               {
                 if (dates) 
                   as.POSIXct(records[[i]], format = "%Y-%m-%d %H:%M") 
                 else 
                   records[[i]]
               },
             "datetime_seconds_" = 
               {
                 if (dates) 
                   as.POSIXct(records[[i]], format = "%Y-%m-%d %H:%M:%S") 
                 else 
                   records[[i]]
               },
             "time_mm_ss" = 
               {
                 if (dates) 
                   chron::times(ifelse(!is.na(records[[i]]), 
                                       paste0("00:", records[[i]]), 
                                       records[[i]]), 
                                format=c(times="h:m:s"))
                 else 
                   records[[i]]
               },
             "time" = 
               {
                 if (dates)
                   chron::times(ifelse(!is.na(records[[i]]), 
                                       paste0(records[[i]], ":00"), 
                                       records[[i]]), 
                                format=c(times="h:m:s"))
                 else 
                   records[[i]]
               },
             "float" = suppressWarnings(as.numeric(records[[i]])),
             "number" = suppressWarnings(as.numeric(records[[i]])),
             "calc" = suppressWarnings(as.numeric(records[[i]])),
             "int" = suppressWarnings(as.integer(records[[i]])),
             "integer" = suppressWarnings(as.numeric(records[[i]])),
             "select" = 
               makeRedcapFactor(x = records[[i]],
                                coding = meta_data$select_choices_or_calculations[meta_data$field_name == field_base],
                                factors = factors),
             "radio" = 
               makeRedcapFactor(x = records[[i]],
                                coding = meta_data$select_choices_or_calculations[meta_data$field_name == field_base],
                                factors = factors),
             "dropdown" = 
               makeRedcapFactor(x = records[[i]],
                                coding = meta_data$select_choices_or_calculations[meta_data$field_name == field_base],
                                factors = factors),
             "yesno" = makeRedcapYN(records[[i]], 
                                    factors),
             "truefalse" = 
              {
                if (factors) 
                  as.logical(records[[i]])
                else
                  records[[i]]
              },
             "checkbox" = 
              {
                makeRedcapCheckbox(x = records[[i]],
                                   suffix = gsub("^.+___", "", names(records)[i]),
                                   coding = meta_data$select_choices_or_calculations[meta_data$field_name == field_base],
                                   factors = factors,
                                   checkboxLabels = checkboxLabels)
              },
             records[[i]]
      ) # End switch
  } # End for loop
  records
}    



  

    