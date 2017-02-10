#' @rdname exportRecords

exportRecords_offline <- 
  function(datafile, meta_data, factors=TRUE,fields=NULL,forms=NULL,
           labels=TRUE,dates=TRUE, checkboxLabels=FALSE, ...)
  {
    
    #* for purposes of the export, we don't need the descriptive fields. 
    #* Including them makes the process more error prone, so we'll ignore them.
    meta_data <- utils::read.csv(meta_data, header=FALSE, skip=1,
                                 stringsAsFactors=FALSE)
    
    col.names=c('field_name', 'form_name', 'section_header', 
                'field_type', 'field_label', 'select_choices_or_calculations', 
                'field_note', 'text_validation_type_or_show_slider_number', 
                'text_validation_min', 'text_validation_max', 'identifier', 
                'branching_logic', 'required_field', 'custom_alignment', 
                'question_number', 'matrix_group_name', 'matrix_ranking',
                'field_annotation')
    names(meta_data) <- col.names[1:length(col.names)]
    meta_data <- subset(meta_data, !meta_data$field_type %in% "descriptive")
    
    #* Check that stated forms exist
    if (any(!forms %in% unique(meta_data$form_name))){
      stop(paste0("'", paste(forms[!forms %in% unique(meta_data$form_name)], collapse="', '"),
                  " are not valid form names"))
    }
    
    #* Create list of field names
    if (!is.null(fields)) #* fields were provided
    {
      # redcap_event_name is automatically included in longitudinal projects
      fields <- fields[!fields %in% "redcap_event_name"] 
      
      # verify all field names exist (could also be 'all(meta_data$field_name %in% fields)'
      if (is.character(fields) && 
          length(which(meta_data$field_name %in% fields)) == length(fields)){
        field_names <- unique(c(fields))
        .params[['fields']] = paste(fields,collapse=',')
      } 
      else #* found non-existent fields
        stop(paste("Non-existent fields:", paste(fields[!fields %in% meta_data$field_name], collapse=", "), sep=" "))
    } 
    else{ #* fields were not provided, default to all fields.
      field_names <- meta_data$field_name
    }
    
    #* Expand 'field_names' to include fields from specified forms.    
    if (!is.null(forms)) 
      field_names <- unique(c(field_names, meta_data$field_name[meta_data$form_name %in% forms]))
    
    # Save a copy of the original field_names before they are futher modified.
    field_names_orig <- field_names

    #* Extract label suffixes for checkbox fields
    #* This takes the choices of the checkboxes from the meta data and organizes
    #* To be conveniently pasted to 'field_label'
    #* In this process, a checkbox field label is replicated as many times as it has options
    checklabs <- function(x){
      if (meta_data$field_type[meta_data$field_name %in% x] == "checkbox"){
        opts <- unlist(strsplit(meta_data$select_choices_or_calculations[meta_data$field_name %in% x], "[|]"))
        opts <- sub("[[:space:]]+$", "", unlist(sapply(strsplit(opts, ","), '[', 2)))
        opts <- sub("[[:space:]]+", ": ", opts)
        return(opts)
      }
      return("")
    }
    field_labels_suffix <- unlist(sapply(field_names, checklabs))
    
    #* convert field_names to a list for convenience of using sapply to get field_labels
    field_names <- lapply(field_names, identity) 
    field_labels <- sapply(field_names, function(x) meta_data$field_label[meta_data$field_name %in% x])
    
    #* This function grabs the variable codings of the checkbox variables.
    #* These need to be appended to the field names
    #* In this process, a checkbox field name is replicated as many times as it has options
    checkvars <- function(x){
      if (meta_data$field_type[meta_data$field_name %in% x] == "checkbox"){
        opts <- unlist(strsplit(meta_data$select_choices_or_calculations[meta_data$field_name %in% x], "[|]"))
        opts <- tryCatch(as.numeric(unlist(sapply(strsplit(opts, ","), '[', 1))),
                         warning = function(cond){ nm <- as.character(unlist(sapply(strsplit(opts, ","), '[', 1)))
                         nm <- gsub('^\\s*','',nm,perl=TRUE)
                         nm <- gsub('\\s*$','',nm,perl=TRUE)
                         return(nm)})
        x <- paste(x, opts, sep="___")
      }
      return(x)
    }
    field_names <- sapply(field_names, checkvars)
    
    #* Ensures field_labels is adjusted to the proper length to account for
    #* checkbox variables and creates the labels.
    field_labels <- rep(field_labels, sapply(field_names, length))
    field_labels <- paste(field_labels, field_labels_suffix, sep="")
    
    #* return field_names to a vector
    field_names <- unlist(field_names)
    
    x <- utils::read.csv(datafile, stringsAsFactors=FALSE, na.strings="", header=TRUE)#[, field_names, drop=FALSE]
    
    # Find the original variable names in the record form dataset.
    x_field_names <- as.vector(t(utils::read.csv(datafile, stringsAsFactors=FALSE, na.strings="", header=FALSE, colClasses='character', nrows=1)))

    # Find the extra variable names in x that are not in the metadata field_name column.
    x_field_names_extra <- x_field_names[! x_field_names %in% field_names_orig]

    # Create a dataframe of these extra variable names, storing their original index.
    x_field_names_df <- data.frame(index=1:ncol(x), field_name=x_field_names, stringsAsFactors = FALSE)
    x_field_names_extra_df <- x_field_names_df[x_field_names_df$field_name %in% x_field_names_extra, ]
    
    # Replace the index with the index of previous item for use with append()'s 'after' argument.
    x_field_names_extra_df$index <- x_field_names_extra_df$index - 1

    lapply(field_names,
           function(i) 
           {
             x[[i]] <<- fieldToVar(as.list(meta_data[meta_data$field_name==sub("___[a-z,A-Z,0-9,_]+", "", i),]), 
                                   x[[i]],factors,dates, checkboxLabels, vname=i)
           }
    )
    
    # Insert the extra field names into the field_names vector in the correct position.
    # Similarly, insert NA into field_labels for those fields not in this vector.
    if (nrow(x_field_names_extra_df) != 0) {
        for (i in 1:nrow(x_field_names_extra_df)) {
            field_names <- append(field_names, x_field_names_extra_df[i, 'field_name'], after=x_field_names_extra_df[i, 'index'])
            field_labels <- append(field_labels, NA, after=x_field_names_extra_df[i, 'index'])
        }
    }

    # Rename the fields of x using the merged field_names vector.
    names(x) <- field_names
    
    if (labels) Hmisc::label(x[, field_names], self=FALSE) <- field_labels
    
    # convert survey timestamps to dates
    if (dates)
    {
      survey_date <- field_names[grepl("_timestamp$", field_names)]
      x[survey_date] <- 
        lapply(x[survey_date],
               function(s) 
               {
                 s[s == "[not completed]"] <- NA
                 s[s == "0000-00-00 00:00:00"] <- NA
                 as.POSIXct(s)
               })
    }

    x
  }

