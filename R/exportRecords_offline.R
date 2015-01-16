#' @rdname exportRecords

exportRecords_offline <- 
  function(datafile, meta_data, factors=TRUE,fields=NULL,forms=NULL,
           labels=TRUE,dates=TRUE, checkboxLabels=FALSE, ...)
  {

    #* for purposes of the export, we don't need the descriptive fields. 
    #* Including them makes the process more error prone, so we'll ignore them.
    meta_data <- read.csv(meta_data,
                          col.names=c('field_name', 'form_name', 'section_header', 
                                      'field_type', 'field_label', 'select_choices_or_calculations', 
                                      'field_note', 'text_validation_type_or_show_slider_number', 
                                      'text_validation_min', 'text_validation_max', 'identifier', 
                                      'branching_logic', 'required_field', 'custom_alignment', 
                                      'question_number', 'matrix_group_name', 'matrix_ranking'),
                          stringsAsFactors=FALSE)
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

    x <- read.csv(datafile, stringsAsFactors=FALSE, na.strings="")#[, field_names, drop=FALSE]
       
    lapply(field_names,
           function(i) 
           {
             x[[i]] <<- fieldToVar(as.list(meta_data[meta_data$field_name==sub("___[a-z,A-Z,0-9,_]+", "", i),]), 
                                   x[[i]],factors,dates, checkboxLabels, vname=i)
           }
    )
   
    if (labels) Hmisc::label(x[, field_names], self=FALSE) <- field_labels
   
   if ("redcap_data_access_group" %in% names(x)) field_names <- c(field_names[1], "redcap_data_access_group", field_names[-1])
   if ("redcap_event_name" %in% names(x)) field_names <- c(field_names[1], "redcap_event_name", field_names[-1])
   field_names <- c(field_names, paste0(unique(meta_data$form_name), "_complete"))
   x <- x[, field_names, drop=FALSE]
   
   x
  }

