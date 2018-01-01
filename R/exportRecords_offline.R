#' @rdname exportRecords
#' @export

exportRecords_offline <- function(datafile, meta_data, 
                                  factors = TRUE, fields = NULL,
                                  forms=NULL, labels = TRUE,
                                  dates = TRUE, checkboxLabels = FALSE, 
                                  colClasses = NA,
                                  ...)
{
  #* Error Collection Object
  coll <- checkmate::makeAssertCollection()
  
  massert(~ factors + labels + dates + checkboxLabels,
          fun = checkmate::assert_logical,
          fixed = list(len = 1,
                       add = coll))
  
  massert(~ fields + forms + datafile + meta_data,
          fun = checkmate::assert_character,
          len = list(datafile = 1, 
                     meta_data = 1),
          fixed = list(null.ok = TRUE,
                       add = coll))
  
  checkmate::reportAssertions(coll)
  
  #* Secure the meta data.
  meta_data <- utils::read.csv(meta_data,
                               stringsAsFactors = FALSE,
                               na.strings = "")
  
  col.names=c('field_name', 'form_name', 'section_header', 
              'field_type', 'field_label', 'select_choices_or_calculations', 
              'field_note', 'text_validation_type_or_show_slider_number', 
              'text_validation_min', 'text_validation_max', 'identifier', 
              'branching_logic', 'required_field', 'custom_alignment', 
              'question_number', 'matrix_group_name', 'matrix_ranking',
              'field_annotation')
  
  names(meta_data) <- col.names[1:length(col.names)]
  
  #* for purposes of the export, we don't need the descriptive fields. 
  #* Including them makes the process more error prone, so we'll ignore them.
  meta_data <- meta_data[!meta_data$field_type %in% "descriptive", ]  
  
  #* Check that all fields exist in the meta data
  if (!is.null(fields)) 
  {
    bad_fields <- fields[!fields %in% meta_data$field_name]
    if (length(bad_fields))
      coll$push(paste0("The following are not valid field names: ",
                       paste0(bad_fields, collapse = ", ")))
  }
  
  #* Check that all form names exist in the meta data
  if (!is.null(forms))
  {
    bad_forms <- forms[!forms %in% meta_data$form_name]
    if (length(bad_forms))
      coll$push(paste0("The following are not valid form names: ",
                       paste0(bad_forms, collapse = ", ")))
  }
  
  checkmate::reportAssertions(coll)
  
  #* Create the vector of field names
  if (!is.null(fields)) #* fields were provided
  {
    # redcap_event_name is automatically included in longitudinal projects
    field_names <- fields[!fields %in% "redcap_event_name"] 
  }
  else if (!is.null(forms))
  {
    field_names <- meta_data$field_name[meta_data$form_name %in% forms]
  }
  else
    #* fields were not provided, default to all fields.
    field_names <- meta_data$field_name
  
  #* Expand 'field_names' to include fields from specified forms.    
  if (!is.null(forms)) 
  {
    field_names <- 
      unique(c(field_names, 
               meta_data$field_name[meta_data$form_name %in% forms]))
  }  
  
  suffixed <- checkbox_suffixes(fields = field_names,
                                meta_data = meta_data, 
                                version = version)
  
  
  x <- utils::read.csv(datafile, 
                       stringsAsFactors = FALSE,
                       colClasses = colClasses)[suffixed$name_suffix]
  
  x <- fieldToVar(records = x, 
                  meta_data = meta_data, 
                  factors = factors, 
                  dates = dates, 
                  checkboxLabels = checkboxLabels)
  
  if (labels){
    x[suffixed$name_suffix] <-
      mapply(nm = suffixed$name_suffix,
             lab = suffixed$label_suffix,
             FUN = function(nm, lab){
               labelVector::set_label(x[[nm]], lab)
             },
             SIMPLIFY = FALSE)
  }
  x  
}

