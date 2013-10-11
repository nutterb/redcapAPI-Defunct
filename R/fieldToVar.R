fieldToVar <- 
  function(m,d,factors=TRUE)
  {
    
    # Continuous variables
    if ((!is.na(m$text_validation_type_or_show_slider_number) && 
           m$text_validation_type_or_show_slider_number %in% c('float','int') ) || 
          (m$field_type %in% c('calc')) )
    {
      
      suppressWarnings(d <- as.numeric(d))
      
      # Ordinal or Categorical variable with a label
    } 
    else if (m$field_type %in% c('select','radio','dropdown'))
    {
      # parses the string "0, Birth \\n 1, Death \\n 2, Unknown" into a
      # character vector for creating a factor
      w <- unlist(strsplit(m$select_choices_or_calculations,"[\n|]"))
      if (length(w)>0) 
      {
        if (length(w) == length(grep(',',w)))
        {
          w <- unlist(lapply(strsplit(w,',',perl=TRUE),
                             function(x)
                               c(x[1],paste(x[2:length(x)],collapse=','))
          ))
          w <- gsub('^\\s*','',w,perl=TRUE)
          w <- gsub('\\s*$','',w,perl=TRUE)
          # Create factor
          # Ignore warnings of NAs produced by coercion as this
          # is typically user entry error
          if (factors==TRUE)
          {
            suppressWarnings(
              d <- factor(as.integer(d),
                          levels=as.integer(w[seq(1,length(w),2)]),
                          labels=w[seq(2,length(w),2)])
            )
            attr(d,'redcapLevels') <- as.integer(w[seq(1,length(w),2)])
          }
          else
          {
            suppressWarnings(d <- as.integer(d))
            attr(d,'redcapLabels') <- w[seq(2,length(w),2)]
            attr(d,'redcapLevels') <- as.integer(w[seq(1,length(w),2)])
          }
        } 
        else if (length(w) == length(grep('^[0-9.]+$',w,perl=TRUE))) 
        {
          # Create integer since the meta data about choices are bungled.
          suppressWarnings(d <- as.integer(d))
        } 
      }
    }
    else if (m$field_type %in% c('yesno'))
    {
      d <- factor(d, 0:1, c('No', 'Yes'))
      attr(d, 'redcapLabels') <- c('No', 'Yes')
      attr(d, 'redcapLevels') <- 0:1
    }
    else if (m$field_type %in% c('truefalse'))
    {
      d <- as.logical(d)
    }
    else if (m$field_type %in% c('checkbox'))
    {
      d <- factor(d, 0:1, c('Unchecked', 'Checked'))
      attr(d, 'redcapLabels') <- c('Unchecked', 'Checked')
      attr(d, 'redcapLevels') <- 0:1
    }
    
    d
  }
