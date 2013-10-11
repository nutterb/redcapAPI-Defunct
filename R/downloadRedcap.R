downloadRedcap <- function(APIKEY, url=options()$redcap_api_url, 
                          fields=NULL, forms=NULL, records=NULL, events=NULL, 
                          raw=FALSE, label=TRUE) {
  require(Hmisc)
  require(stringr)
  
  rcon <- redcapConnection(url, APIKEY)
  gg <- exportMetaData(rcon)
  
  # remove quotes
  rmq <- function(x) str_replace_all(x, "['\"]", '')
  clean <- function(x) { str_replace_all(rmq(x), '[\n]', ' ') }
  
  # remove quote characters, replace line breaks
  gg$field_label <- str_replace_all(clean(gg$field_label), '[\x80-\x9f]', '')
  
  if (!is.null(fields)) gg <- subset(gg, field_name %in% fields)
  
  # make a list of calls to 'label'
  labelList <- function(r){
    switch(gg$field_type[r],
           checkbox = {choices <- unlist(str_split(gg$select_choices_or_calculations[r], "[|]"))
                       choices <- str_split_fixed(choices, ", ", 2)
                       nums <- str_trim(choices[,1])
                       choices <- str_trim(choices[,2])
                       return(sprintf('label(data$%s___%s) <- "%s (choice=%s)"', 
                                      gg$field_name[r], nums, sub('[\n]', ' ', gg$field_label[r]), rmq(choices)))
           },
           sprintf('label(data$%s) <- "%s"', gg$field_name[r], sub('[\n]', ' ', gg$field_label[r])))
  }
  
  # Make a list of calls to create factors
  #levelList <- function(r){
  #  switch(gg$field_type[r],
  #         checkbox = {choices <- unlist(str_split(gg$select_choices_or_calculations[r], "[|]"))
  #                     choices <- str_split_fixed(choices, ", ", 2)
  #                     nums <- str_trim(choices[, 1])
  #                     choices <- str_c("c('", str_c(str_trim(choices[,2]), collapse="', '"), "')", sep="")
  #                     return(sprintf('data$%1$s___%2$s <- factor(data$%1$s___%2$s, c("0", "1"), c("Unchecked", "Checked"))', 
  #                                    gg$field_name[r], nums))
  #         },
  #         radio = return(sprintf('attr(data$%s, "redcapLevels") <- NULL', gg$field_name[r])),
  #         dropdown = return(sprintf('attr(data$%s, "redcapLevels") <- NULL', gg$field_name[r])),
  #         yesno = return( sprintf('data$%1$s <- factor(data$%1$s, levels=c("0", "1"), labels=c("No", "Yes"))', gg$field_name[r])),
  #         truefalse = return(sprintf('data$%1$s <- factor(data$%1$s, levels=c("0", "1"), labels=c("FALSE", "TRUE"))', gg$field_name[r])),
  #         notes = return(""),
  #         text = return(""),
  #         slider = return(""),
  #         sprintf("print(\"I don\'t know field_type \'%s\' for field \'%s\'\")", gg$field_type[r], gg$field_name[r]))
  #}
  
  
  # <instrument>_complete
  complete <- sprintf('%s_complete', unique(gg$form_name))
  
  if (label){
    labels <- lapply(1:nrow(gg), labelList)    
    if (any(complete %in% gg$field_names)) labels[[nrow(gg)+1]] <- sprintf('label(data$%s) <- "Complete?"', 
                                                                           complete[complete %in% gg$field_names])
  }
  
  #if (!raw){
  #  levels <- lapply(1:nrow(gg), levelList)
  #  if (any(complete %in% gg$field_names)) levels[[nrow(gg)+1]] <- 
  #    sprintf('data$%1$s <- factor(data$%1$s, levels=c("2", "1", "0"), labels=c("Complete", "Unverified", "Incomplete"))', 
  #            complete[complete %in% gg$field_names])
  #}
  
  
  #info <- c(if (!raw) unlist(levels) else "", if (label) unlist(labels) else "")
  info <- unlist(labels)
  # download data
  data <- exportRecords(rcon, factors=!raw, fields=fields, forms=forms, records=records, events=events)
  # apply labels and levels
  eval(parse(text=info))
  return(data)
}
