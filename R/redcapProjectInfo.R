redcapProjectInfo <- function(rcon, date=TRUE, label=TRUE, 
                              meta_data=TRUE, users=TRUE, instruments=TRUE,
                              events=TRUE, arms=TRUE, mappings=TRUE,
                              version=TRUE, ...) UseMethod("redcapProjectInfo")

redcapProjectInfo.redcapDbConnection <- function(rcon, date=TRUE, label=TRUE, 
                              meta_data=TRUE, users=TRUE, instruments=TRUE,
                              events=TRUE, arms=TRUE, mappings=TRUE,
                              version=TRUE, ...){
  message("Please accept my apologies.  The redcapProjectInfo method for redcapDbConnection objects\n",
          "has not yet been written.  Please consider using the API.")
}

redcapProjectInfo.redcapApiConnection <- function(rcon, date=TRUE, label=TRUE, 
                              meta_data=TRUE, users=TRUE, instruments=TRUE,
                              events=TRUE, arms=TRUE, mappings=TRUE,
                              version=TRUE, ...,
                              v.number = ""){
  
  version <- if (version) tryCatch(exportVersion(rcon),
                                   error = function(cond){
                                     warning(paste("Error in exportVersion: ", cond))
                                     return(cond)
                                   }) else v.number
  meta_data <- if (meta_data) tryCatch(exportMetaData(rcon),
                                   error = function(cond){
                                     warning(paste("Error in exportMetaData: ", cond))
                                     return(cond)
                                   }) else NULL
  users <- if (users) tryCatch(exportUsers(rcon, date, label),
                                   error = function(cond){
                                     warning(paste("Error in exportUsers: ", cond))
                                     return(cond)
                                   }) else NULL
  instruments <- if (instruments) tryCatch(exportInstruments(rcon),
                                   error = function(cond){
                                     warning(paste("Error in exportInstruments: ", cond))
                                     return(cond)
                                   }) else NULL
  events <- if (events) tryCatch(exportEvents(rcon),
                                   error = function(cond){
                                     warning(paste("Error in exportEvents: ", cond))
                                     return(cond)
                                   }) else NULL
  arms <- if (arms) tryCatch(exportArms(rcon),
                                   error = function(cond){
                                     warning(paste("Error in exportArms: ", cond))
                                     return(cond)
                                   }) else NULL
  mappings <- if (mappings) tryCatch(exportMappings(rcon),
                                   error = function(cond){
                                     warning(paste("Error in exportMappings: ", cond))
                                     return(cond)
                                   }) else NULL
  
  
  proj <- list(meta_data = meta_data,
               users=users,
               instruments=instruments,
               events=events,
               arms=arms,
               mappings=mappings,
               version=version)
  class(proj) <- c("redcapProject", "list")
  return(proj)
}
