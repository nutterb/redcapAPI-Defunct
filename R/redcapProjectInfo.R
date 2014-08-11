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
  
  version <- if (version) exportVersion(rcon) else v.number
  meta_data <- if (meta_data) exportMetaData(rcon) else NULL
  users <- if (users) exportUsers(rcon, date, label) else NULL
  instruments <- if (instruments) exportInstruments(rcon) else NULL
  events <- if (events) exportEvents(rcon) else NULL
  arms <- if (arms) exportArms(rcon) else NULL
  mappings <- if (mappings) exportMappings(rcon) else NULL
  
  
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
