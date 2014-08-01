redcapProjectInfo <- function(rcon, date=TRUE, label=TRUE, ...) UseMethod("redcapProjectInfo")

redcapProjectInfo.redcapDbConnection <- function(rcon, date=TRUE, label=TRUE, ...){
  message("Please accept my apologies.  The redcapProjectInfo method for redcapDbConnection objects\n",
          "has not yet been written.  Please consider using the API.")
}

redcapProject.redcapApiConnection <- function(rcon, date=TRUE, label=TRUE){
  options(redcapProjectInfo = list(meta_data = exportMetaData(rcon),
                               users = exportUsers(rcon, date, label),
                               events = exportEvents(rcon),
                               arms = exportArms(rcon),
                               mappings = exportMappings(rcon)))
}
