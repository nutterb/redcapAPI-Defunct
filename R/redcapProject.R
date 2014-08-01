redcapProject <- function(rcon, ...) UseMethod("redcapProject")

redcapProject.redcapDbConnection <- function(rcon, ...){
  message("Please accept my apologies.  The redcapProject method for redcapDbConnection objects\n",
          "has not yet been written.  Please consider using the API.")
}

redcapProject.redcapApiConnection <- function(rcon, date, label){
  options(redcapProject = list(meta_data = exportMetaData(rcon),
                               users = exportUsers(rcon, date, label),
                               events = exportEvents(rcon),
                               arms = exportArms(rcon),
                               mappings = exportMappings(rcon)))
}
