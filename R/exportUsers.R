exportUsers <- function(rcon, ...) UseMethod("exportUsers")

exportUsers.redcapDbConnection <- function(rcon, date=TRUE, label=TRUE, ...){
  message("Please accept my apologies.  The exportUsers method for redcapDbConnection objects\n",
          "has not yet been written.  Please consider using the API.")
}

exportUsers.redcapApiConnection <- function(rcon, date=TRUE, label=TRUE, ...){
  #* parameters for the Users File Export
  .params <- list(token=rcon$token, content='user', format='csv', returnFormat='csv')
  
  #* Export Users file and convert to data frame
  x <- httr::POST(url=rcon$url, body=.params, config=rcon$config)
  if (x$status_code != "200") stop(paste0(x$status_code, ": ", as.character(x)))
  x <- read.csv(textConnection(as.character(x)), stringsAsFactors=FALSE, na.strings="")
  
  #* convert expiration date to POSIXct class
  if (date) x$expiration <- as.POSIXct(x$expiration, format="%Y-%m-%d")
  
  #* convert data export and form editing privileges to factors
  if (label){
    x$data_export <- factor(x$data_export, c(0, 2, 1), c("No access", "De-identified", "Full data set"))
    
    x[, which(names(x) == "data_export"):length(x)] <- 
        lapply(x[, which(names(x) == "data_export"):length(x)], 
            factor, c(0, 2, 1, 3),
            c("No access", "Read only", "view records/responses and edit records", "Edit survey responses"))
  }
  
  return(x)
}
