importRecords <- function(rcon, data, 
                          overwriteBehavior=c('normal', 'overwrite'),
                          returnContent=c('count', 'ids', 'nothing'),
                          returnFormat=c('xml', 'csv', 'json')){
                          
  overwriteBehavior <- match.arg(overwriteBehavior, c('normal', 'overwrite'))
  returnContent <- match.arg(returnContent, c('count', 'ids', 'nothing'))
  returnFormat <- match.arg(returnFormat, c('xml', 'csv', 'json'))
  
  #** Format the data for REDCap import
  #** Thanks go to:
  #**   https://github.com/etb/my-R-code/blob/master/R-pull-and-push-from-and-to-REDCap.R
  #**   http://stackoverflow.com/questions/12393004/parsing-back-to-messy-api-strcuture/12435389#12435389
  data <- lapply(data, function(x){
    if(any(is.na(x))) {x[is.na(x)] <- ""; x} else {x}
  })

  l1 <- paste(names(data), collapse=",")
  l2 <- capture.output(write.table(data, sep=",", col.names=FALSE, row.names=FALSE))
  out <- paste0(c(l1, l2, ""), collapse="\n")

  ## Reattach attributes
  att <- list("Content-Type" = structure(c("text/html", "utf-8"),
              .Names = c("", "charset")))
  attributes(out) <- att

  cat(postForm(uri=rcon$url,
               token = rcon$token, content='record', format='csv',
               type='flat', overwriteBehavior = overwriteBehavior, 
               data=out))
}
