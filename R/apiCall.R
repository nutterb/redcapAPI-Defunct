apiCall <- function(url, body, config){
  x <- tryCatch(httr::POST(url=url, body=body, config=config),
                error = function(cond){
                  if (grepl("GnuTLS recv error [(]-9[)]", cond)){
                    m <- httr::POST(url=url, body=body,
                                         config=c(list(encoding='identity'), config))
                    return(m)
                  }
                })
  return(x)
}
