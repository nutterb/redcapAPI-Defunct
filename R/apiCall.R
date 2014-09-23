apiCall <- function(url, body, config){
  x <- tryCatch(httr::POST(url, body, config),
                error = function(cond){
                  if (grepl("GnuTLS recv error [(]-9[)]", cond)){
                    m <- RCurl::postForm(uri=url, .params=body,
                                         .opts=config)
                    m <- list(content=charToRaw(m),
                              status_code = 200,
                              url=rcon$url)
                    class(m) <- "response"
                    return(m)
                  }
                })
  return(x)
}
