.onLoad <- function(libname,pkgname)
{
  options(redcap_api_url = NA,
          redcap_error_handling = "null")
}

.onUnload <- function(libPath)
{
  options(redcap_api_url=NULL,
          redcap_error_handling = NULL)
}
