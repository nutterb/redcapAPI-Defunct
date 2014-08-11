.onLoad <- function(libname,pkgname)
{
  options(redcap.dataTable='redcap_data')
  options(redcap.metaDataTable='redcap_metadata')
  options(redcap_api_url = NA)
}

.onUnload <- function(libPath)
{
  options(redcap.dataTable=NULL)
  options(redcap.metaDataTable=NULL)
  options(redcap_api_url=NULL)
}
