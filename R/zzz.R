.onLoad <- function(libname,pkgname)
{
  options(redcap.dataTable='redcap_data')
  options(redcap.metaDataTable='redcap_metadata')
  options(redcap_api_url = NA)
  options(redcap_project_info = list(meta_data = NULL,
                                     users = NULL,
                                     events = NULL,
                                     arms = NULL,
                                     mappings = NULL))
}

.onUnload <- function(libPath)
{
  options(redcap.dataTable=NULL)
  options(redcap.metaDataTable=NULL)
  options(redcap_api_url=NULL)
}
