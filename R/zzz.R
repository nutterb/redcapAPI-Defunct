packageStartupMessage(
  "Welcome to redcapAPI 2.0.  Please Note:\n",
  " - redcapDbConnection methods have all been deprecated.\n",
  " - 'exportProjectInfo' has been deprecated and replaced with 'exportBundle'.\n",
  " - 'exportBundle' saves its result to an option. Consider discontinuing\n",
  "   use of bundle objects unless working with multiple REDCap projects in one session.")

.onLoad <- function(libname,pkgname)
{
  options(redcap_api_url = NA,
          redcap_error_handling = "null",
          redcap_bundle = 
            structure(
              list(
                version = NULL,
                meta_data = NULL,
                users = NULL,
                instruments = NULL,
                events = NULL,
                arms = NULL,
                mappings = NULL
              ),
              class = c("redcapBundle", "redcapProject", "list")
            )
  )
}

.onUnload <- function(libPath)
{
  options(redcap_api_url=NULL,
          redcap_error_handling = NULL,
          redcap_bundle = NULL)
}
