#' @name redcapConnection
#' @export redcapConnection
#' 
#' @title Connect to a REDCap Database
#' @description Creates an object of class \code{redcapApiConnection} for 
#' using the REDCap API [or a direct connection through an SQL server]
#' 
#' @param url URL for a REDCap database API.  Check your institution's REDCap 
#'   documentation for this address.  Either \code{url} or \code{conn} must 
#'   be specified.
#' @param token REDCap API token
#' @param conn The database connection to be used. If used, \code{project}
#'   must also be used.
#' @param project The project ID in the REDCap tables.
#' @param config A list to be passed to \code{httr::POST}.  This allows the 
#'   user to set additional configurations for the API calls, such as 
#'   certificates, ssl version, etc. For the majority of users, this does 
#'   not need to be altered.  See Details for more about this argument's 
#'   purpose and the \code{redcapAPI} wiki for specifics on its use.
#'   
#' @details
#' For convenience, you may consider using 
#' \code{options(redcap_api_url=[your URL here])} in your RProfile.
#' To obtain an API token for a project, do the following:\cr
#' Enter the 'User Right' section of a project\cr
#' Select a user\cr
#' Check the box for 'API Data Export' or 'API Data Import,' as appropriate.  A full tutorial on 
#' configuring REDCap to use the API can be found at \url{https://github.com/nutterb/redcapAPI/wiki}
#' 
#' Tokens are specific to a project, and a token must be created for each 
#' project for which you wish to use the API.
#' 
#' The \code{config} argument is passed to the \code{httr::POST} argument of 
#' the same name.  The most likely reason for using this argument is that the 
#' certificate files bundled in \code{httr} have fallen out of date.  
#' Hadley Wickham is pretty good about keeping those certificates up 
#' to date, so most of the time this problem can be resolved by updating 
#' \code{httr} to the most recent version.  If that doesn't work, a 
#' certificate file can be manually passed via the \code{config} argument.  
#' The \code{redcapAPI} wiki has a more detailed tutorial on how to 
#' find and pass an SSL certificate to the API call 
#' (\url{https://github.com/nutterb/redcapAPI/wiki/Manually-Setting-an-SSL-Certificate-File}).
#' 
#' Additional Curl option can be set in the \code{config} argument.  See the documentation
#' for \code{httr::config} and \code{httr:httr_options} for more Curl options.
#' 
#' @author Jeffrey Horner
#' 
#' @references 
#' This functionality were originally developed by Jeffrey Horner in the 
#' \code{redcap} package.
#' \url{https://github.com/vubiostat/redcap}
#' 
#' A tutorial on configuring the REDCap user rights for the API is found at 
#' \url{https://github.com/nutterb/redcapAPI/wiki/Setting-the-User-Rights-to-Grant-API-Access}
#' 
#' A tutorial on requesting and obtaining your API token is found at
#' \url{https://github.com/nutterb/redcapAPI/wiki/Finding-Your-REDCap-API-Token}
#' 
#' A tutorial on finding your API url is found at
#' \url{https://github.com/nutterb/redcapAPI/wiki/Finding-your-REDCap-API-URL}
#' 
#' A tutorial for finding and using alternate SSL certificates is found at 
#' \url{https://github.com/nutterb/redcapAPI/wiki/Manually-Setting-an-SSL-Certificate-File}
#' 
#' @examples
#' \dontrun{
#' rcon <- redcapConnection(url=[YOUR_REDCAP_URL], token=[API_TOKEN])
#' 
#' options(redcap_api_url=[YOUR_REDCAP_URL])
#' rcon <- redcapConnection(token=[API_TOKEN])
#' }
#' 

redcapConnection <-
function(url=getOption('redcap_api_url'),token,conn,project, config=list())
{
   if (is.na(url) && missing(conn))
      stop("Need one of url or conn")
   if (!is.na(url))
   {
      if (missing(token))
         stop("Need an API token")
      return(
         structure(
            list(url=url,token=token, config=config),
            class='redcapApiConnection'
         )
      )
   }
   else
   {
      if (missing(project))
         stop("Need a project_id specified in project variable")
      return(
         structure(
            list(conn=conn,project=project),
            class='redcapDbConnection'
         )
      )
   }
}
