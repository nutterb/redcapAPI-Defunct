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
