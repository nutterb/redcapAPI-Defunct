compareRedcapVersion <- function(current_version, required_version){
  #* Ultimately, a value of -1, 0, or 1 will be returned.
  #* -1 indicates the current_version is lower than the required version
  #* 0 indicates the versions are the same
  #* 1 indicates the current version is ahead of the required version
  #* In context of asking "is the current version at least as high as the required version
  #*     an -1 means FALSE.  Any other answer is TRUE
  
  #* accept version numbers of the forms x, x.y, and x.y.z
  check_current <- if (is.null(current_version)) FALSE 
    else grepl("(\\d{1,3}|\\d{1,3}[.]\\d{1,3}|\\d{1,3}[.]\\d{1,3}[.]\\d{1,3})", current_version)
  
  #* If a version style is not recognized, or current_version is NULL, set the current version to 'Version Unknown'
  if (!check_current) current_version <- "Version Unknown"
  
  #* if current version is unknown, return -1
  if (current_version == "Version Unknown") return(-1)

  check_current <- grepl("(\\d{1,3}|\\d{1,3}[.]\\d{1,3}|\\d{1,3}[.]\\d{1,3}[.]\\d{1,3})", check_current)
  
  #* If a version style is not recognized, set the current version to 'Version Unknown'
  if (!check_current) current_version <- "Version Unknown"
  
  #* if current version is null or unknown, return -1
  if (is.null(current_version) | current_version == "Version Unknown") return(-1)

  #* compare versions using the function in the utils package
  else return(utils::compareVersion(current_version, required_version))
}
