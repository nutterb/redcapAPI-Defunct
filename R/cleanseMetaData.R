#' @name cleanseMetaData
#' @title Clean Meta Data of UTF Characters
#' 
#' @description There have been isolated cases observed where certain 
#'   characters in the data dictionary prevent it from being downloaded
#'   correctly.  In one case, the data dictionary could not be downloaded
#'   at all through the API.  It is suspected that these problematic 
#'   characters are a result of copying and pasting text out of word 
#'   processing programs.  The problematic characters are not necessarily 
#'   visible and their exact location can be difficult to identify.  As 
#'   a last resort, \code{cleanseMetaData} can read a meta data file 
#'   downloaded through the user interface, purge it of any UTF-8 characters,
#'   and write an alternate data dictionary that contains only ASCII 
#'   characters.  
#'   
#' @param meta_data_file \code{character(1)} the path to a meta data file 
#'   that has been downloaded using the REDCap user interface.
#' @param meta_data_clean \code{character(1)} the path of the file to which
#'   the cleaned meta data will be written.
#'

cleanseMetaData <- function(meta_data_file, meta_data_clean,
                            overwrite = FALSE)
{
  coll <- checkmate::makeAssertCollection()
  
  checkmate::assert_character(x = meta_data_file,
                              len = 1,
                              add = coll)
  
  checkmate::assert_character(x = meta_data_clean,
                              len = 1,
                              add = coll)
  
  checkmate::reportAssertions(coll)
  
  if (!file.exists(meta_data_file)){
    coll$push(sprintf("File not found: %s", meta_data_file))
  }
  
  if (file.exists(meta_data_clean) && !overwrite){
    coll$push(sprintf("File exists and overwrite is set to FALSE: %s", 
                      meta_data_clean))
  }
  
  if (meta_data_clean == meta_data_file){
    coll$push(sprintf("%s %s", 
                      "Sorry. I won't allow you to overwrite your file. ",
                      "Please use a different path for 'meta_data_clean."))
  }
  
  checkmate::reportAssertions(coll)
  
  if (file.exists(meta_data_clean) && overwrite){
    warning("Attempting to overwrite ", meta_data_clean)
  }
  
  dd <- readLines(meta_data_file)
  dd <- paste0(dd, collapse = "\n")
  dd <- iconv(dd, 
              from = "utf8", 
              to = "ASCII", 
              sub = "")
  
  dd <- read.csv(text = dd, 
                 stringsAsFactors = FALSE)
  
  write.csv(dd, 
            meta_data_clean, 
            row.names = FALSE, 
            na = "")
}
