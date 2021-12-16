#' @name dataFrameToString
#' @title Convert Data Frame to String Format 
#' 
#' @details Converts a data frame to the appropriate string format 
#'   to facility importing records, arms, events, etc.
#'   
#' @param data \code{data.frame}. Each element in the data frame must
#'   be an atomic vector.
#'   
#' @export

dataFrameToString <- function(data)
{
  coll <- checkmate::makeAssertCollection()
  
  checkmate::assert_data_frame(x = data, 
                               types = "atomic",
                               add = coll)
  
  checkmate::reportAssertions(coll)
  
  paste0(
    utils::capture.output(
      utils::write.table(data, 
                         sep = ",",
                         col.names = TRUE,
                         row.names = FALSE)
    ),
    collapse = "\n"
  )
}