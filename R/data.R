#' @docType data
#' @name TestCase01_MetaData
#' @title REDCap Test Case 01: Meta Data / Data Dictionary
#' 
#' @description These data are the data dictionary for a data set consisting 
#' of a cross-sectional, single center, single form, single arm 
#' design. This is available to assist with testing.  The data 
#' dictionary is designed to include at least one column of 
#' each data type.
#' 
#' @format A data frame with 31 rows and 18 columns.
#' 
#' @source Randomly generated and meaningless data.

"TestCase01_MetaData"

#' @docType data
#' @name TestCase01_Data
#' @title REDCap Test Case 01: Formatted Data
#' 
#' @description These data are the formatted data for a data set consisting 
#' of a cross-sectional, single center, single form, single arm 
#' design.  The data are what should be an expected result from 
#' a call to \code{exportRecords} with the default values.
#' 
#' @format A data frame with 100 rows and 36 columns.

"TestCase01_Data"

#' @docType data
#' @name TestCase01_DataRaw
#' @title REDCap Test Case 01: Raw Data
#' 
#' @description These data are the formatted data for a data set consisting 
#' of a cross-sectional, single center, single form, single arm 
#' design.  The data are what should be an expected result from 
#' a call to \code{exportRecords} with the arguments 
#' \code{factors = FALSE, labels = FALSE, dates = FALSE, survey = FALSE,}
#' \code{dag = FALSE, form_complete_auto = FALSE}
#' 
#' @format A data frame with 100 rows and 36 columns.

"TestCase01_DataRaw"