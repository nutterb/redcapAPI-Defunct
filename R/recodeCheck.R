#' @name recodeCheck
#' @export recodeCheck
#' @importFrom Hmisc label.default
#' @importFrom Hmisc label.data.frame
#' @importFrom Hmisc 'label<-.default'
#' @importFrom Hmisc 'label<-.data.frame'
#' @importFrom Hmisc '[.labelled'
#' @importFrom Hmisc print.labelled
#' 
#' @title Change labelling of \code{checkbox} variables
#' @description Rewrites the labelling of \code{checkbox} variables from 
#'   Checked/Unchecked to Yes/No (or some other user-specified labelling).
#'   
#' @param df A data frame, presumably retrieved from REDCap, though not a 
#'   strict requirement.
#' @param vars Optional character vector of variables to convert.  If left 
#'   missing, all of the variables in \code{df} that are identified as 
#'   \code{checkbox} variables are relabelled.  See 'Details' for more about 
#'   identifying \code{checkbox} variables.
#' @param old A character vector to be passed to \code{factor}.  
#'   This indicates the levels to be replaced and their order.
#' @param new A character vector of labels to replace the values in 
#'   \code{levels}.  The first value becomes the reference value.
#' @param reverse For convenience, if the user would prefer to reverse the 
#'   order of the elements in \code{levels} and \code{labels}, 
#'   simply set this to \code{TRUE}.
#'   
#' @details 
#' \code{checkbox} variables are \emph{not} identified using the metadata 
#' from the REDCap database.  Instead, variables are scanned, and those 
#' variables in which every value is in \code{levels} are assumed to be 
#' \code{checkbox} variables.  
#' 
#' Realistically, this could be used to relabel any set of factors with 
#' identical labels, regardless of the data source.  The number of labels is 
#' not limited, but \code{levels} and \code{labels} should have the same length.
#' 
#' The actual code to perform this is not particularly difficult 
#' (\code{df[checkbox] <- lapply(df[checkbox], factor, levels=levels, labels=labels)}), 
#' but \code{checkbox} variables are common enough in REDCap 
#' (and the Checked/Unchecked scheme so unpalatable) that a quick way to 
#' replace the labels was highly desirable
#' 
#' @author Benjamin Nutter
#' 
#' @examples
#' \dontrun{
#' > #*** Note: I cannot provide working examples without
#' > #*** compromising security.  Instead, I will try to 
#' > #*** offer up sample code with the matching results
#' > 
#' > 
#' > #*** Create the connection object
#' > rcon <- redcapConnection(url=[YOUR_REDCAP_URL], token=[API_TOKEN])
#' > 
#' > #* Default appearance after export
#' > Prenatal <- exportRecords(rcon, fields=c("maternal_mrn", "consults"))
#' Warning message:
#'   In exportMetaData.redcapApiConnection(rcon) : NAs introduced by coercion
#' > head(Prenatal)
#' maternal_mrn   redcap_event_name consults___1
#' 1 0d714b1efc778d8e738c7f8eb399d224 mfm_episode_1_arm_1    Unchecked
#' 2 0ef1975c93365e2246038d317838816d mfm_episode_1_arm_1    Unchecked
#' 3 a0d81f6f1e55de0770825f460e8e4894 mfm_episode_1_arm_1      Checked
#' 4 a1a0a470c658d05e7df636607fc89bd4 mfm_episode_1_arm_1      Checked
#' 5 a577d8066a12536adb97df9d66ad7d39 mfm_episode_1_arm_1      Checked
#' 6 a5e9beb28c9883b8bd8961481064037e mfm_episode_1_arm_1    Unchecked
#' consults___2 consults___3 consults___4 consults___5 consults___6 consults___7
#' 1    Unchecked      Checked    Unchecked    Unchecked      Checked    Unchecked
#' 2      Checked      Checked    Unchecked    Unchecked    Unchecked    Unchecked
#' 3    Unchecked      Checked    Unchecked    Unchecked    Unchecked    Unchecked
#' 4    Unchecked    Unchecked    Unchecked    Unchecked    Unchecked    Unchecked
#' 5    Unchecked      Checked    Unchecked    Unchecked    Unchecked    Unchecked
#' 6    Unchecked    Unchecked    Unchecked    Unchecked    Unchecked    Unchecked
#' consults___8 consults___9 consults___10 consults___11 consults___12
#' 1    Unchecked    Unchecked     Unchecked     Unchecked     Unchecked
#' 2    Unchecked    Unchecked     Unchecked     Unchecked     Unchecked
#' 3    Unchecked    Unchecked     Unchecked     Unchecked     Unchecked
#' 4    Unchecked    Unchecked     Unchecked     Unchecked     Unchecked
#' 5      Checked    Unchecked     Unchecked     Unchecked     Unchecked
#' 6    Unchecked    Unchecked     Unchecked     Unchecked     Unchecked
#' consults___13 consults___14 consults___90
#' 1     Unchecked     Unchecked     Unchecked
#' 2     Unchecked     Unchecked       Checked
#' 3     Unchecked     Unchecked     Unchecked
#' 4     Unchecked     Unchecked     Unchecked
#' 5     Unchecked     Unchecked     Unchecked
#' 6     Unchecked     Unchecked       Checked
#' > 
#' > 
#' > #* Use the default settings to recode as No/Yes
#' > Prenatal2 <- recodeCheck(Prenatal)
#' > head(Prenatal2)
#' maternal_mrn   redcap_event_name consults___1
#' 1 0d714b1efc778d8e738c7f8eb399d224 mfm_episode_1_arm_1           No
#' 2 0ef1975c93365e2246038d317838816d mfm_episode_1_arm_1           No
#' 3 a0d81f6f1e55de0770825f460e8e4894 mfm_episode_1_arm_1          Yes
#' 4 a1a0a470c658d05e7df636607fc89bd4 mfm_episode_1_arm_1          Yes
#' 5 a577d8066a12536adb97df9d66ad7d39 mfm_episode_1_arm_1          Yes
#' 6 a5e9beb28c9883b8bd8961481064037e mfm_episode_1_arm_1           No
#' consults___2 consults___3 consults___4 consults___5 consults___6 consults___7
#' 1           No          Yes           No           No          Yes           No
#' 2          Yes          Yes           No           No           No           No
#' 3           No          Yes           No           No           No           No
#' 4           No           No           No           No           No           No
#' 5           No          Yes           No           No           No           No
#' 6           No           No           No           No           No           No
#' consults___8 consults___9 consults___10 consults___11 consults___12
#' 1           No           No            No            No            No
#' 2           No           No            No            No            No
#' 3           No           No            No            No            No
#' 4           No           No            No            No            No
#' 5          Yes           No            No            No            No
#' 6           No           No            No            No            No
#' consults___13 consults___14 consults___90
#' 1            No            No            No
#' 2            No            No           Yes
#' 3            No            No            No
#' 4            No            No            No
#' 5            No            No            No
#' 6            No            No           Yes
#' > 
#' > 
#' > 
#' > #* Alter the defaults to recode as Received Consult/No Consult Necessary
#' > Prenatal3 <- recodeCheck(Prenatal, 
#' +                          levels=c("Checked", "Unchecked"), 
#' +                          labels=c("Received Consult", "No Consult Necessary"))
#' > head(Prenatal3)
#' maternal_mrn   redcap_event_name         consults___1
#' 1 0d714b1efc778d8e738c7f8eb399d224 mfm_episode_1_arm_1 No Consult Necessary
#' 2 0ef1975c93365e2246038d317838816d mfm_episode_1_arm_1 No Consult Necessary
#' 3 a0d81f6f1e55de0770825f460e8e4894 mfm_episode_1_arm_1     Received Consult
#' 4 a1a0a470c658d05e7df636607fc89bd4 mfm_episode_1_arm_1     Received Consult
#' 5 a577d8066a12536adb97df9d66ad7d39 mfm_episode_1_arm_1     Received Consult
#' 6 a5e9beb28c9883b8bd8961481064037e mfm_episode_1_arm_1 No Consult Necessary
#' consults___2         consults___3         consults___4
#' 1 No Consult Necessary     Received Consult No Consult Necessary
#' 2     Received Consult     Received Consult No Consult Necessary
#' 3 No Consult Necessary     Received Consult No Consult Necessary
#' 4 No Consult Necessary No Consult Necessary No Consult Necessary
#' 5 No Consult Necessary     Received Consult No Consult Necessary
#' 6 No Consult Necessary No Consult Necessary No Consult Necessary
#' consults___5         consults___6         consults___7
#' 1 No Consult Necessary     Received Consult No Consult Necessary
#' 2 No Consult Necessary No Consult Necessary No Consult Necessary
#' 3 No Consult Necessary No Consult Necessary No Consult Necessary
#' 4 No Consult Necessary No Consult Necessary No Consult Necessary
#' 5 No Consult Necessary No Consult Necessary No Consult Necessary
#' 6 No Consult Necessary No Consult Necessary No Consult Necessary
#' consults___8         consults___9        consults___10
#' 1 No Consult Necessary No Consult Necessary No Consult Necessary
#' 2 No Consult Necessary No Consult Necessary No Consult Necessary
#' 3 No Consult Necessary No Consult Necessary No Consult Necessary
#' 4 No Consult Necessary No Consult Necessary No Consult Necessary
#' 5     Received Consult No Consult Necessary No Consult Necessary
#' 6 No Consult Necessary No Consult Necessary No Consult Necessary
#' consults___11        consults___12        consults___13
#' 1 No Consult Necessary No Consult Necessary No Consult Necessary
#' 2 No Consult Necessary No Consult Necessary No Consult Necessary
#' 3 No Consult Necessary No Consult Necessary No Consult Necessary
#' 4 No Consult Necessary No Consult Necessary No Consult Necessary
#' 5 No Consult Necessary No Consult Necessary No Consult Necessary
#' 6 No Consult Necessary No Consult Necessary No Consult Necessary
#' consults___14        consults___90
#' 1 No Consult Necessary No Consult Necessary
#' 2 No Consult Necessary     Received Consult
#' 3 No Consult Necessary No Consult Necessary
#' 4 No Consult Necessary No Consult Necessary
#' 5 No Consult Necessary No Consult Necessary
#' 6 No Consult Necessary     Received Consult
#' > 
#' > 
#' > 
#' > ## The order of the levels can be rearranged if desired
#' > levels(Prenatal2$consults___90)
#' [1] "No"  "Yes"
#' > levels(Prenatal3$consults___90)
#' [1] "Received Consult"     "No Consult Necessary"
#' }

recodeCheck <- function(df, vars, 
                        old=c("Unchecked", "Checked"), new=c("No", "Yes"), 
                        reverse=FALSE){
    #Hlabel <- require(Hmisc)
    #if (!Hlabel) stop("Please install the 'Hmisc' package.")

  #* If no variable names are provided, check the data frame for all variables in which all values 
  #* are either "Checked" or "Unchecked"
  if (missing(vars)){
    checkbox <- sapply(df, function(x) all(attributes(x)$redcapLabels %in% old))
  }

  #* If variable names are given, ensure that they are checkbox variables.  Ignore them if anything
  #* other than "Checked" or "Unchecked" appears in the values.
  else {
    vars_are_check <- sapply(df, function(x) all(attributes(x)$redcapLabels %in% old))
    vars_not_check <- vars[!vars_are_check]
    if (any(!vars_are_check)) warning(paste0("'", paste(vars[!vars_are_check], collapse = "', '"), 
                                            "' do not appear to be 'checkbox' variables.",
                                            "\nThese variables were not recoded."))
    checkbox <- vars[vars_are_check]
  }

  var.label <- Hmisc::label(df[checkbox])

  #* Utility function for recoding check variables
  recodeFn <- function(v, old=old, new=new, reverse=reverse){
    if (is.factor(v)) v <- redcapFactorFlip(v)
    attributes(v)$redcapLabels <- if (reverse) rev(new) else new
    if (reverse) attributes(v)$redcapLevels <- rev(attributes(v)$redcapLevels)
    return(redcapFactorFlip(v))
  }

  #* Apply the new labels
  df[checkbox] <- lapply(df[checkbox], recodeFn, old, new, reverse)
  Hmisc::label(df[, checkbox], self=FALSE) <- var.label
  return(df)
}
