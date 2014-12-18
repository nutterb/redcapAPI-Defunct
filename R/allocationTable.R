allocationTable <- function(rcon, random, strata=NULL, 
                            group=NULL, dag_id=NULL, 
                            replicates, block.size, 
                            seed=NULL, 
                            proj=NULL, ...)
  UseMethod("allocationTable")

allocationTable.redcapDbConnection <- function(rcon, random, strata=NULL, 
                                               group=NULL, dag_id=NULL, 
                                               replicates, block.size, 
                                               seed=NULL, 
                                               proj=NULL, ...){
    message("Please accept my apologies.  The exportUsers method for redcapDbConnection objects\n",
            "has not yet been written.  Please consider using the API.")
  }

allocationTable.redcapApiConnection <- function(rcon, random, strata=NULL, 
                                                group=NULL, dag_id=NULL, 
                                                replicates, block.size, 
                                                seed=NULL, 
                                                proj=NULL, ...){
  
  #* Establish the meta_data table
  meta_data <- if (is.null(proj$meta_data)) exportMetaData(rcon) else proj$meta_data
  
  if (missing(block.size)) block.size <- replicates
  
  #* Verify that all given fields exist in the database
  if (!all(c(random, strata, group) %in% meta_data$field_name)){
    not_found <- c(random, strata, group)
    not_found <- not_found[!not_found %in% meta_data$field_name]
    stop(paste0("',", paste0(not_found, collapse="', '"), "' are not found in the REDCap database"))
  }
  
  #* Function to extract the coded values from the meta_data
  redcapChoices <- function(v, meta_data){
    if (meta_data$field_type[meta_data$field_name == v] %in% c("dropdown", "radio", "checkbox")){
      choice_str <- meta_data$select_choices_or_calculations[meta_data$field_name == v]
      choice_str <- unlist(strsplit(choice_str, " [|] "))
      return(stringr::str_split_fixed(choice_str, ", ", 2)[, 1])
    }
    else if (meta_data$field_type[meta_data$field_name == v] %in% c("yesno", "true_false"))
      return(0:1)
    else stop(paste0("'", v, "' is not a valid variable for stratification/randomization"))
  }
  
  #* randomization levels
  random_levels <- redcapChoices(random, meta_data)
  
  #* readjust block.size 
  #* block.size has to be a multiple of length(random_levels) or the sampling
  #* won't have equal probability
  if (block.size %% length(random_levels)){
    block.size <- ceiling(block.size/length(random_levels)) * length(random_levels)
    warning(paste0("'block.size is not a multiple of number of randomization options.\n",
                   "It has been changed to ", block.size))
  }

  #* stratification groups
  strata <- c(strata, group)
  strata_levels <- sapply(strata, redcapChoices, meta_data)
  if (!is.null(dag_id)) strata_levels[['redcap_data_access_group']] <- dag_id
  
  #* Allocation table
  allocation <- expand.grid(strata_levels)
  
  #* A value of seed is required.
  if (is.null(seed)) 
    stop(paste0("'seed' must be a numeric vector of length 1 or length ", nrow(allocation)))
  
  #* If the length of seed is 1, extend it so each stratum has an independent
  #* randomization
  if (length(seed) == 1)
    seed <- seed + (1:nrow(allocation)-1) * 100
  
  #* Randomization function
  Randomization <- function(choices, block.size, replicates, seed){
    set.seed(seed) #* set the seed
    blocks <- 1:ceiling(replicates/block.size) #* determine the number of blocks
    #* Randomizations
    do.call("c", lapply(blocks, function(x) sample(rep(choices, length.out=block.size), block.size)))
  }
  
  #* Generate an allocation table for each stratum
  allocation <- lapply(1:nrow(allocation),
                       function(r){
                         a <- allocation[r, ]
                         #* extend the length of the stratum data frame to accomodate the sampling
                         a <-  a[rep(row.names(a), ceiling(replicates/block.size)*block.size), ]
                         a[[random]] <- Randomization(random_levels, block.size, replicates, seed[r])
                         return(a)
                       })

  #* Combine the allocation tables
  allocation <- do.call("rbind", allocation)
  
  #* reorder the allocation table for uploading to REDCap
  allocation <- allocation[, c(random, strata)]
  rownames(allocation) <- NULL  
  
  return(allocation)
}
