#' @rdname allocationTable
#' @export allocationTable_offline
#' @importFrom stringr str_split_fixed 

allocationTable_offline <- function(meta_data, random, strata=NULL, 
                            group=NULL, dag.id=NULL, 
                            replicates, block.size, 
                            block.size.shift = 0,
                            seed.dev=NULL, seed.prod=NULL, ...){
  
  error.flag <- 0
  error.msg <- NULL
  
  warn.flag <- 0
  warn.msg <- NULL
  
  #* Establish the meta_data table
  meta_data <- read.csv(meta_data,
                        col.names=c('field_name', 'form_name', 'section_header', 
                                    'field_type', 'field_label', 'select_choices_or_calculations', 
                                    'field_note', 'text_validation_type_or_show_slider_number', 
                                    'text_validation_min', 'text_validation_max', 'identifier', 
                                    'branching_logic', 'required_field', 'custom_alignment', 
                                    'question_number', 'matrix_group_name', 'matrix_ranking'),
                        stringsAsFactors=FALSE)
  
  #* A utility function to extract the coded values from the meta_data
  redcapChoices <- function(v, meta_data){
    if (meta_data$field_type[meta_data$field_name == v] %in% c("dropdown", "radio")){
      choice_str <- meta_data$select_choices_or_calculations[meta_data$field_name == v]
      choice_str <- unlist(strsplit(choice_str, " [|] "))
      return(stringr::str_split_fixed(choice_str, ", ", 2)[, 1])
    }
    else if (meta_data$field_type[meta_data$field_name == v] %in% c("yesno", "true_false"))
      return(0:1)
    else stop(paste0("'", v, "' is not a valid variable for stratification/randomization"))
  }
  
  #***************************************
  #* Parameter Checking
  #* 1. Verifying that 'random' is not missing
  #* 2. random, strata and group are characters
  #* 3. random and group have length 1
  #* 4. all fields in 'random', 'strata', and 'group' exist in meta_data 
  #* 5. Calculate n_strata
  #* 6. Verify 'replicates' is not missing and is numeric
  #* 7. If 'blocks.size' is missing, set it equal to 'replicates'.
  #*    If not missing, it must be numeric
  #* 9. block.size must be a multiple of n_strata
  #* 9. First element in block.size.shift must be 0
  #* 10. block.size.shift must be strictly increasing in the interval [0, 1)
  #* 11. block.size.shift must have the same length as block.size
  #* 12. The sum of all of the blocks must add up to replicates
  #* 13. Check if all blocks conform to blocking design (warning produced)
  #* 14. seed.dev is not NULL and has length 1 or n_strata
  #* 15. seed.prod is not NULL and has length 1 or n_strata
  #* 16. no pairwise elements of seed.dev are equal to seed.prod
  
  #* 1. Verifying that 'random' is not missing
  if (missing(random)){
    error.flag <- error.flag + 1
    error.msg <- c(error.msg,
                   paste0(error.flag, ": No value is given for 'random'"))
  }
  
  #* 2. random, strata and group are characters
  if (!all(sapply(c(random, strata, group), is.character))){
    error.flag <- error.flag + 1
    error.msg <- c(error.msg,
                   paste0(error.flag, ": 'random', 'strata', and 'group' must ",
                          "all be character class"))
  }
  
  #* 3. random and group have length 1
  if (!all(sapply(c(random, group), length) == 1)){
    error.flag <- error.flag + 1
    error.msg <- c(error.msg,
                   paste0(error.flag, ": 'random' and 'group' must have length 1"))
  }
  
  #* 4. all fields in 'random', 'strata', and 'group' exist in meta_data
  #* Verify that all given fields exist in the database
  if (!all(c(random, strata, group) %in% meta_data$field_name)){
    not_found <- c(random, strata, group)
    not_found <- not_found[!not_found %in% meta_data$field_name]
    
    error.flag <- error.flag + 1
    error.msg <- c(error.msg,
                   paste0(error.flag, ": '", paste0(not_found, collapse="', '"), 
                          "' are not found in the REDCap database"))
  }
    
  #* 5. Calculate n_levels
  #* randomization levels
  random_levels <- redcapChoices(random, meta_data)
  n_levels <- length(random_levels)
  
  #* stratification groups
  strata <- c(strata, group)
  strata_levels <- lapply(strata, redcapChoices, meta_data)
  names(strata_levels) <- strata
  if (!is.null(dag.id)) strata_levels[['redcap_data_access_group']] <- dag.id
  
  #* Allocation table
  allocation <- expand.grid(strata_levels)
  if (nrow(allocation) == 0) allocation <- data.frame(place.holding.strata=1)
    
  n_strata <- nrow(allocation)
  
  #* 6. Verify 'replicates' is not missing and is numeric
  if (ifelse(missing(replicates), TRUE, !is.numeric(replicates))){
    error.flag <- error.flag + 1
    error.msg <- c(error.msg,
                   paste0(error.flag, ": 'replicates' is a required argument and must be numeric"))
  }
  
  #* 7. If 'block.size' is missing, set it equal to 'replicates'.
  if (missing(block.size)){
    block.size <- replicates
    warn.flag <- warn.flag + 1
    warn.msg <- c(warn.msg,
                  paste0(warn.flag, ": 'block.size' was not provided.  The value of 'replicates' is used"))
  }
  else{
    if (!is.numeric(block.size)){
      error.flag <- error.flag + 1
      error.msg <- c(error.msg,
                     paste0(error.msg, ": 'block.size' must be numeric."))
    }
  }
  
  #* 8. block.size must be a multiple of n_levels
  if (any((block.size %% n_levels) != 0)){
    error.flag <- error.flag + 1
    error.msg <- c(error.msg,
                   paste0(error.msg, ": 'block.size' must be a multiple of ", n_levels))
  }
  
  #* 9. First element in block.size.shift must be 0
  if (block.size.shift[1] != 0){
    error.flag <- error.flag + 1
    error.msg <- c(error.msg,
                   paste0(error.msg, ": The first element of 'block.size.shift' must be 0"))
  }

  #* 10. block.size.shift must be strictly increasing in the interval [0, 1)
  if (!all(block.size.shift >= 0) | !all(block.size.shift < 1) | 
        !all(diff(block.size.shift) > 0)){
    error.flag <- error.flag + 1
    error.msg <- c(error.msg,
                   paste0(error.msg, ": 'block.size.shift' must be strictly increasing ",
                          "on the interval [0, 1)"))
  }
  
  #* 11. block.size.shift must have the same length as block.size
  if (length(block.size) != length(block.size.shift)){
    error.flag <- error.flag + 1
    error.msg <- c(error.msg,
                   paste0(error.msg, ": 'block.size' and 'block.size.shift' must have the same length"))
  }
  
  #* 12. The sum of all of the blocks must add up to replicates
  max.n <- cumsum(diff(c(block.size.shift * replicates, replicates)))
  
  blocks <- NULL
  for (i in 1:length(block.size)){
    while(sum(blocks) < max.n[i]){
      blocks <- c(blocks, block.size[i])
    }
  }
  
  Blocks <- data.frame(block.num = 1:length(blocks),
                       block.size = blocks,
                       cum.n = cumsum(blocks))
  Blocks <- merge(Blocks, data.frame(block.size=block.size,
                                     max.n = max.n),
                  by="block.size", sort=FALSE)
  Blocks$conform <- with(Blocks, cum.n <= max.n)
  
  if (sum(Blocks$block.size) != replicates){
    warn.flag <- warn.flag + 1
    warn.msg <- c(warn.msg,
                   paste0(warn.flag, ": The sum of the block sizes should add up to 'replicates'\n",
                          "  Please review the Blocks attribute and consider changing your blocking scheme"))
  }
  
  #* 13. Check if all blocks conform to blocking design (warning produced)
  if (!all(Blocks$conform)){
    warn.flag <- warn.flag + 1
    warn.msg <- c(warn.msg,
                  paste0(warn.flag, ": The blocking design did not conform exactly to specifications\n",
                         "  Please review the Blocks attribute and consider changing your blocking scheme"))
  }
  
  #* 14. seed.dev is not NULL and has length 1 or n_strata
  if (ifelse(is.null(seed.dev), TRUE, !length(seed.dev) %in% c(1, n_strata))){
    error.flag <- error.flag + 1
    error.msg <- c(error.msg, 
                   paste0(error.flag, ": 'seed.dev' is a required argument and must be length 1 or ", n_strata))
  }
  
  #* 15. seed.prod is not NULL and has length 1 or n_strata
  if (ifelse(is.null(seed.prod), TRUE, !length(seed.prod) %in% c(1, n_strata))){
    error.flag <- error.flag + 1
    error.msg <- c(error.msg, 
                   paste0(error.flag, ": 'seed.prod' is a required argument and must be length 1 or ", n_strata))
  }
  
  #* 16. no pairwise elements of seed.dev are equal to seed.prod
  if (any(seed.dev == seed.prod)){
    error.flag <- error.flag + 1
    error.msg <- c(error.msg,
                   paste0(error.msg, ": No pairwise elements of 'seed.dev' and 'seed.prod' may be equal"))
  }
  
  if (length(seed.dev) == 1) seed.dev <- seed.dev + ((1:n_strata)-1)*100
  if (length(seed.prod) == 1) seed.prod <- seed.prod + ((1:n_strata)-1)*100
  
  if (warn.flag) warning(paste(warn.msg, collapse="\n"))
  if (error.flag) stop(paste(error.msg, collapse="\n"))
  
  #* Randomization function
  Randomization <- function(choices, Blocks, seed){
    set.seed(seed) #* set the seed
    #* Randomizations
    do.call("c", lapply(Blocks$block.size, function(x) sample(rep(choices, length.out=x), x)))
  }
  
#   return(list(allocation, Blocks, random_levels, seed.dev))
  
  #* Generate an allocation table for each stratum (Development)
  dev_allocate <- lapply(1:nrow(allocation),
                       function(r){
                         a <- allocation[r, , drop=FALSE]
                         #* extend the length of the stratum data frame to accomodate the sampling
                         a <-  a[rep(row.names(a), sum(Blocks$block.size)), , drop=FALSE]
                         a[[random]] <- Randomization(random_levels, Blocks, seed.dev[r])
                         return(a)
                       })
  
  #* Combine the allocation tables
  dev_allocate <- do.call("rbind", dev_allocate)
  
  #* reorder the allocation table for uploading to REDCap
  dev_allocate <- dev_allocate[, c(random, strata), drop=FALSE]
  rownames(dev_allocate) <- NULL  
  
  
  #* Generate an allocation table for each stratum (Production)
  prod_allocate <- lapply(1:nrow(allocation),
                         function(r){
                           a <- allocation[r, , drop=FALSE]
                           #* extend the length of the stratum data frame to accomodate the sampling
                           a <-  a[rep(row.names(a), sum(Blocks$block.size)), , drop=FALSE]
                           a[[random]] <- Randomization(random_levels, Blocks, seed.prod[r])
                           return(a)
                         })
  #* Combine the allocation tables
  prod_allocate <- do.call("rbind", prod_allocate)
  
  #* reorder the allocation table for uploading to REDCap
  prod_allocate <- prod_allocate[, c(random, strata), drop=FALSE]
  rownames(prod_allocate) <- NULL  
  

  return(list(dev_allocate = dev_allocate, dev_seed = seed.dev,
              prod_allocate = prod_allocate, prod_seed = seed.prod,
              blocks = Blocks))
}
