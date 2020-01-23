# FN: Create dataset
# - Inputs
#     - `n`: number of individuals in the dataset
#     - `parallel`: T/F; whether to parallelize dataset creation
#     - `levels_row`: a list of simulation levels
# - Output
#     - A data frame containing the following fields:
#         - `patient_id`: unique ID of an individual
#         - `sex`: individual's sex
#         - `...`: ...
# - Description
#     - This function creates a simulated dataset

create_dataset <- function(
  sampling_frame, parallel_inner, levels_row
) {
  
  df_pop <- data.frame(
    "woman_id" = integer(),
    "community_id" = integer(),
    "household_id" = integer(),
    "admin_district" = character(),
    "health_district" = character(),
    # !!!!! add birth history variables
    stringsAsFactors=FALSE
  )
  
  # Initialize counter
  woman_id <- 0
  
  # Generate population
  for (i in sampling_frame$community_id) {
    for (j in 1:sampling_frame[1,"num_hh"]) {
      
      # Generate number of women (age 18-49) in household
      # !!!!! Temporary: 0-3 (equal prob)
      num_women = sample(0:3, size=1)
      
      if (num_women!=0) {
        for (k in 1:num_women) {
          
          # Add row to data frame
          # !!!!! Note: it will be important later to remember that ~25% of households are not included in this data frame; sampling should be done from the sampling frame, not this list
          df_pop[nrow(df_pop)+1,] <- list(
            "woman_id" = (woman_id <- woman_id+1),
            "community_id" = i,
            "household_id" = j,
            "admin_district" = character(),
            "health_district" = character()
            # !!!!! add birth history variables
          )
          
          # !!!!!
          
          
        }
      }
      
    }
  }
  
  create_row <- function(i) {
    
    # Calculate `patient_id`
    patient_id <- i
    
    # Do something with levels_row
    myvar <- levels_row$dimension_1
    
    # Calculate `sex`
    sex <- sample(c(0,1), size=1)
    
    return(c(
      "patient_id" = patient_id,
      "sex" = sex
    ))
    
  }
  
  if (parallel_inner == TRUE) {
    # Run in parallel
    clusterExport(cl, cluster_export)
    df_list <- parLapply(cl, 1:n, create_row)
  } else {
    # Run in series
    df_list <- lapply(1:n, create_row)
  }
  
  # Convert list to data frame
  df <- data.frame(
    matrix(
      unlist(df_list),
      nrow = length(df_list),
      byrow = TRUE
    )
  )
  names(df) <- names(df_list[[1]])
  
  # Return data frame
  return(df)
  
}
