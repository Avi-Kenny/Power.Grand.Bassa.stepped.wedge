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

create_dataset <- function(n, parallel_inner, levels_row) {
  
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
