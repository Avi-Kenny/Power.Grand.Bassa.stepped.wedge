# FN: Create dataset
# - Inputs
#     - `sampling_frame`: GrandBassa2020SamplingFrame_truncated.xlsx
#     - `parallel_inner`: T/F; whether to parallelize inner computations of dataset creation
#     - `levels_row`: a list of simulation levels
#     - `program_effect`: TO DO
# - Output
#     - A data frame containing the following fields:
#         - `patient_id`: unique ID of an individual
#         - `sex`: individual's sex
#         - `...`: ...
# - Description
#     - This function creates a simulated dataset

create_dataset <- function(
  sampling_frame, parallel_inner, levels_row, program_effect
) {
  
  population <- list()
  woman_id <- 0
  
  # Generate population
  for (i in 1:length(sampling_frame$community_id)) {
    for (j in 1:sampling_frame$num_hh[i]) {
      
      # !!!!! Need to account for ICC somewhere here
      
      # Generate number of women (age 18-49) in household
      # Unif{0,1,2,3}; 1.5 women per hh is roughly correct
      num_women = sample(0:3, size=1)
      
      if (num_women!=0) {
        for (k in 1:num_women) {
          
          # Generate birth history
          # !!!!! Need to specify effect size here via `program_effect` argument
          # !!!!! Currently Unif{18,19,...,49}; change this to match actual distribution
          woman_age <- sample(18:49, 1) # !!!!! Placeholder
          
          # Add row to data frame
          # !!!!! Note: it will be important later to remember that ~25% of households are not included in this data frame; sampling should be done from the sampling frame, not this list
          woman_id <- woman_id + 1
          population[[woman_id]] <- list(
            "woman_id" = woman_id,
            "community_id" = sampling_frame$community_id[i],
            "household_id" = j,
            "admin_district" = 2, # !!!!!
            "health_district" = 3, # !!!!!
            "birth_history" = birth_history(woman_age)
          )
          
        }
      }
    }
  }
  
  # !!!!! Parallelize later
  # generate_data_one_community <- function(community_id) {
  #   return(c(
  #     "patient_id" = patient_id
  #   ))
  # }
  # if (parallel_inner == TRUE) {
  #   # Run in parallel
  #   clusterExport(cl, cluster_export)
  #   df_list <- parLapply(cl, 1:n, generate_data_one_community)
  # } else {
  #   # Run in series
  #   df_list <- lapply(1:n, generate_data_one_community)
  # }
  
  # Return population dataset
  return(population)
  
}
