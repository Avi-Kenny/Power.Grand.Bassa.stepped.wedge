# FN: Run one simulation
# - Inputs
#     - `i`: unused; facilitates parallelization
#     - `levels_row`: a one-row data frame containing simulation levels
#     - `constants`: a list containing simulation constants
#     - `dataset`: either a dataset returned by create_dataset() (if `config$dataset`=='one') or NULL (if `config$dataset`=='many')
# - Output
#     - A list containing the results of one simulation, with the following fields:
#         - `sim_index`: index (integer) of the simulation
#         - `result 1`: simulation summary statistic #1
#         - `result 2`: simulation summary statistic #2
#         - `...`: ...
# - Description
#     - This function runs a single simulation
#     - Note: `i` is unused; facilitates parallelization

run_one_simulation <- function(i, levels_row, constants, dataset) {
  
  # Get level/constant variables
  dim_1 <- levels_row$dimension_1
  
  # Create dataset
  if (config$dataset=='many') {
    dataset <- create_dataset(
      sampling_frame = constants$sampling_frame,
      parallel_inner = ifelse(
        config$parallel=="inner", TRUE, FALSE
      ),
      levels_row = levels_row
    )
  }
  
  # Define local variables
  n_inner <- 10
  
  # Run non-parallel code
  # ...
  
  # Define parallel tasks
  # Note: `i` is unused; facilitates parallelization
  my_func <- function(i, levels_row, other_args) {
    
    # ...
    
    return(NA)
    
  }
  
  # Run parallel tasks
  if (config$parallel == "inner") {
    
    # Export variables/functions
    clusterExport(cl, cluster_export)
    
    # Run in parallel
    inner_results <- parLapply(cl, 1:n_inner, my_func,
                               levels_row, "other arg")
    
  } else {
    
    # Run in series
    inner_results <- lapply(1:n_inner, my_func,
                            levels_row, "other arg")
    
  }
  
  # Do stuff with `inner_results`
  # ...
  
  # Return results list
  return (list(
    "sim_index" = levels_row$sim_index,
    "dataset" = dataset,
    "summary" = list(
      "stat_1" = NA,
      "stat_2" = NA
    )
  ))
  
}
