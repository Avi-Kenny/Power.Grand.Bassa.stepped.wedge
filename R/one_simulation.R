#' Run a single simulation
#'
#' @return A list returned by `perform_analysis`

one_simulation <- function() {
  
  # Take a sample from population
  sample <- take_sample(
    sampling_frame = C$sampling_frame,
    n_clusters = L$sample_size,
    type = "SRS"
  )
  
  # Store number of households in sample
  
  # Generate data for sample
  dataset <- create_dataset(
    sample = sample,
    program_effect = L$program_effect,
    re_comm_sd = 0.1,
    re_tx_sd = 0.1,
    show_progress = FALSE
  )
  
  # Transform dataset for analysis
  dataset_trans <- transform_dataset(
    dataset = dataset,
    recall_years = 5
  )
  
  # Perform statistical analysis
  results <- perform_analysis(
    dataset = dataset_trans,
    method = L$method
  )
  
  return(c(
    results,
    list(num_hh=sum(sample$num_hh))
  ))
  
}
