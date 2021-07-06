#' Run a single simulation
#'
#' @return A list returned by `perform_analysis`

one_simulation <- function() {
  
  # # !!!!! Testing
  # set.seed(2112)
  # C <- list(sampling_frame=sampling_frame)
  # L <- list(sample_size=1000, program_effect=0.2, tvte=FALSE)
  
  # Take a sample from population
  sample <- take_sample(
    sampling_frame = C$sampling_frame,
    n_clusters = L$sample_size,
    type = "SRS"
  )
  
  # Generate data for sample
  dataset <- create_dataset(
    sample = sample,
    program_effect = L$program_effect,
    re_comm_sd = 0.1,
    re_tx_sd = 0.1,
    tvte = L$tvte,
    show_progress = FALSE
  )
  
  # Transform dataset for analysis
  dataset_trans <- transform_dataset(
    dataset = dataset,
    recall_years = 5
  )
  
  # Perform statistical analysis
  results <- perform_analysis(
    dataset_trans = dataset_trans,
    method = L$method
  )
  
  return(c(
    results,
    list(num_hh=sum(sample$num_hh))
  ))
  
}
