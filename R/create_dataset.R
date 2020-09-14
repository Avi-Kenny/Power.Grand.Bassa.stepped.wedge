#' Generate one simulated dataset representing the Grand Bassa population
#'
#' @param sampling_frame GrandBassa2020SamplingFrame_truncated.xlsx
#' @param program_effect Percent reduction in U5MR in intervention group
#' @return A list of lists, where each sub-list contains the following: \cr
#'     * `woman_id`: woman ID \cr
#'     * `community_id`: community ID \cr
#'     * `household_id`: household ID \cr
#'     * `admin_district`: admin district ID \cr
#'     * `health_district`: health district ID \cr
#'     * `birth_history`: a list returned by birth_history()

create_dataset <- function(sampling_frame, program_effect) {
  
  population <- list()
  woman_id <- 0
  
  # Generate population
  for (i in 1:length(sampling_frame$community_id)) {
    for (j in 1:sampling_frame$num_hh[i]) {
      
      # Generate number of women (age 15-49) in household
      # Corresponds to "Distribution A"
      num_women = sample(
        x = c(0:5),
        size = 1,
        replace = TRUE,
        prob = c(0.255, 0.651, 0.071, 0.018, 0.003, 0.001)
      )
      
      if (num_women!=0) {
        for (k in 1:num_women) {
          
          # Generate woman's age
          # Corresponds to "Distribution B"
          # !!!!! Consider smoothing this distribution a bit
          woman_age = sample(
            x = c(15:49),
            size = 1,
            replace = TRUE,
            prob = c(
              0.034, 0.038, 0.033, 0.050, 0.043, 0.038, 0.023,
              0.033, 0.026, 0.021, 0.039, 0.032, 0.021, 0.031,
              0.040, 0.041, 0.018, 0.032, 0.033, 0.029, 0.038,
              0.044, 0.025, 0.031, 0.033, 0.037, 0.011, 0.025,
              0.015, 0.008, 0.025, 0.011, 0.011, 0.023, 0.008
            )
          )
          
          # Determine the crossover date from control to intervention
          #     The first month (CMC) at which the program was in the
          #     intervention state
          crossover_date <- 1450 # !!!!! placeholder

          # Generate birth history
          # !!!!! Need to account for ICC somewhere here
          birth_history <- birth_history(
            "woman_age" = woman_age,
            "program_effect" = program_effect,
            "crossover_date" = crossover_date
          )
          
          # Add row to data frame
          # !!!!! Note: it will be important later to remember that ~25% of
          #     households are not included in this data frame; sampling should
          #     be done from the sampling frame, not this list
          woman_id <- woman_id + 1
          population[[woman_id]] <- list(
            "woman_id" = woman_id,
            "community_id" = sampling_frame$community_id[i],
            "household_id" = j,
            "admin_district" = sampling_frame$admin_district[i],
            "health_district" = sampling_frame$health_district[i],
            "birth_history" = birth_history
          )
          
        }
      }
    }
  }
  
  # Return population dataset
  return(population)
  
}
