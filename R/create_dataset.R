#' Generate one simulated dataset representing the Grand Bassa population
#'
#' @param sampling_frame GrandBassa2020SamplingFrame_truncated.xlsx
#' @param program_effect Percent reduction in U5MR in intervention group
#' @param show_progress True/false; should progress statement ("5 of 999
#'     communities generated") be printed?
#' @return A list containing the women (a dataframe) and birth histories (a
#'     list)

create_dataset <- function(sampling_frame, program_effect,
                           show_progress=FALSE) {
  
  women <- data.frame(
    "woman_id" = integer(),
    "community_id" = integer(),
    "household_id" = integer(),
    "admin_district" = integer(),
    "health_district" = integer(),
    "crossover_date" = integer()
  )
  birth_history <- data.frame(
    "woman_id" = integer(),
    "child_id" = integer(),
    "birthdates" = integer(),
    "deathdates" = integer(),
    "alive" = integer()
  )
  
  woman_id <- 1
  
  # Sample random effects
  re_comm <- rnorm(n=length(sampling_frame$community_id), mean=0, sd=0.05)
  re_tx <- rnorm(n=length(sampling_frame$community_id), mean=0, sd=0.05)
  
  # Generate population
  for (i in 1:length(sampling_frame$community_id)) {
    
    for (j in 1:sampling_frame$num_hh[i]) {
      
      # Generate number of women (age 15-49) in household
      # Corresponds to "Distribution A"
      num_women = sample(
        x = c(0:6),
        size = 1,
        replace = TRUE,
        prob = c(0.227, 0.623, 0.114, 0.029, 0.004, 0.002, 0.001)
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
          crossover_date <- case_when(
            sampling_frame$health_district[i] == "1" ~ dates_to_cmc(2019,11),
            sampling_frame$health_district[i] == "2" ~ dates_to_cmc(2018,12),
            sampling_frame$health_district[i] == "3A&B" ~ dates_to_cmc(2018,12),
            sampling_frame$health_district[i] == "3C" ~ dates_to_cmc(2018,6),
            sampling_frame$health_district[i] == "4" ~ dates_to_cmc(2021,6), # !!!!! Tentative
            sampling_frame$health_district[i] == "Campwood" ~ dates_to_cmc(2018,6),
            sampling_frame$health_district[i] == "Commonwealth" ~ dates_to_cmc(2021,6), # !!!!! Tentative
            sampling_frame$health_district[i] == "Owensgrove" ~ dates_to_cmc(2019,11)
          )
          
          # Generate birth history
          # !!!!! Need to account for ICC somewhere here
          bh <- create_birth_history(
            "woman_age" = woman_age,
            "program_effect" = program_effect,
            "cluster_effect" = cluster_effect,
            "crossover_date" = crossover_date
          )
          
          # Create women dataframe
          women[nrow(women)+1,] <- list(
            "woman_id" = woman_id,
            "community_id" = sampling_frame$community_id[i],
            "household_id" = j,
            "admin_district" = sampling_frame$admin_district[i],
            "health_district" = sampling_frame$health_district[i],
            "crossover_date" = crossover_date
          )
          
          # Create birth history dataframe
          if (!is.na(bh$birthdates_cmc[1])) {
            birth_history <- rbind(
              birth_history,
              data.frame(
                "woman_id" = rep(woman_id, length(bh$alive)),
                "child_id" = c(1:length(bh$alive)),
                "birthdate" = bh$birthdates_cmc,
                "deathdate" = bh$deathdates_cmc,
                "alive" = bh$alive
              )
            )
          }
          
          woman_id <- woman_id + 1
          
        }
      }
    }
    
    if (show_progress) {
      cat(paste(
        i, "of", length(sampling_frame$community_id), "communities generated\n"
      ))
    }
    
  }
  
  # Return datasets
  return(list(
    "women" = women,
    "birth_history" = birth_history
  ))
  
}
