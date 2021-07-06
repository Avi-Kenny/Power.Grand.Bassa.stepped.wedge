#' Generate one simulated dataset representing the Grand Bassa population
#'
#' @param sample A sample, returned by take_sample()
#' @param program_effect Percent reduction in U5MR in intervention group
#' @param re_comm_sd Standard deviation of the community-level random effect
#' @param re_tx_sd Standard deviation of the treatment-level random effect
#' @param tvte Boolean. Should data be generated with a time-varying treatment
#'     effet?
#' @param show_progress True/false; should progress statement ("5 of 999
#'     communities generated") be printed?
#' @return A list containing the women (a dataframe) and birth histories (a
#'     list)

create_dataset <- function(sample, program_effect, re_comm_sd, re_tx_sd,
                           tvte, show_progress=FALSE) {
  
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
  n_clusters <- length(sample$community_id)
  
  # Sample random effects
  # The pmax() ensures that probabilities are not multiplied by a negative
  #     number (although this is unlikely for reasonable SDs)
  re_comm <- rnorm(n=n_clusters, mean=1, sd=re_comm_sd)
  re_tx <- rnorm(n=n_clusters, mean=1, sd=re_tx_sd)
  re_comm <- pmax(re_comm,0)
  re_tx <- pmax(re_tx,0)
  
  # Generate dataset
  for (i in 1:n_clusters) {
    
    for (j in 1:sample$num_hh[i]) {
      
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
            sample$health_district[i] == "1" ~ dates_to_cmc(2019,11),
            sample$health_district[i] == "2" ~ dates_to_cmc(2018,12),
            sample$health_district[i] == "3A&B" ~ dates_to_cmc(2018,12),
            sample$health_district[i] == "3C" ~ dates_to_cmc(2018,6),
            sample$health_district[i] == "4" ~ dates_to_cmc(2021,6), # !!!!! Tentative
            sample$health_district[i] == "Campwood" ~ dates_to_cmc(2018,6),
            sample$health_district[i] == "Commonwealth" ~ dates_to_cmc(2021,6), # !!!!! Tentative
            sample$health_district[i] == "Owensgrove" ~ dates_to_cmc(2019,11)
          )
          
          # Generate birth history
          bh <- create_birth_history(
            "woman_age" = woman_age,
            "program_effect" = program_effect,
            "re_comm" = re_comm[i],
            "re_tx" = re_tx[i],
            "tvte" = tvte,
            "crossover_date" = crossover_date
          )
          
          # Create women dataframe
          women[nrow(women)+1,] <- list(
            "woman_id" = woman_id,
            "community_id" = sample$community_id[i],
            "household_id" = j,
            "admin_district" = sample$admin_district[i],
            "health_district" = sample$health_district[i],
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
        i, "of", n_clusters, "communities generated\n"
      ))
    }
    
  }
  
  # Return datasets
  return(list(
    "women" = women,
    "birth_history" = birth_history
  ))
  
}
