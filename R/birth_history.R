#' Generate one stepped wedge dataset
#'     Note: this function is not deterministic; use set.seed() if exact
#'     reproduction of results is necessary
#'
#' @param woman_age The woman's age (in years)
#' @param program_effect The program effect, in terms of percent reduction in
#'     probability of child death
#' @param crossover_date The first month (CMC) at which the program was in the
#'     intervention state
#' @return A list representing the entire birth history for a single woman,
#'     sampled from a distribution, containing the following: \cr
#'     * `birthdates_cmc`: birth dates of all children (in CMC)
#'     * `alive`: binary vector representing whether children are still alive
#'     * `deathdates_cmc`: death dates of all children (in CMC)

birth_history <- function(woman_age, program_effect, crossover_date) {
  
  # Loop through woman-years to generate births
  # i is the "current age" of the woman
  # !!!!! Consider making this loop start at 13
  birthdates_cmc <- c()
  for (i in 14:(woman_age-1)) {
    
    current_year <- 2023 + i - woman_age

    # Generate birth probabilities from Distribution C
    # !!!!! Probabilities currently seem too high
    # !!!!! Add a random effect here ?????
    # !!!!! Check values for ages 13-14
    birth_prob <- case_when(
      i >= 14 & i <= 19 ~ 0.309,
      i >= 20 & i <= 24 ~ 0.318,
      i >= 25 & i <= 29 ~ 0.292,
      i >= 30 & i <= 34 ~ 0.293,
      i >= 35 & i <= 39 ~ 0.260,
      i >= 40 & i <= 44 ~ 0.269,
      i >= 45 & i <= 49 ~ 0.170
    )
    
    if (runif(1)<birth_prob) {
      birthdate <- dates_to_cmc(year=current_year, month=sample(1:12,1))
      birthdates_cmc <- c(birthdates_cmc, birthdate)
    }
    
  }
  
  if (is.null(birthdates_cmc)) {
    birthdates_cmc <- NA
    alive <- NA
    deathdates_cmc <- NA
  } else {
    alive <- rep(1, length(birthdates_cmc))
    deathdates_cmc <- rep(NA, length(birthdates_cmc))
  }
  
  if (!is.na(birthdates_cmc[1])) {
    
    # Loop through births to generate deaths
    # Variable death_prob will ignore over-five deaths
    # Assume for simplicity that entire survey is done in Jan 2023
    for (i in 1:length(birthdates_cmc)) {
      
      # Loop over months
      j_start <- birthdates_cmc[i]+1
      j_end <- min(dates_to_cmc(2023,1), j_start+59)
      for (j in j_start:j_end) {
        
        if (alive[i] == 1) {
          
          # `ca`` is child's age last month
          ca <- j - birthdates_cmc[i] - 1
          
          # Calculate "current" program effect
          # !!!!! Currently modeling program effect onset as an "off/on switch"
          # !!!!! Need to account for crossover month
          if (j > crossover_date) {
            program_effect_now = program_effect
          } else {
            program_effect_now = 0
          }
          
          # # Calculate death probability
          # #     We divide the U5MR by 5,000 since U5MR is per 1,000 and is over
          # #     a five year period; this is a rough approximation
          # death_prob_year <- (1-program_effect_now) * ( baseline_u5mr(
          #   cmc_to_dates(j-1)$year,
          #   start_year = 2000, # !!!!! Placeholder
          #   end_year = 2023, # !!!!! Placeholder
          #   start_u5mr = 120, # !!!!! Placeholder
          #   end_u5mr = 80 # !!!!! Placeholder
          # ) / 5000 )
          
          # Calculate yearly death probabilities and convert to monthly probs
          # !!!!! Currently not accounting for linear time trend (see
          #     commented-out code above)
          # Leverages Distribution E to obtain the relative probabilities
          # !!!!! Smooth Distribution E
          # !!!!! Check that death_prob_month for child_age_last_month==0
          #     roughly equals the neonatal mortality rate
          if (ca<=11) {
            
            death_prob_year <- 0.0733
            
            relative_prob <- case_when(
              ca == 0 ~ 0.264,
              ca == 1 ~ 0.184,
              ca == 2 ~ 0.162,
              ca == 3 ~ 0.106,
              ca == 4 ~ 0.05,
              ca == 5 ~ 0.039,
              ca == 6 ~ 0.073,
              ca == 7 ~ 0.011,
              ca == 8 ~ 0.061,
              ca == 9 ~ 0.022,
              ca == 10 ~ 0.022,
              ca == 11 ~ 0.006
            )
            
            death_prob_month <- death_prob_year*relative_prob
            
          } else {
            
            death_prob_year <- case_when(
              ca >= 12 & ca <= 23 ~ 0.0160,
              ca >= 24 & ca <= 35 ~ 0.0114,
              ca >= 36 & ca <= 47 ~ 0.0034,
              ca >= 48 & ca <= 59 ~ 0.0038
            )
            
            death_prob_month <- 1-((1-death_prob_year)^(1/12))
            
          }
          
          # Multiply the death probability to account for the program effect
          death_prob_month <- (1-program_effect_now) * death_prob_month
          
          # !!!!! DEBUGGING
          if (is.na(death_prob_month)) {
            print("is.na(death_prob_month) == TRUE")
            print(ca)
            print(i)
            print(j)
            print(birthdates_cmc[i])
            print(j_start)
            print(j_end)
            print(dates_to_cmc(2023,1))
            print(j_start+59)
          }
          
          # Sample to determine whether child died in the past month
          if (runif(1)<death_prob_month) {
            alive[i] <- 0
            deathdates_cmc[i] <- j-1
          }
          
        }
      }
    }
  }
  
  return(list(
    birthdates_cmc = birthdates_cmc,
    alive = alive,
    deathdates_cmc = deathdates_cmc
  ))
  
}
