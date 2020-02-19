# FN: Sample from a birth history distribution
# - Inputs
#     - `woman_age`: the woman's age
# - Output
#     - An entire birth history sampled from a distribution
# - Description
#     - Note: this function is not deterministic; use set.seed() if exact reproduction of results is necessary

birth_history <- function(woman_age) {
  
  # CMC helper functions
  # !!!!! Consider moving these elsewhere
  dates_to_cmc <- function(year, month) {
    12*(year-1900)+month
  }
  cmc_to_dates <- function(cmc) {
    month <- ifelse(mod(cmc, 12)!=0, mod(cmc, 12), 12)
    return(list(
      year = 1900 + (cmc-month)/12,
      month = month
    ))
  }
  
  # Specify baseline mortality curve
  # Currently a linear decrease from start_year to end_year
  # Year must be between
  # !!!!! Need to change these numbers later to be more realistic
  # !!!!! Possibly move this function elsewhere or pass in arguments through levels_row
  baseline_mortality <- function(year, start_year=2000, end_year=2023, start_u5mr=130, end_u5mr=80) {
    
    slope <- (end_u5mr-start_u5mr)/(end_year-start_year)
    intercept <- start_u5mr - (slope*start_year)
    return ( slope*year + intercept )
    
  }
  
  # Loop through woman-years to generate births
  # !!!!! Need more accurate birth probabilities as a function of woman's age and current year
  birthdates_cmc <- c()
  for (i in 13:(woman_age-1)) {
    
    current_year <- 2023 - woman_age + i
    birth_prob <- 1/6 # !!!!! Placeholder
    
    if (runif(1)<birth_prob) {
      birthdate <- dates_to_cmc(year=current_year, month=sample(1:12,1))
      birthdates_cmc <- c(birthdates_cmc, birthdate)
    }
    
  }
  
  # Loop through births to generate deaths
  # death_prob will ignore over-five deaths
  # !!!!! Need more accurate death probabilities as a function of child's age and current year
  # !!!!! Death probability in time period 2022-2023 should also account for birth month
  # !!!!! Assume for simplicity that entire survey is done in Jan 2023
  alive <- c()
  deathdates_cmc <- c()
  if (length(birthdates_cmc) >= 1) {
    
    for (i in 1:length(birthdates_cmc)) {
      
      alive[i] <- 1
      deathdates_cmc[i] <- NA
      birthdate_cmc <- birthdates_cmc[i]
      child_birth_year <- cmc_to_dates(birthdate_cmc)$year
      
      for (j in 1:5) {
        
        if (alive[i] == 1 & (child_birth_year + j)<=2023) {
          
          current_year <- child_birth_year + j
          death_prob <- baseline_mortality(current_year)/(5*1000) # !!!!! Placeholder
          
          if (runif(1)<death_prob) {
            alive[i] <- 0
            deathdate <- dates_to_cmc(year=current_year, month=sample(1:12,1))
            deathdates_cmc[i] <- deathdate
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
