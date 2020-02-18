# FN: Sample from a birth history distribution
# - Inputs
#     - `mother_age`: the mother's age
# - Output
#     - An entire birth history sampled from a distribution
# - Description
#     - Note: this function is not deterministic; use set.seed() if exact reproduction of results is necessary

birth_history <- function(mother_age) {
  
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
  
  baseline_mortality(year=2010)
  
  # ...
  
}
