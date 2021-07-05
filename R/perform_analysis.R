#' Perform statistical analysis
#'
#' @param dataset A dataset returned by transform_dataset()
#' @param method Type of analysis; one of c("Mixed model (immediate Tx effect)",
#'     "Mixed model (time-varying Tx effect)", "Callaway-Sant'Anna")
#' @return A list containing the following:
#'   - `tx_effect`: Estimated treatment effect
#'   - `p`: P-value corresponding to a Wald-type hypothesis test
#'   - `reject_h0`: Binary variable that equals 1 if H_0 was rejected

perform_analysis <- function(dataset, method) {
  
  # model <- glmer(
  model <- glmmTMB(
    cbind(n_deaths,n_alive-n_deaths) ~ x_ij + factor(month) + (1|community_id),
    data = dataset,
    family = "binomial" # binomial(link="log")
  )
  summary(model)
  
  # Calculate results
  s <- summary(model)
  tx_effect <- exp(s$coefficients$cond["x_ij","Estimate"])
  p <- s$coefficients$cond["x_ij","Pr(>|z|)"]
  reject_h0 <- as.integer(tx_effect<1 & p<0.05)
  
  if (tx_effect>1 & p<0.05) {
    warning("Treatment had 'negative effect'")
  }
  
  # Return results
  return(list(tx_effect=tx_effect, p=p, reject_h0=reject_h0))
  
}
