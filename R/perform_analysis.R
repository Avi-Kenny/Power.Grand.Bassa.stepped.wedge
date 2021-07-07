#' Perform statistical analysis
#'
#' @param dataset_trans A dataset returned by transform_dataset()
#' @param method Type of analysis; one of c("Mixed model (immediate Tx effect)",
#'     "Mixed model (time-varying Tx effect)", "Callaway-Sant'Anna")
#' @return A list containing the following:
#'   - `tx_effect`: Estimated treatment effect
#'   - `p`: P-value corresponding to a Wald-type hypothesis test
#'   - `reject_h0`: Binary variable that equals 1 if H_0 was rejected

perform_analysis <- function(dataset_trans, method) {
  
  if (method=="Mixed model (immediate Tx effect)") {
    
    # Fit mixed model
    # Currently treating time trend as linear
    model <- glmmTMB(
      cbind(n_deaths,n_alive-n_deaths) ~ x_ij + month +
        (1|community_id),
      data = dataset_trans,
      family = "binomial" # binomial(link="log")
    )
    
    # Calculate results
    s <- summary(model)
    tx_effect <- exp(s$coefficients$cond["x_ij","Estimate"])
    p <- s$coefficients$cond["x_ij","Pr(>|z|)"]
    reject_h0 <- as.integer(tx_effect<1 & p<0.05)
    
    if (tx_effect>1 & p<0.05) warning("Treatment had 'opposite effect'")
    
  }
  
  if (method=="Mixed model (time-varying Tx effect)") {
    
    # Fit mixed model
    # Currently treating time trend as linear
    model <- glmmTMB(
      cbind(n_deaths,n_alive-n_deaths) ~ factor(x_it) + month +
        (1|community_id),
      data = dataset_trans,
      family = "binomial" # binomial(link="log")
    )
    
    # Calculate results
    s <- summary(model)
    coeff_names <- names(s$coefficients$cond[,1])
    theta_l_hat <- as.numeric(s$coefficients$cond[,1])
    sigma_l_hat <- vcov(model)$cond
    indices <- c(1:length(coeff_names))
    indices <- indices[str_sub(coeff_names,1,12)=="factor(x_it)"]
    coeff_names <- coeff_names[indices]
    theta_l_hat <- theta_l_hat[indices]
    sigma_l_hat <- sigma_l_hat[indices,indices]
    sigma_l_hat <- as.matrix(sigma_l_hat)
    len <- length(coeff_names)
    A <- (1/len) * matrix(rep(1,len), nrow=1)
    log_tx_effect <- (A %*% theta_l_hat)[1,1]
    tx_effect <- exp(log_tx_effect)
    se_log_tx_effect <- sqrt(A %*% sigma_l_hat %*% t(A))[1,1]
    p <- pchisq((log_tx_effect^2)/(se_log_tx_effect^2),1, lower.tail=F)
    reject_h0 <- as.integer(tx_effect<1 & p<0.05)
    
    if (tx_effect>1 & p<0.05) warning("Treatment had 'opposite effect'")

  }
  
  if (method=="Mixed model (time-varying and random Tx effect)") {
    
    # Fit mixed model
    # Currently treating time trend as linear
    model <- glmmTMB(
      cbind(n_deaths,n_alive-n_deaths) ~ factor(x_it) + month +
        (x_ij|community_id),
      data = dataset_trans,
      family = "binomial" # binomial(link="log")
    )
    
    # Calculate results
    s <- summary(model)
    coeff_names <- names(s$coefficients$cond[,1])
    theta_l_hat <- as.numeric(s$coefficients$cond[,1])
    sigma_l_hat <- vcov(model)$cond
    indices <- c(1:length(coeff_names))
    indices <- indices[str_sub(coeff_names,1,12)=="factor(x_it)"]
    coeff_names <- coeff_names[indices]
    theta_l_hat <- theta_l_hat[indices]
    sigma_l_hat <- sigma_l_hat[indices,indices]
    sigma_l_hat <- as.matrix(sigma_l_hat)
    len <- length(coeff_names)
    A <- (1/len) * matrix(rep(1,len), nrow=1)
    log_tx_effect <- (A %*% theta_l_hat)[1,1]
    tx_effect <- exp(log_tx_effect)
    se_log_tx_effect <- sqrt(A %*% sigma_l_hat %*% t(A))[1,1]
    p <- pchisq((log_tx_effect^2)/(se_log_tx_effect^2),1, lower.tail=F)
    reject_h0 <- as.integer(tx_effect<1 & p<0.05)
    
    if (tx_effect>1 & p<0.05) warning("Treatment had 'opposite effect'")
    
  }
  
  if (method=="Callaway-Sant'Anna") {
    
    # Transformations specific to CS method
    # This is somewhat problematic because it doesn't account for births that
    #     happen in the 6-month interval (i.e. it is not a proper survival
    #     model)
    
    # Cut off data when last group crosses over
    dataset_trans %<>% filter(month<=53)
    
    # Round crossover dates to nearest 6 month; adds some minor error but only
    #     affects one group slightly
    dataset_trans %<>% mutate(
      crossover_date = 6*round(crossover_date/6),
      crossover_date = ifelse(crossover_date==54, 0, crossover_date)
    )
    
    dataset_trans %<>% mutate(
      time_period = ceiling(month/6),
      crossover_period = crossover_date/6
    )
    
    dataset_trans %<>% group_by(crossover_period,time_period,community_id) %>%
      dplyr::summarize(
        n_alive = n_alive[1],
        n_deaths = sum(n_deaths)
      )
    
    dataset_trans %<>% mutate(
      mort_rate = n_deaths/n_alive
    )
    
    dataset_trans %<>% filter(!is.na(mort_rate))
    
    out <- att_gt(
      yname = "mort_rate",
      tname = "time_period",
      idname = "community_id",
      gname = "crossover_period",
      xformla = ~1,
      data = as.data.frame(dataset_trans),
      est_method = "reg"
    )
    
    es <- aggte(out, type = "dynamic")
    
    # Calculate results
    tx_effect <- es$overall.att
    se_tx_effect <- es$overall.se
    p <- pchisq((tx_effect^2)/(se_tx_effect^2),1, lower.tail=F)
    reject_h0 <- as.integer(tx_effect<0 & p<0.05)
    
    if (tx_effect>0 & p<0.05) warning("Treatment had 'opposite effect'")
    
  }
  
  # Return results
  return(list(tx_effect=tx_effect, p=p, reject_h0=reject_h0))
  
}
