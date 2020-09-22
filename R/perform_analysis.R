#' Perform statistical analysis
#'
#' @param results !!!!! TO DO
#' @param type !!!!! TO DO
#' @return A list containing the following: \cr
#'     * `params`: !!!!! TO DO \cr
#'     * `data`: !!!!! TO DO

perform_analysis <- function(sample, type) {
  
  # !!!!! Pass these in as arguments
  survey_date <- dates_to_cmc(2023,1)
  recall_yrs <- 5
  
  # Create merged dataset; one row per child
  df_joined <- inner_join(
    sample$women,
    sample$birth_history,
    by = "woman_id"
  )
  
  # Data wrangling
  df_joined$deathdate <- tidyr::replace_na(df_joined$deathdate, 9999)
  df_joined %<>% mutate(
    time_start = pmax(birthdate, survey_date-(recall_yrs*12)),
    time_end = pmin(deathdate, birthdate+60, survey_date),
    died = 1 - alive
  )
  df_joined %<>% filter(time_start<time_end)
  
  # Transform dataset to account for time-varying treatment indicator
  df_joined <- survSplit( 
    Surv(time_start, time_end, died) ~ .,
    data = df_joined,
    cut = unique(df_joined$crossover_date),
  )
  df_joined %<>% mutate(
    tx_status = ifelse(time_start>=crossover_date, 1, 0),
    uid = (100*woman_id) + child_id,
    age_start = time_start - birthdate,
    age_end = time_end - birthdate
  )
  df_joined %<>% subset( select=-c(
    household_id, admin_district, health_district, alive
  ))
  
  # !!!!! Add random effect using coxme
  # https://stats.idre.ucla.edu/r/dae/mixed-effects-cox-regression/#:~:text=Mixed%20effects%20cox%20regression%20models,both%20fixed%20and%20random%20effects.
  cox_1 <- coxph(
    Surv(age_start, age_end, died) ~ tx_status,
    # id = uid, # !!!!! Testing
    data = df_joined
  )
  summ_1 <- summary(cox_1)
  p_1 <- summ_1$coefficients[1,5]
  
  # Model with random intercept for community
  cox_2 <- coxme(
    Surv(age_start, age_end, died) ~ tx_status + (1|community_id),
    data = df_joined
  )
  summ_2 <- summary(cox_2)
  nfrail <- nrow(cox_2$var) - length(cox_2$coefficients)
  se <- sqrt(diag(cox_2$var)[nfrail + 1:length(cox_2$coefficients)])
  p_2 <- signif(1 - pchisq((cox_2$coefficients/se)^2, 1), 2)
  
  # # Model with random intercept for community and random treatment effect
  # cox_3 <- coxme(
  #   Surv(age_start, age_end, died) ~ tx_status + (0+tx_status|community_id) +
  #                                    (1|community_id),
  #   data = df_joined
  # )
  # summ_3 <- summary(cox_3)
  
  # Return results
  return(list(
    tx_effect_1 = summ_1$coefficients[1,1],
    tx_effect_2 = cox_2$coefficients[[1]],
    p_1 = p_1,
    p_2 = p_2,
    reject_h0_1 = ifelse(
      summ_1$coefficients[1,1]<0 & p_1<0.05,
      1, 0
    ),
    reject_h0_2 = ifelse(
      cox_2$coefficients[[1]]<0 & p_2<0.05,
      1, 0
    )
  ))
  
}
