#' Transform dataset to a format suitable for analysis
#'
#' @param dataset A dataset returned by create_dataset()
#' @param recall_years Number of recall years to use
#' @param grouping Either 1 (use granular community IDs) or 2 (use grouped
#'     community IDs)
#' @return A dataset formatted such that one row represents one community-month

transform_dataset <- function(dataset, recall_years, grouping) {
  
  # Create merged dataset; one row per child
  d <- inner_join(
    dataset$women,
    dataset$birth_history,
    by = "woman_id"
  )
  d %<>% subset(select=-c(
    child_id, woman_id, household_id, admin_district, health_district
  ))
  
  if (grouping==1) {
    d %<>% subset(select=-c(community_id2))
  } else if (grouping==2) {
    d %<>% subset(select=-c(community_id))
    d %<>% dplyr::rename("community_id"=community_id2)
  }
  
  # Number of U5 deaths, by community/month
  # The condition "deathdate-birthdate<60" is technically not necessary because
  #     no under-five deaths are generated in the fake dataset
  df_y <- d %>%
    filter(alive==0 & deathdate-birthdate<60) %>%
    rename("month" = deathdate) %>%
    subset(select=-c(crossover_date, birthdate, alive)) %>%
    group_by(community_id, month) %>%
    dplyr::summarize(n_deaths = n())
  
  # Number of U5 children alive at month start, by community/month
  # !!!!! It appears that `alive` decreases with time; is this expected?
  survey_date <- dates_to_cmc(2022,1)
  month_start <- survey_date-(recall_years*12)
  month_end <- survey_date - 1
  months <- month_start:month_end
  df_n <- d %>% subset(select=community_id)
  for (month in months) {
    
    col <- as.integer(d$alive==1 & month-d$birthdate<60) +
      as.integer(d$alive==0 & month-d$birthdate<60 & month<=d$deathdate)
    df_n <- cbind(df_n,col)
    names(df_n)[length(names(df_n))] <- paste0("mth_",month)
    
  }
  df_n %<>% group_by(community_id) %>%
    summarize_all(sum) %>%
    pivot_longer(
      cols = -community_id,
      names_to = "month",
      names_prefix = "mth_",
      values_to = "n_alive"
    ) %>%
    mutate(month=as.integer(month))
  
  # Program status
  df_crossover <- d %>% group_by(community_id) %>% dplyr::summarize(
    crossover_date = crossover_date[1]
  )
  
  # Create analysis dataframe
  df_joined <- expand.grid(
    "community_id" = unique(df_y$community_id),
    "month" = months
  )
  
  # Generate x_ij "treatment status" variable
  # x_it is aggregated into 6-month periods
  agg_level <- 6
  df_joined %<>% inner_join(df_crossover, by="community_id")
  df_joined %<>% mutate(
    x_ij = as.integer(month>=crossover_date),
    x_it = as.integer(ceiling(pmax(0,month-crossover_date)/agg_level))
  ) %>%
    arrange(community_id, month)
  
  # Join with other component dataframes
  df_joined %<>% left_join(df_y, by=c("community_id", "month"))
  df_joined %<>% left_join(df_n, by=c("community_id", "month"))
  df_joined %<>% filter(!is.na(n_alive))
  df_joined$n_deaths %<>% replace_na(0)
  
  # Rescale time to start at 1
  min_month <- min(df_joined$month)
  df_joined %<>% mutate(
    month = (month-min_month)+1,
    crossover_date = (crossover_date-min_month)+1
  )
  
  return(df_joined)
  
}
