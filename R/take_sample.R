#' Take a complex sample from the Grand Bassa population
#'     - Simulates the Grand Bassa impact evaluation sampling mechanism
#'     - First stage is a simple random sample of communities (not PPS)
#'     - Second stage is a census of all women in all households
#'     - !!!!! If we want to test PPS, be sure to sample households from
#'       sampling frame rather than population dataset, since some households
#'       have zero women
#'
#' @param population A population dataset object returned by create_dataset()
#' @param sampling_frame GrandBassa2020SamplingFrame_truncated.xlsx
#' @param n_clusters Number of clusters to sample
#' @return A truncated data frame representing a cluster sample of the entire
#'     population

take_sample <- function(population, sampling_frame, n_clusters) {
  
  # Sample communities
  sampled_communities <- sample(sampling_frame$community_id, size=n_clusters)
  
  # Create truncated objects
  women_sample <- population$women %>% filter(
    community_id %in% sampled_communities
  )
  birth_history_sample <- population$birth_history %>% filter(
    woman_id %in% women_sample$woman_id
  )
  
  return(list(
    "women" = women_sample,
    "birth_history" = birth_history_sample
  ))
  
}
