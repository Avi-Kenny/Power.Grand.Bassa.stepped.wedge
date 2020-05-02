#' Take a complex sample from the Grand Bassa population
#'     - Simulates the Grand Bassa impact evaluation sampling mechanism
#'     - First stage is a simple random sample of communities (not PPS)
#'     - Second stage is a census of all women in all households
#'
#' @param population A population dataset object returned by create_dataset()
#' @param sampling_frame GrandBassa2020SamplingFrame_truncated.xlsx
#' @param n_clusters Number of clusters to sample
#' @return A truncated data frame representing a cluster sample of the entire
#'     population

take_sample <- function(population, sampling_frame, n_clusters) {
  
  # Sample communities
  sampled_communities <- sample(sampling_frame$community_id, size=n_clusters)
  
  # Filter list
  sample <- list.filter(
    population,
    community_id %in% sampled_communities
  )
  
  return(sample)
  
}
