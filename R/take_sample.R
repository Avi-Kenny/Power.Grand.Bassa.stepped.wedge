# FN: Take a sample
# - Inputs
#     - `population`: a population dataset list returned by create_dataset()
#     - `sampling_frame`: GrandBassa2020SamplingFrame_truncated.xlsx
# - Output
#     - A smaller dataset representing a cluster sample of the entire population
# - Description
#     - Simulates the Grand Bassa impact evaluation sampling mechanism
#     - First state is a simple random sample of communities (not PPS)
#     - Second state is a census of all women in all households

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
