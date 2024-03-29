#' Take a complex sample from the Grand Bassa population
#'     - Simulates the Grand Bassa impact evaluation sampling mechanism
#'     - First stage is a simple random sample of communities (not PPS)
#'     - Second stage is a census of all women in all households
#'
#' @param sampling_frame GrandBassa2020SamplingFrame_truncated.xlsx
#' @param n_clusters Number of clusters to sample in first stage
#' @param type Placeholder, in case we want to add other sampling methods
#' @return A truncated dataset representing a cluster sample of the entire
#'     population

take_sample <- function(sampling_frame, n_clusters, type="SRS") {
  
  if (type=="SRS") {
    
    # Sample communities
    sampled_comms <- sample(sampling_frame$community_id, size=n_clusters)
    
    # Filter sampling frame
    sample <- sampling_frame %>% filter(community_id %in% sampled_comms)
    
    # Modification to merge smaller communities into larger ones
    pop <- 0
    new_comm_ids <- c()
    new_id <- 1
    for (i in 1:nrow(sample)) {
      new_comm_ids <- c(new_comm_ids, new_id)
      pop <- pop + sample[i,"num_hh"]
      if (pop>=100) {
        pop <- 0
        new_id <- new_id + 1
      }
    }
    sample$community_id2 <- new_comm_ids
    
  }
  
  return(sample)
  
}
