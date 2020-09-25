# Sample Size - Grand Bassa Stepped Wedge Impact Evaluation
# Author: Avi Kenny



#########################.
##### Load packages #####
#########################.

# devtools::install_github("Avi-Kenny/simba")
library(simba)
library(parallel)
library(ggplot2)
library(readxl)
library(magrittr)
library(dplyr)
library(tibble)
library(survival)
library(coxme)
library(tidyr)



############################.
##### Simulation setup #####
############################.

# Set working directory (if working locally, just set to source file location)
if (Sys.getenv("USERDOMAIN")=="AVI-KENNY-T460") {
  setwd("C:/Users/avike/OneDrive/Desktop/Avi/Biostats + Research/Research/Jim Hughes/Project - Stepped wedge/z.stepped.wedge/R")
} else {
  setwd("z.Power-Grand-Bassa-stepped-wedge/R")
}

# Load functions
{
  source("helpers.R")
  source("create_dataset.R")
  source("create_birth_history.R")
  source("perform_analysis.R")
  source("take_sample.R")
}

# Read and transform sampling frame
sampling_frame <- read_excel("../data/GrandBassa2020SamplingFrame_truncated.xlsx")
sampling_frame %<>% rowid_to_column("community_id")
sampling_frame %<>% rename(
  "num_hh" = `numHH`,
  "health_district" = `map_health_district`,
  "admin_district" = `map_adm_distri`
)

# # Generate population (one for entire simulation)
# new_dataset <- FALSE
# if (new_dataset) {
#   dataset <- create_dataset(
#     "sampling_frame" = sampling_frame,
#     "program_effect" = 0.25,
#     "re_comm_sd" = 0.1,
#     "re_tx_sd" = 0.1
#   )
#   saveRDS(dataset, file="gb_dataset")
# } else {
#   dataset <- readRDS("gb_dataset")
# }

sim <- new_sim()

sim %<>% set_config(
  num_sim = 1,
  datasets = "many",
  parallel = "outer",
  packages = c("magrittr", "dplyr", "tibble", "survival", "coxme", "tidyr")
)

sim %<>% set_levels(
  sample_size = c(20, 50) # !!!!! 600, 700
)

sim %<>% add_constants(
  sampling_frame = sampling_frame
)

# Add necessary functions
{
  sim %<>% add_creator(create_dataset)
  sim %<>% add_method(baseline_u5mr)
  sim %<>% add_method(dates_to_cmc)
  sim %<>% add_method(cmc_to_dates)
  sim %<>% add_method(create_birth_history)
  sim %<>% add_method(take_sample)
  sim %<>% add_method(perform_analysis)
}

sim %<>% add_script(
  "script_1",
  function() {
    
    # Take a sample from population
    sample <- take_sample(
      sampling_frame = C$sampling_frame,
      n_clusters = L$sample_size
    )
    
    # Generate data for sample
    dataset <- create_dataset(
      sample = sample,
      program_effect = 0.25,
      re_comm_sd = 0.1,
      re_tx_sd = 0.1,
      show_progress = FALSE
    )
    
    # Perform statistical analysis
    results <- perform_analysis(
      dataset = dataset,
      method = 1,
      recall_years = 5
    )
    
    return (results)
    
  }
)



################################################.
##### Run simulation and summarize results #####
################################################.

# Run simulation
sim %<>% run("script_1")

# Summarize results
sim %>% summary() %>%
  mutate(
    mean_est_pct_reduction_1 = 1-exp(mean_tx_effect_1),
    mean_est_pct_reduction_2 = 1-exp(mean_tx_effect_1),
    power_1 = mean_reject_h0_1,
    power_2 = mean_reject_h0_2
  ) %>% print()
