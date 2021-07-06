# Title: Sample Size - Grand Bassa Stepped Wedge Impact Evaluation
# Author: Avi Kenny, Marco Carone

##################.
##### CONFIG #####
##################.

# Set global config
cfg <- list(
  which_sim = "",
  level_set_which = "level_set_1",
  run_or_update = "run",
  num_sim = 40, # !!!!!
  pkgs = c("dplyr", "readxl", "tibble", "survival", "tidyr", "lme4", "glmmTMB",
           "stringr", "did"),
  pkgs_nocluster = c("ggplot2"),
  parallel = "none",
  stop_at_error = FALSE
)

# Set cluster config
cluster_config <- list(
  js = "slurm",
  dir = "/home/akenny/z.Power-Grand-Bassa-stepped-wedge"
)



#################.
##### SETUP #####
#################.

# Set local vs. cluster variables
if (Sys.getenv("USERDOMAIN")=="AVI-KENNY-T460") {
  # Local
  setwd(paste0("C:/Users/avike/OneDrive/Desktop/Avi/Last Mile Health/Research ",
               "+ Evaluation/Projects (active)/Grand Bassa impact evaluation/P",
               "ower-Grand-Bassa-stepped-wedge/R"))
  load_pkgs_local <- TRUE
} else {
  # Cluster
  setwd("z.Power-Grand-Bassa-stepped-wedge/R")
  load_pkgs_local <- FALSE
}

# Load packages (if running locally)
if (load_pkgs_local) {
  for (pkg in c(cfg$pkgs,cfg$pkgs_nocluster)) {
    do.call("library", list(pkg))
  }
}

# Load simba + functions
# devtools::install_github("Avi-Kenny/simba")
{
  library(simba)
  source("one_simulation.R")
  source("create_dataset.R")
  source("create_birth_history.R")
  source("take_sample.R")
  source("transform_dataset.R")
  source("perform_analysis.R")
  source("helpers.R")
}



##########################################################.
##### MAIN: Set level sets for different simulations #####
##########################################################.

if (Sys.getenv("simba_run") %in% c("first", "")) {
  
  # Compare all methods
  level_set_1 <- list(
    sample_size = 1000,
    # sample_size = c(1000, 1733),
    program_effect = c(0, 0.2),
    tvte = c(TRUE, FALSE),
    method = c("Mixed model (immediate Tx effect)",
               "Mixed model (time-varying Tx effect)",
               "Callaway-Sant'Anna")
  )
  
  level_set <- eval(as.name(cfg$level_set_which))
  
}



##########################################.
##### MAIN: Setup and run simulation #####
##########################################.

# Use these commands to run on Slurm:
# sbatch --export=simba_run='first',cluster='bionic',type='R',project='z.Power-Grand-Bassa-stepped-wedge' -e ./io/slurm-%A_%a.out -o ./io/slurm-%A_%a.out --constraint=gizmok run_r.sh
# sbatch --depend=afterok:11 --array=1-20 --export=simba_run='main',cluster='bionic',type='R',project='z.Power-Grand-Bassa-stepped-wedge' -e ./io/slurm-%A_%a.out -o ./io/slurm-%A_%a.out --constraint=gizmok run_r.sh
# sbatch --depend=afterok:12 --export=simba_run='last',cluster='bionic',type='R',project='z.Power-Grand-Bassa-stepped-wedge' -e ./io/slurm-%A_%a.out -o ./io/slurm-%A_%a.out --constraint=gizmok run_r.sh

if (cfg$run_or_update=="run") {
  
  run_on_cluster(
    
    first = {
      
      # Simulation setup
      sim <- new_sim()
      sim %<>% set_config(
        num_sim = cfg$num_sim,
        parallel = cfg$parallel,
        stop_at_error = cfg$stop_at_error,
        seed = 1000,
        packages = cfg$pkgs
      )
      sim <- do.call(set_levels, c(list(sim), level_set))
      
      # Read and transform sampling frame
      sampling_frame <- read_excel("../data/GrandBassa2020SamplingFrame_truncated.xlsx")
      sampling_frame %<>% rowid_to_column("community_id")
      sampling_frame %<>% rename(
        "num_hh" = `numHH`,
        "health_district" = `map_health_district`,
        "admin_district" = `map_adm_distri`
      )
      
      # Add functions to simulation object
      sim %<>% add_creator(create_dataset)
      sim %<>% add_method(create_birth_history)
      sim %<>% add_method(take_sample)
      sim %<>% add_method(transform_dataset)
      sim %<>% add_method(perform_analysis)
      sim %<>% add_method(dates_to_cmc)
      sim %<>% add_method(cmc_to_dates)
      sim %<>% add_method(baseline_u5mr)
      
      # Add constants
      sim %<>% add_constants(sampling_frame=sampling_frame)

      # Simulation script
      sim %<>% set_script(one_simulation)
      
    },
    
    main = {
      sim %<>% run()
    },
    
    last = {
      
      sim %>% summarize() %>% print()
      
    },
    
    cluster_config = cluster_config
    
  )
  
}

if (cfg$run_or_update=="update") {
  
  update_sim_on_cluster(
    
    first = {
      sim <- readRDS('/home/akenny/z.Power-Grand-Bassa-stepped-wedge/sim.simba')
      sim <- do.call(set_levels, c(list(sim), level_set))
    },
    
    main = {
      sim %<>% update_sim()
    },
    
    last = {},
    
    cluster_config = cluster_config
    
  )
  
}



#################################.
##### MAIN: Process results #####
#################################.

if (FALSE) {
  
  # Summarize results
  # !!!!! Revisit this
  sim %>% summarize() %>% print()
  
}



#######################################################################.
##### TESTING: Generate fake dataset for testing analysis methods #####
#######################################################################.

if (FALSE) {
  
  # # Read in dataset
  # dataset <- readRDS("gb_dataset.rds")
  
  # Read and transform sampling frame
  sampling_frame <- read_excel("../data/GrandBassa2020SamplingFrame_truncated.xlsx")
  sampling_frame %<>% rowid_to_column("community_id")
  sampling_frame %<>% rename(
    "num_hh" = `numHH`,
    "health_district" = `map_health_district`,
    "admin_district" = `map_adm_distri`
  )
  
  # Take a sample from population
  sample <- take_sample(sampling_frame=sampling_frame, n_clusters=500)
  
  # Generate data for sample
  dataset <- create_dataset(
    sample = sample,
    program_effect = 0.25,
    re_comm_sd = 0.1,
    re_tx_sd = 0.1,
    show_progress = FALSE
  )
  
  # # Save dataset
  # saveRDS(dataset, file="gb_dataset.rds")
  
  # Perform statistical analysis
  recall_years <- 5
  
}
