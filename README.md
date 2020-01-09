# Power_Grand-Bassa-stepped-wedge

## Overview
- Briefly, this program does the following:
    1) Generate a population according to user-supplied parameters (e.g. population size) that is meant to be similar to the population of Grand Bassa
	2) For some arbitrary large number (e.g. N=1,000), simulate the entire study. This involves the following steps:
	    a) Sample from the population in a way that mimics the cluster sampling procedure that will be used in the actual study
		b) "Survey" the sampled households/individuals. This amounts to creating a smaller dataset that includes relevant variables and introduces a layer of data quality issues (i.e. missing and/or mis-recorded data).
		c) Process the dataset and run one or more analyses involving a hypothesis test of "effect vs. no effect".
	3) Finally, calculate the power of the study as the percentage of the N simulations for which the null hypothesis was rejected.
- Note: we may wish to modify the procedure above such that the population is re-generated a number of times. This would be a trivial extension.

## Instructions

- If running this locally on a laptop/desktop, ignore the files `run_r.sh` and `run_rmd.R`. If running this on a cluster computing system, these two files facilitate submission of `MAIN.Rmd` as a batch job. These files are written for use on a Linux cluster using Sun Grid Engine (Oracle Grid Engine). Modify these files as needed for other systems.
- Modify the following sections of `MAIN.Rmd`, and then run the file, either locally or on a cluster computing system:
    1) [TO DO]
	2) [TO DO]
	3) [TO DO]
