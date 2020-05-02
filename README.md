# Power Calculator: Grand Bassa stepped wedge impact evaluation

## Overview
- Briefly, this program does the following:
	1) Generate a population representing that of Grand Bassa County, Liberia.
	2) For some arbitrary large number (e.g. N=1,000), simulate the entire study N times. This involves the following steps:
		a) Sample from the population in a way that mimics the cluster sampling procedure that will be used in the actual study.
		b) "Survey" the sampled households/individuals. This amounts to creating a smaller dataset that includes relevant variables and introduces a layer of data quality issues (e.g. nonresponse).
		c) Process the dataset and run one or more analyses involving hypothesis test around program effect.
	3) Finally, calculate the power of the study as the percentage of the N simulations for which the null hypothesis was rejected.
- Note: we may wish to modify the procedure above such that the population is re-generated for each simulation replicate.

## Details of population generation
- The code that generates the population can be viewed as a function of four things: the Grand Bassa sampling frame (a list of all communities and the number of households per community), a baseline mortality curve, the anticipated program effect (the overall level of the effect and the "onset curve" of the effect), and the pattern of implementation (when did each community receive the intervention).
- The code that generates the population does the following:
	1) Loop through all communities and all households and choose an integer between 0 and ~12 randomly from `distribution A`; this integer represents the number of women of reproductive age who live in that household.
	2) Generate each woman's age by sampling from `distribution B`.
	3) Generate each woman's reproductive history through the following process:
		a) Loop through every year between age 13 and the mother's current age. For each year, sample from `distribution C` to determine whether the woman gave birth that year. If a 1 (birth) is sampled, randomly select a birth date in that year. This will generate the "births" component of the reproductive history.
		b) For each child, loop through every year between 1 and (up to) 5 and sample from `distribution D` to determine whether the child died that year. If a 1 (death) is sampled, randomly select a death date in that year from a distribution (a non-uniform distribution that reflects the true age-at-death structure). This will generate the "deaths" component of the reproductive history.
- The above code relies on a number of distributions, some of which are discrete uniform distributions and some of which are "probability functions" (e.g. probability of birth as a function of woman's age). These should be calculated in advance using LMS data. `Distribution A` is the distribution of the number of women of reproductive age per household. `Distribution B` is the age distribution of all women aged 15-49. `Distribution C` is the probability of giving birth as a function of a woman's age and calendar year. `Distribution D` is the probability of child death (in a year) among children aged 0-4 as a function of calendar year, child’s age, and mother's age.
- For distributions A and B, we can use the empirical distributions from LMS data. Distributions C and D can be determined through logistic regression using LMS data.

## Instructions
- [TO DO]
