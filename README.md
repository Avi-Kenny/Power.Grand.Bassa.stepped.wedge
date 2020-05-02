# Power Calculator: Grand Bassa stepped wedge impact evaluation

## Overview
- Briefly, this program does the following:
	1) Generate a population representing that of Grand Bassa County, Liberia.
	2) For some large number N (e.g. N=1,000), simulate the entire study N times. This involves the following steps:
		1) Sample from the population in a way that mimics the cluster sampling procedure that will be used in the actual study.
		2) For a given sample size, "survey" the sampled households/individuals. This amounts to creating a smaller dataset that includes relevant variables and introduces a layer of data quality issues (e.g. nonresponse).
		3) Process the dataset and run one or more analyses involving a hypothesis test of program effectiveness.
	3) Calculate the power of the study as the percentage of the N simulations for which the null hypothesis was rejected.
	4) By repeating steps 1-3 for a number of different sample sizes, we can determine the sample size needed for 80% power, 90% power, etc.
- Note: we may wish to modify the procedure above such that the population is re-generated for each simulation replicate.

## Details of population generation
- The code that generates the population can be viewed as a function of four things: the Grand Bassa sampling frame (a list of all communities and the number of households per community), a baseline mortality curve, the anticipated program effect (the overall level of the effect and the "onset curve" of the effect), and the pattern of implementation (when did each community receive the intervention). This code does the following:
	1) Loop through all communities and all households and choose an integer between 0 and ~12 randomly from `distribution A`; this integer represents the number of women of reproductive age who live in that household.
	2) Generate each woman's age by sampling from `distribution B`.
	3) Generate each woman's reproductive history through the following process:
		1) Loop through every year between age 13 and the mother's current age. For each year, sample from `distribution C` to determine whether the woman gave birth that year. If a 1 (birth) is sampled, randomly select a birth date in that year. This will generate the "births" component of the reproductive history.
		2) For each birth, loop through every year between 1 and min{5, child age} and sample from `distribution D` to determine whether the child died that year. If a 1 (death) is sampled, randomly select a death age either from `distribution E` (if the child died in the first year) or uniformly randomly (if the child died in the second year or later). This will generate the "deaths" component of the reproductive history.
- The above code relies on a number of distributions, some of which are discrete uniform distributions and some of which are Bernoulli distributions with associated "probability functions" (e.g. probability of birth as a function of woman's age). These should be calculated in advance using LMS data. `Distribution A` is the distribution of the number of women of reproductive age per household. `Distribution B` is the age distribution of all women aged 15-49. `Distribution C` is the probability of giving birth as a function of woman's age and calendar year, plus a community-level random effect. `Distribution D` is the probability of child dying at age k conditional on survival until age k-1 (among children aged 0-4) as a function of calendar year, child’s age, and mother's age, plus a community-level random effect (and multiplied by a "program effect" factor). `Distribution E` is the distribution of deaths ages (in months) among children 0-11 months who died.
- For distributions `A`, `B`, and `E`, we can use the empirical distributions from LMS data. Distributions `C` and `D` can be determined through logistic regression using LMS data.

## Instructions
- [TO DO]
