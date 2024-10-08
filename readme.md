This repository contains the replication scripts for the article:
* Thomas Feliciani, Junwen Luo, Kalpana Shankar, Funding lotteries for research grant allocation: An extended taxonomy and evaluation of their fairness, _Research Evaluation_, Volume 33, Issue 1, 2024, rvae025, https://doi.org/10.1093/reseval/rvae025

These scripts run in R 4.3.2 and rely on some external libraries specified below (and also listed at the top of each script). The scripts are:

## util.r
Here we defined the fundamental functions necessary to run the simulation model -- most notably, *util.r* contains the implementation of the various Types of selection procedure.
This script is automatically executed by "simulation.r".
Runs in base R.

## simulation.r
This script is divided into two parts. The first part defines the main function "run", which runs a simulation model of a selection procedure with a specified parameter configuration. The second part of this script executes the simulation experiment by first defining a parameter space and then by calling "run" for each unique parameter configuration. It then saves the output in the folder "./output/".
Relies on the R library *compiler*.

## results.r
This scripts loads the results files generated by "batteries.r", draws all figures for the accompanying paper, and saves them to the folder "./outputGraphics/".
Relies on the R libraries *plyr*, *reshape2*, *ggplot2*.
