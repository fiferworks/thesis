####PSYLLID ANALYSIS PROCEDURES####
#
# Austin Nathaniel Fife 2018
# As part of his M.S. Thesis in Entomology
#
# By running these scripts in order, you can
# recreate the analysis I preformed on the
# data and reproduce the graphs I used as well.
# Please read the readmes in each folder, they
# help provide context for the files contained
# within. The analysis seen here is divided into
# seven scripts which can be accessed in the
# 'scripts' folder if you would like to see the
# details of the analysis.
# The script runs pretty well, but sometimes
# a few of the text outputs in the /results
# folder will fail to record the file properly
# in that case, you should open the actual scripts
# in R and run them in order if you want those files
# to be fixed

packages_list <-
  c("devtools",
    "lme4",
    "pwr",
    "writexl",
    "pastecs",
    "optimx",
    "car",
    "fitdistrplus",
    "emmeans",
    "multcompView",
    "viridis",
    "tidyverse",
    "ggridges",
    "egg"
  )

#uncomment and run these two lines to install required  packages before running the first time
# lapply(packages_list, install.packages)
# library(devtools)
# install_github("baptiste/egg") #broken?

#Loads the required packages for this analysis
lapply(packages_list, library, character.only = TRUE)

#removes the files in parentheses
rm(packages_list)

source('scripts/01_combine_data.R')
source('scripts/02_behavior_stats.R')
source('scripts/03_behavior_models.R')
source('scripts/04_probing_graphs.R')
source('scripts/05_walking_graphs.R')
source('scripts/06_cleaning_graphs.R')
source('scripts/07_leaving_graphs.R')
source('scripts/08_fecundity_stats.R')
source('scripts/09_fecundity_models.R')
source('scripts/10_fecundity_graphs.R')

# optional scripts you can run for other graphs
# or to see my power calculations
# source('scripts/other_graphs.R')
# source('scripts/power_calculations.R')

####FINAL CLEANUP####
rm(list = ls())
print("DONE!")
