####POWER CALCULATIONS TO DETERMINE N FOR NO-CHOICE PROJECT####
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

#uncomment and run to install required  packages before running the first time
# install.packages("devtools")
# library(devtools)
# install.packages("lme4")
# install.packages("pwr")
# install.packages("writexl")
# install.packages("pastecs")
# install.packages("optimx")
# install.packages("car")
# install.packages("fitdistrplus")
# install.packages("emmeans")
# install.packages("multcompView")
# install.packages("viridis")
# install.packages("tidyverse")
# install.packages("ggridges")
# install_github("baptiste/egg")

#Loads the required packages for this analysis
lapply(packages_list, library, character.only = TRUE)

#removes the files in parentheses
rm(packages_list)


#calculate effect size
cohen.ES(test = "anov", size = "small")

cohen.ES(test = "anov", size = "medium")

cohen.ES(test = "anov", size = "large")

#calculate our n for different effect sizes
small <- pwr.anova.test(
  k = 8,
  n = NULL,
  f = 0.1,
  sig.level = 0.05,
  power = 0.8
)

medium <- pwr.anova.test(
  k = 8,
  n = NULL,
  f = 0.25,
  sig.level = 0.05,
  power = 0.8
)

large <- pwr.anova.test(
  k = 8,
  n = NULL,
  f = 0.4,
  sig.level = 0.05,
  power = 0.8
)

"plot"(small)
"plot"(medium)
"plot"(large)

#cleanup
rm(large, medium, small)

#from these results, we decided to have n >= 30,
#because we are looking for a medium effect size
