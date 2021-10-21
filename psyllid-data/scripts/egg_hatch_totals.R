####EGG AND HATCH TOTALS####
#these data are used for the total columns in table 3.7
library(tidyverse)
library(writexl)

fecnd <-
  readxl::read_xlsx('results/Psyllid Fecundity on Four Germplasms.xlsx')

#filtering by Period
per_1 <- filter(fecnd, Period == 'Period 1')
per_2 <- filter(fecnd, Period == 'Period 2')
per_3 <- filter(fecnd, Period == 'Period 3')
per_4 <- filter(fecnd, Period == 'Period 4')

#total eggs per Period
teg_1 <- mean(per_1$Eggs)
teg_2 <- mean(per_2$Eggs)
teg_3 <- mean(per_3$Eggs)
teg_4 <- mean(per_4$Eggs)

#calculating SEMS
sm_teg_1 <- sd(per_1$Eggs) / sqrt(length(per_1$Eggs))
sm_teg_2 <- sd(per_2$Eggs) / sqrt(length(per_2$Eggs))
sm_teg_3 <- sd(per_3$Eggs) / sqrt(length(per_3$Eggs))
sm_teg_4 <- sd(per_4$Eggs) / sqrt(length(per_4$Eggs))


#hatch rate per Period
hch_1 <- sum(per_1$Nymphs) / sum(per_1$Eggs)
hch_2 <- sum(per_2$Nymphs) / sum(per_2$Eggs)
hch_3 <- sum(per_3$Nymphs) / sum(per_3$Eggs)
hch_4 <- sum(per_4$Nymphs) / sum(per_4$Eggs)

#preparing hatch data for SEMs
n_1 <- filter(per_1, `Hatch %` > 0)
n_2 <- filter(per_2, `Hatch %` > 0)
n_3 <- filter(per_3, `Hatch %` > 0)
n_4 <- filter(per_4, `Hatch %` > 0)

hach_1 <- n_1$Nymphs / n_1$Eggs
hach_2 <- n_2$Nymphs / n_2$Eggs
hach_3 <- n_3$Nymphs / n_3$Eggs
hach_4 <- n_4$Nymphs / n_4$Eggs

#calculating hatch SEMs
sm_hch_1 <- sd(hach_1) / sqrt(length(hach_1))
sm_hch_2 <- sd(hach_2) / sqrt(length(hach_2))
sm_hch_3 <- sd(hach_3) / sqrt(length(hach_3))
sm_hch_4 <- sd(hach_4) / sqrt(length(hach_4))

#making a table of the data
ttable <-
  as.tibble(
    cbind(
      teg_1,
      sm_teg_1,
      teg_2,
      sm_teg_2,
      teg_3,
      sm_teg_3,
      teg_4,
      sm_teg_4,
      hch_1,
      sm_hch_1,
      hch_2,
      sm_hch_2,
      hch_3,
      sm_hch_3,
      hch_4,
      sm_hch_4
    )
  )
ttable <- rename(
  ttable,
  `Egg Total for Period 1` = teg_1,
  `Egg Total SEM Period 1` = sm_teg_1,
  `Egg Total for Period 2` = teg_2,
  `Egg Total SEM for Period 2` = sm_teg_2,
  `Egg Total for Period 3` = teg_3,
  `Egg Total SEM for Period 3` = sm_teg_3,
  `Egg Total for Period 4` = teg_4,
  `Egg Total SEM for Period 4` = sm_teg_4,
  `Fecundity for Period 1` = hch_1,
  `Fecundity SEM for Period 1` = sm_hch_1,
  `Fecundity for Period 2` = hch_2,
  `Fecundity SEM for Period 2` = sm_hch_2,
  `Fecundity for Period 3` = hch_3,
  `Fecundity SEM for Period 3` = sm_hch_3,
  `Fecundity for Period 4` = hch_4,
  `Fecundity SEM for Period 4` = sm_hch_4
)
write_xlsx(ttable, 'results/Egg and Hatch Totals plus SEMs.xlsx')
rm(list = ls())
