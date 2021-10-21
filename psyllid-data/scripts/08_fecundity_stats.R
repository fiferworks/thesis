####FECUNDITY DATA PREPARATION####
#reading in datasheet
fcnd_data <- readxl::read_xlsx("data/fecundity_master.xlsx")

#filtering psyllids which did not lay eggs
fcnd_data <-
  dplyr::filter(fcnd_data, fcnd_data$tot_egg != 0)

#splitting and tidying up the data so each row is a single observation, easier for modelling
fegg <-
  gather(fcnd_data,
         egg_1,
         egg_2,
         egg_3,
         egg_4,
         key = "egg_period",
         value = "count_eggs")
fegg <-
  select(fegg,
         'psyllid',
         'germ',
         'block',
         'letter',
         'egg_period',
         'count_eggs')

#the same for nymphs
fnym <-
  gather(fcnd_data,
         nym_1,
         nym_2,
         nym_3,
         nym_4,
         key = "nym_period",
         value = "count_nym")
fnym <-
  select(fnym,
         'psyllid',
         'germ',
         'block',
         'letter',
         'nym_period',
         'count_nym')

#renaming columns for eventual table combining
names(fegg)[names(fegg) == "egg_period"] <- "period"
names(fegg)[names(fegg) == "count_eggs"] <- "eggs"
fegg$period <- gsub('egg_', '', fegg$period)

#same for nymphs
names(fnym)[names(fnym) == "nym_period"] <- "period"
names(fnym)[names(fnym) == "count_nym"] <- "nymphs"
fnym$period <- gsub('nym_', '', fnym$period)

#combining tables
fcnd <- inner_join(fegg, fnym)

#subsetting the data to remove nas and correcting nymphs > eggs errors
fcnd_1<-subset(fcnd, fcnd$nymphs < fcnd$eggs)
fcnd_2<-subset(fcnd, fcnd$nymphs == fcnd$eggs)
fcnd_err<-subset(fcnd, fcnd$nymphs > fcnd$eggs)
fcnd_err$eggs<-fcnd_err$nymphs
fcnd<-bind_rows(fcnd_err, fcnd_1, fcnd_2)

#creating column for hatch ratio and removing divide by zero errors
fcnd$hatch <- fcnd$nymphs/fcnd$eggs
fcnd$hatch[is.nan(fcnd$hatch) == T] <- NA

#ordering the data
fcnd<-arrange(fcnd, psyllid, period, germ)

#saving copies
write_csv (fcnd_data, "results/fcnd_data.csv")
write_csv (fcnd, "results/fcnd.csv")
rm(fegg, fnym)

#filtering data by germplasm
fcnd_10LB <- (filter(fcnd_data, germ == "10LB"))
write_xlsx(fcnd_10LB, "data/fcnd_10LB.xlsx")

fcnd_3LB <- (filter(fcnd_data, germ == "3LB"))
write_xlsx(fcnd_3LB, "data/fcnd_3LB.xlsx")

fcnd_4LB <- (filter(fcnd_data, germ == "4LB"))
write_xlsx(fcnd_4LB, "data/fcnd_4LB.xlsx")

fcnd_RB <- (filter(fcnd_data, germ == "RB"))
write_xlsx(fcnd_RB, "data/fcnd_RB.xlsx")

rm(fcnd_10LB, fcnd_3LB, fcnd_4LB, fcnd_RB)



#makes a list of all files we just made
kleen_list <-
  c(
    "data/fcnd_10LB.xlsx",
    "data/fcnd_3LB.xlsx",
    "data/fcnd_4LB.xlsx",
    "data/fcnd_RB.xlsx"
  )

#custom function which calculates standard error of the means
SEM <- function(x)
  +
  sqrt(var(x, na.rm = TRUE) / length(na.omit(x)))

####SUMMARY STATS-CALCULATION LOOP####
#a loop that gets descriptive statistics from each file and combines them into different tables
for (file in kleen_list) {
  if (!exists("tbl_tmp")) {
    tbl_tmp <- readxl::read_xlsx("data/dummy.xlsx", col_names = TRUE)
  }
  if (!exists("eg_tbl")) {
    eg_tbl <- readxl::read_xlsx("data/dummy.xlsx", col_names = TRUE)
    names(eg_tbl)[names(eg_tbl) == "probe"] <- "period"
    names(eg_tbl)[names(eg_tbl) == "walk"] <- "germ"
    names(eg_tbl)[names(eg_tbl) == "clean"] <- "mean"
    names(eg_tbl)[names(eg_tbl) == "off leaf"] <- "sem"
    ny_tbl <- eg_tbl
    egday_1 <- eg_tbl
    egday_2 <- eg_tbl
    egday_3 <- eg_tbl
    egday_4 <- eg_tbl
    nyday_1 <- eg_tbl
    nyday_2 <- eg_tbl
    nyday_3 <- eg_tbl
    nyday_4 <- eg_tbl
    ht_tbl <- eg_tbl
    htday_1 <- eg_tbl
    htday_2 <- eg_tbl
    htday_3 <- eg_tbl
    htday_4 <- eg_tbl
  }

  if (exists("tbl_tmp")) {
    tbl_tmp <- readxl::read_xlsx(file)

    #gets the name of the germplasm
    germ <- tbl_tmp[1, 2]

    #calculating means
    egm_1 <- mean(tbl_tmp$egg_1, na.rm = T)
    egm_2 <- mean(tbl_tmp$egg_2, na.rm = T)
    egm_3 <- mean(tbl_tmp$egg_3, na.rm = T)
    egm_4 <- mean(tbl_tmp$egg_4, na.rm = T)
    egm_tot <- mean(tbl_tmp$tot_egg, na.rm = T)

    nym_1 <- mean(tbl_tmp$nym_1, na.rm = T)
    nym_2 <- mean(tbl_tmp$nym_2, na.rm = T)
    nym_3 <- mean(tbl_tmp$nym_3, na.rm = T)
    nym_4 <- mean(tbl_tmp$nym_4, na.rm = T)
    nym_tot <- mean(tbl_tmp$tot_nym, na.rm = T)

    htm_1 <-
      (sum(tbl_tmp$nym_1, na.rm = T) / sum(tbl_tmp$egg_1, na.rm = T))
    htm_2 <-
      (sum(tbl_tmp$nym_2, na.rm = T) / sum(tbl_tmp$egg_2, na.rm = T))
    htm_3 <-
      (sum(tbl_tmp$nym_3, na.rm = T) / sum(tbl_tmp$egg_3, na.rm = T))
    htm_4 <-
      (sum(tbl_tmp$nym_4, na.rm = T) / sum(tbl_tmp$egg_4, na.rm = T))
    htm_tot <-
      (sum(tbl_tmp$tot_nym, na.rm = T) / sum(tbl_tmp$tot_egg, na.rm = T))

    #calculates sems with function we made earlier
    egsem_1 <- SEM(tbl_tmp$egg_1)
    egsem_2 <- SEM(tbl_tmp$egg_2)
    egsem_3 <- SEM(tbl_tmp$egg_3)
    egsem_4 <- SEM(tbl_tmp$egg_4)
    egsem_tot <- SEM(tbl_tmp$tot_egg)

    nysem_1 <- SEM(tbl_tmp$nym_1)
    nysem_2 <- SEM(tbl_tmp$nym_2)
    nysem_3 <- SEM(tbl_tmp$nym_3)
    nysem_4 <- SEM(tbl_tmp$nym_4)
    nysem_tot <- SEM(tbl_tmp$tot_nym)

    htsem_1 <- SEM(tbl_tmp$ht_1)
    htsem_2 <- SEM(tbl_tmp$ht_2)
    htsem_3 <- SEM(tbl_tmp$ht_3)
    htsem_4 <- SEM(tbl_tmp$ht_4)
    htsem_tot <- SEM(tbl_tmp$percent)

    #combines data by day for a later graph
    egd_1 <- as.tibble(cbind('Period 1', germ, egm_1, egsem_1))
    egd_2 <- as.tibble(cbind('Period 2', germ, egm_2, egsem_2))
    egd_3 <- as.tibble(cbind('Period 3', germ, egm_3, egsem_3))
    egd_4 <- as.tibble(cbind('Period 4', germ, egm_4, egsem_4))

    nyd_1 <- as.tibble(cbind('Period 1', germ, nym_1, nysem_1))
    nyd_2 <- as.tibble(cbind('Period 2', germ, nym_2, nysem_2))
    nyd_3 <- as.tibble(cbind('Period 3', germ, nym_3, nysem_3))
    nyd_4 <- as.tibble(cbind('Period 4', germ, nym_4, nysem_4))

    htd_1 <- as.tibble(cbind('Period 1', germ, htm_1, htsem_1))
    htd_2 <- as.tibble(cbind('Period 2', germ, htm_2, htsem_2))
    htd_3 <- as.tibble(cbind('Period 3', germ, htm_3, htsem_3))
    htd_4 <- as.tibble(cbind('Period 4', germ, htm_4, htsem_4))

    #fixing names in the graph data so we can combine the columns correctly later
    names(egd_1)[names(egd_1) == '"Period 1"'] <- "period"
    names(egd_2)[names(egd_2) == '"Period 2"'] <- "period"
    names(egd_3)[names(egd_3) == '"Period 3"'] <- "period"
    names(egd_4)[names(egd_4) == '"Period 4"'] <- "period"

    names(egd_1)[names(egd_1) == 'egm_1'] <- "mean"
    names(egd_2)[names(egd_2) == 'egm_2'] <- "mean"
    names(egd_3)[names(egd_3) == 'egm_3'] <- "mean"
    names(egd_4)[names(egd_4) == 'egm_4'] <- "mean"

    names(egd_1)[names(egd_1) == 'egsem_1'] <- "sem"
    names(egd_2)[names(egd_2) == 'egsem_2'] <- "sem"
    names(egd_3)[names(egd_3) == 'egsem_3'] <- "sem"
    names(egd_4)[names(egd_4) == 'egsem_4'] <- "sem"

    names(nyd_1)[names(nyd_1) == '"Period 1"'] <- "period"
    names(nyd_2)[names(nyd_2) == '"Period 2"'] <- "period"
    names(nyd_3)[names(nyd_3) == '"Period 3"'] <- "period"
    names(nyd_4)[names(nyd_4) == '"Period 4"'] <- "period"

    names(nyd_1)[names(nyd_1) == 'nym_1'] <- "mean"
    names(nyd_2)[names(nyd_2) == 'nym_2'] <- "mean"
    names(nyd_3)[names(nyd_3) == 'nym_3'] <- "mean"
    names(nyd_4)[names(nyd_4) == 'nym_4'] <- "mean"

    names(nyd_1)[names(nyd_1) == 'nysem_1'] <- "sem"
    names(nyd_2)[names(nyd_2) == 'nysem_2'] <- "sem"
    names(nyd_3)[names(nyd_3) == 'nysem_3'] <- "sem"
    names(nyd_4)[names(nyd_4) == 'nysem_4'] <- "sem"

    names(htd_1)[names(htd_1) == '"Period 1"'] <- "period"
    names(htd_2)[names(htd_2) == '"Period 2"'] <- "period"
    names(htd_3)[names(htd_3) == '"Period 3"'] <- "period"
    names(htd_4)[names(htd_4) == '"Period 4"'] <- "period"

    names(htd_1)[names(htd_1) == 'htm_1'] <- "mean"
    names(htd_2)[names(htd_2) == 'htm_2'] <- "mean"
    names(htd_3)[names(htd_3) == 'htm_3'] <- "mean"
    names(htd_4)[names(htd_4) == 'htm_4'] <- "mean"

    names(htd_1)[names(htd_1) == 'htsem_1'] <- "sem"
    names(htd_2)[names(htd_2) == 'htsem_2'] <- "sem"
    names(htd_3)[names(htd_3) == 'htsem_3'] <- "sem"
    names(htd_4)[names(htd_4) == 'htsem_4'] <- "sem"
  }

  #data for graphs later
  egday_1 <- bind_rows(egday_1, egd_1)
  egday_2 <- bind_rows(egday_2, egd_2)
  egday_3 <- bind_rows(egday_3, egd_3)
  egday_4 <- bind_rows(egday_4, egd_4)

  nyday_1 <- bind_rows(nyday_1, nyd_1)
  nyday_2 <- bind_rows(nyday_2, nyd_2)
  nyday_3 <- bind_rows(nyday_3, nyd_3)
  nyday_4 <- bind_rows(nyday_4, nyd_4)

  htday_1 <- bind_rows(htday_1, htd_1)
  htday_2 <- bind_rows(htday_2, htd_2)
  htday_3 <- bind_rows(htday_3, htd_3)
  htday_4 <- bind_rows(htday_4, htd_4)

  eg_tbl <- bind_rows(egday_1, egday_2, egday_3, egday_4)
  ny_tbl <- bind_rows(nyday_1, nyday_2, nyday_3, nyday_4)
  ht_tbl <- bind_rows(htday_1, htday_2, htday_3, htday_4)
}
#ignore the warnings about 'coercing into a character vector', that is what we want

#fixing names
names(eg_tbl)[names(eg_tbl) == 'germ'] <- "Germplasm"
names(eg_tbl)[names(eg_tbl) == 'mean'] <- "Mean"
names(eg_tbl)[names(eg_tbl) == 'sem'] <- "SEM"

names(ny_tbl)[names(ny_tbl) == 'germ'] <- "Germplasm"
names(ny_tbl)[names(ny_tbl) == 'mean'] <- "Mean"
names(ny_tbl)[names(ny_tbl) == 'sem'] <- "SEM"

names(ht_tbl)[names(ht_tbl) == 'germ'] <- "Germplasm"
names(ht_tbl)[names(ht_tbl) == 'mean'] <- "Mean"
names(ht_tbl)[names(ht_tbl) == 'sem'] <- "SEM"

#combining dataset
fcnd_table <- bind_cols(eg_tbl, ny_tbl, ht_tbl)
fcnd_table <-
  select(fcnd_table,
         "period",
         "Germplasm",
         'Mean',
         'SEM',
         "Mean1",
         "SEM1",
         "Mean2",
         "SEM2")

#renaming parts of dataset
names(fcnd_table)[names(fcnd_table) == 'period'] <- "Period"
names(fcnd_table)[names(fcnd_table) == 'Mean'] <- "Egg_Mean"
names(fcnd_table)[names(fcnd_table) == 'SEM'] <- "Egg_SEM"

names(fcnd_table)[names(fcnd_table) == 'Mean1'] <- "Nymph_Mean"
names(fcnd_table)[names(fcnd_table) == 'SEM1'] <- "Nymph_SEM"

names(fcnd_table)[names(fcnd_table) == 'Mean2'] <- "Hatch_Percent"
names(fcnd_table)[names(fcnd_table) == 'SEM2'] <- "Hatch_SEM"

#converting ratios into percentages
fcnd_table$Hatch_Percent <- fcnd_table$Hatch_Percent * 100
fcnd_table$Hatch_SEM <- fcnd_table$Hatch_SEM * 100

#saving files
write_xlsx(fcnd_table, "results/fcnd_table.xlsx")

#cleanup all the things!
file.remove(kleen_list)
rm(list = ls())
