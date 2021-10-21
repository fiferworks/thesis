####COLONY SEX RATIO####

females <- tally(bhvr_data, sex == "f")
males <- tally(bhvr_data, sex == "m")
sex_ratio <- (females / males)
names(sex_ratio)[names(sex_ratio) == "n"] <- "F:M"
write_csv(sex_ratio, "results/total_sex_ratio.csv")
rm(females, males)




####MEAN +/- SEM TABLES OF NO-CHOICE DATA####
#Removed extreme outlier: "RB_p3_t1"
bhvr <- filter(bhvr_data, i_probes <= 11)

bhvr$sex_var <- interaction(bhvr$sex, bhvr$variety)

write_csv(bhvr, "results/bhvr.csv")

#removes the untrimmed data from the workspace
rm(bhvr_data)

#getting summary stats from each variety
#creating cleaned files for future access
bhvr_RB <- filter(bhvr, variety == "RB")

f_RB <- filter(bhvr_RB, sex == 'f')
m_RB <- filter(bhvr_RB, sex == 'm')
f_RB <-
  dplyr::select(f_RB,
                d_probes,
                d_walks,
                d_cleans,
                d_off,
                i_probes,
                i_walks,
                i_cleans,
                i_off)
m_RB <-
  dplyr::select(m_RB,
                d_probes,
                d_walks,
                d_cleans,
                d_off,
                i_probes,
                i_walks,
                i_cleans,
                i_off)

clean_RB <-
  dplyr::select(bhvr_RB,
                d_probes,
                d_walks,
                d_cleans,
                d_off,
                i_probes,
                i_walks,
                i_cleans,
                i_off)

write_xlsx(clean_RB, "data/clean_RB.xlsx")
write_xlsx(bhvr_RB, "data/bhvr_RB.xlsx")
write_xlsx(f_RB, "data/f_RB.xlsx")
write_xlsx(m_RB, "data/m_RB.xlsx")
rm(bhvr_RB, clean_RB, f_RB, m_RB)

bhvr_3LB <- filter(bhvr, variety == "3LB")
f_3LB <- filter(bhvr_3LB, sex == 'f')
m_3LB <- filter(bhvr_3LB, sex == 'm')
f_3LB <-
  dplyr::select(f_3LB,
                d_probes,
                d_walks,
                d_cleans,
                d_off,
                i_probes,
                i_walks,
                i_cleans,
                i_off)
m_3LB <-
  dplyr::select(m_3LB,
                d_probes,
                d_walks,
                d_cleans,
                d_off,
                i_probes,
                i_walks,
                i_cleans,
                i_off)
clean_3LB <-
  dplyr::select(bhvr_3LB,
                d_probes,
                d_walks,
                d_cleans,
                d_off,
                i_probes,
                i_walks,
                i_cleans,
                i_off)
write_xlsx(clean_3LB, "data/clean_3LB.xlsx")
write_xlsx(bhvr_3LB, "data/bhvr_3LB.xlsx")
write_xlsx(f_3LB, "data/f_3LB.xlsx")
write_xlsx(m_3LB, "data/m_3LB.xlsx")
rm(bhvr_3LB, clean_3LB, f_3LB, m_3LB)

bhvr_4LB <- filter(bhvr, variety == "4LB")
f_4LB <- filter(bhvr_4LB, sex == 'f')
m_4LB <- filter(bhvr_4LB, sex == 'm')
f_4LB <-
  dplyr::select(f_4LB,
                d_probes,
                d_walks,
                d_cleans,
                d_off,
                i_probes,
                i_walks,
                i_cleans,
                i_off)
m_4LB <-
  dplyr::select(m_4LB,
                d_probes,
                d_walks,
                d_cleans,
                d_off,
                i_probes,
                i_walks,
                i_cleans,
                i_off)
clean_4LB <-
  dplyr::select(bhvr_4LB,
                d_probes,
                d_walks,
                d_cleans,
                d_off,
                i_probes,
                i_walks,
                i_cleans,
                i_off)
write_xlsx(clean_4LB, "data/clean_4LB.xlsx")
write_xlsx(bhvr_4LB, "data/bhvr_4LB.xlsx")
write_xlsx(f_4LB, "data/f_4LB.xlsx")
write_xlsx(m_4LB, "data/m_4LB.xlsx")
rm(bhvr_4LB, clean_4LB, f_4LB, m_4LB)

bhvr_10LB <- filter(bhvr, variety == "10LB")
f_10LB <- filter(bhvr_10LB, sex == 'f')
m_10LB <- filter(bhvr_10LB, sex == 'm')
f_10LB <-
  dplyr::select(f_10LB,
                d_probes,
                d_walks,
                d_cleans,
                d_off,
                i_probes,
                i_walks,
                i_cleans,
                i_off)
m_10LB <-
  dplyr::select(m_10LB,
                d_probes,
                d_walks,
                d_cleans,
                d_off,
                i_probes,
                i_walks,
                i_cleans,
                i_off)
clean_10LB <-
  dplyr::select(bhvr_10LB,
                d_probes,
                d_walks,
                d_cleans,
                d_off,
                i_probes,
                i_walks,
                i_cleans,
                i_off)
write_xlsx(clean_10LB, "data/clean_10LB.xlsx")
write_xlsx(bhvr_10LB, "data/bhvr_10LB.xlsx")
write_xlsx(f_10LB, "data/f_10LB.xlsx")
write_xlsx(m_10LB, "data/m_10LB.xlsx")
rm(bhvr_10LB, clean_10LB, f_10LB, m_10LB)




####NO-CHOICE SUMMARY TABLE####
#makes a list of all cleaned files we just made
clean_list <-
  c(
    "data/clean_10LB.xlsx",
    "data/clean_3LB.xlsx",
    "data/clean_4LB.xlsx",
    "data/clean_RB.xlsx"
  )
variety_list <- c("10LB", "3LB", "4LB", "RB")

#a loop that gets descriptive statistics from each file and combines them in a table
for (file in clean_list) {
  if (!exists("all_stats")) {
    all_stats <- readxl::read_xlsx("data/dummy.xlsx", col_names = TRUE)
  }
  if (!exists("n_values")) {
    n_values <- readxl::read_xlsx("data/dummy.xlsx", col_names = TRUE)
  }

  if (exists("all_stats")) {
    all_stats_tmp <- readxl::read_xlsx(file)

    #calculates descriptive statistics
    desc_tmp <- stat.desc(all_stats_tmp)

    #mean and SEM
    mean_tmp <- desc_tmp[9,]
    SEM_tmp <- desc_tmp[10,]
    srow_tmp <- rbind(mean_tmp, SEM_tmp)
    all_stats <- rbind(all_stats, srow_tmp)

    #n for variety
    n_tmp <- as_tibble(desc_tmp[1, 1])
    names(n_tmp)[names(n_tmp) == "value"] <- "n"
    n_values <- rbind(n_values, n_tmp)

  }
  rm(desc_tmp, mean_tmp, SEM_tmp, srow_tmp, n_tmp, all_stats_tmp)
}

#adding on the variety names and saving the file
n_values <- add_column(n_values, variety_list, .before = TRUE)
names(n_values)[names(n_values) == "variety_list"] <- "variety"

#copying the variety_list
variety_listmean <- variety_list

#editing the values to add 'mean'
variety_listmean <- gsub("B", "B mean", variety_list)

#editing the values to add 'SEM'
variety_listSEM <- gsub("B", "B SEM", variety_list)

#adding the new columns together
variety_list <- as.tibble(c(variety_listmean, variety_listSEM))

#arranging and adding the correct name to the header
variety_list <- arrange(variety_list, value)
names(variety_list)[names(variety_list) == "value"] <- "variety"

#combines data to make one data tibble
all_stats <- signif(all_stats, digits = 2)
all_stats <- cbind(variety_list, all_stats)
all_stats <- as.tibble(all_stats)

#filters table by mean values
all_mean <-
  filter(
    all_stats,
    variety == "10LB mean" |
      variety == "3LB mean" |
      variety == "4LB mean" | variety == "RB mean"
  )
all_mean <-
  dplyr::select(
    all_mean,
    "d_probes",
    "d_walks",
    "d_cleans",
    "d_off",
    "i_probes",
    "i_walks",
    "i_cleans",
    "i_off"
  )

#filters table by sem values
sem <-
  filter(
    all_stats,
    variety == "10LB SEM" |
      variety == "3LB SEM" |
      variety == "4LB SEM" | variety == "RB SEM"
  )
sem <-
  dplyr::select(
    sem,
    "d_probes",
    "d_walks",
    "d_cleans",
    "d_off",
    "i_probes",
    "i_walks",
    "i_cleans",
    "i_off"
  )

#changes the sames for the sem table
names(sem)[names(sem) == "d_probes"] <- "d_probes SEM"
names(sem)[names(sem) == "d_walks"] <- "d_walks SEM"
names(sem)[names(sem) == "d_cleans"] <- "d_cleans SEM"
names(sem)[names(sem) == "d_off"] <- "d_off SEM"

names(sem)[names(sem) == "i_probes"] <- "i_probes SEM"
names(sem)[names(sem) == "i_walks"] <- "i_walks SEM"
names(sem)[names(sem) == "i_cleans"] <- "i_cleans SEM"
names(sem)[names(sem) == "i_off"] <- "i_off SEM"

#gets the variety names
var_names <-
  filter(
    all_stats,
    variety == "10LB mean" |
      variety == "3LB mean" |
      variety == "4LB mean" | variety == "RB mean"
  )
var_names <- dplyr::select(var_names, variety)

#gets the n values
n_values <- dplyr::select(n_values, n)

#creates the ± symbols in columns for later
p <- c('±', '±', '±', '±')
p <- as.tibble(cbind(p, p, p, p, p, p, p, p))

#combines the mean and sem tables
msem_table <- cbind(all_mean, sem)
data_table <- cbind(var_names, n_values, msem_table, p)


#rearranges the table
data_table <-
  as.tibble(
    dplyr::select(
      data_table,
      'variety',
      'n',
      'i_probes',
      'p',
      'i_probes SEM',
      'd_probes',
      'V2',
      'd_probes SEM',
      'i_walks',
      'V3',
      'i_walks SEM',
      'd_walks',
      'V4',
      'd_walks SEM',
      'i_cleans',
      'V5',
      'i_cleans SEM',
      'd_cleans',
      'V6',
      'd_cleans SEM',
      'i_off',
      'V7',
      'i_off SEM',
      'd_off',
      'V8',
      'd_off SEM'
    )
  )


#fixes the names
data_table$variety <- gsub('10LB mean', '10LB', data_table$variety)
data_table$variety <- gsub('3LB mean', '3LB', data_table$variety)
data_table$variety <- gsub('4LB mean', '4LB', data_table$variety)
data_table$variety <- gsub('RB mean', 'RB', data_table$variety)

names(data_table)[names(data_table) == "variety"] <- "Variety"

names(data_table)[names(data_table) == "i_probes"] <- "Probes"
names(data_table)[names(data_table) == "i_probes SEM"] <- "SEM"
names(data_table)[names(data_table) == "d_probes"] <- "Time (s)"
names(data_table)[names(data_table) == "d_probes SEM"] <- "SEM"

names(data_table)[names(data_table) == "i_walks"] <- "Walks"
names(data_table)[names(data_table) == "i_walks SEM"] <- "SEM"
names(data_table)[names(data_table) == "d_walks"] <- "Time (s)"
names(data_table)[names(data_table) == "d_walks SEM"] <- "SEM"

names(data_table)[names(data_table) == "i_cleans"] <- "Cleans"
names(data_table)[names(data_table) == "i_cleans SEM"] <- "SEM"
names(data_table)[names(data_table) == "d_cleans"] <- "Time (s)"
names(data_table)[names(data_table) == "d_cleans SEM"] <- "SEM"

names(data_table)[names(data_table) == "i_off"] <- "Off Leaf"
names(data_table)[names(data_table) == "i_off SEM"] <- "SEM"
names(data_table)[names(data_table) == "d_off"] <- "Time (s)"
names(data_table)[names(data_table) == "d_off SEM"] <- "SEM"

names(data_table)[names(data_table) == "p"] <- "±"
names(data_table)[names(data_table) == "V2"] <- "±"
names(data_table)[names(data_table) == "V3"] <- "±"
names(data_table)[names(data_table) == "V4"] <- "±"
names(data_table)[names(data_table) == "V5"] <- "±"
names(data_table)[names(data_table) == "V6"] <- "±"
names(data_table)[names(data_table) == "V7"] <- "±"
names(data_table)[names(data_table) == "V8"] <- "±"

#saves a copy
write_csv(data_table, "results/data_table.csv")

#cleanup
rm(
  all_stats,
  n_values,
  all_mean,
  sem,
  msem_table,
  var_names,
  data_table,
  file,
  variety_list,
  variety_listmean,
  variety_listSEM,
  clean_list,
  p
)

#file cleanup
file.remove(
  "data/clean_RB.xlsx",
  "data/clean_3LB.xlsx",
  "data/clean_4LB.xlsx",
  "data/clean_10LB.xlsx"
)


####NO-CHOICE DATA TABLE SPLIT BY SEX####
#makes a list of all sex-split cleaned files
clsex_list <-
  c(
    "data/f_10LB.xlsx",
    "data/m_10LB.xlsx",
    "data/f_3LB.xlsx",
    "data/m_3LB.xlsx",
    "data/f_4LB.xlsx",
    "data/m_4LB.xlsx",
    "data/f_RB.xlsx",
    "data/m_RB.xlsx"
  )

variety_list <-
  c("10LB_f",
    "10LB_m",
    "3LB_f",
    "3LB_m",
    "4LB_f",
    "4LB_m",
    "RB_f",
    "RB_m")

#a loop that gets descriptive statistics from each file and combines them in a table
for (file in clsex_list) {
  if (!exists("all_stats")) {
    all_stats <- readxl::read_xlsx("data/dummy.xlsx", col_names = TRUE)
  }
  if (!exists("n_values")) {
    n_values <- readxl::read_xlsx("data/dummy.xlsx", col_names = TRUE)
  }

  if (exists("all_stats")) {
    all_stats_tmp <- readxl::read_xlsx(file)

    #calculates descriptive statistics
    desc_tmp <- stat.desc(all_stats_tmp)

    #mean and SEM
    mean_tmp <- desc_tmp[9, ]
    SEM_tmp <- desc_tmp[10, ]
    srow_tmp <- rbind(mean_tmp, SEM_tmp)
    all_stats <- rbind(all_stats, srow_tmp)

    #n for variety
    n_tmp <- as_tibble(desc_tmp[1, 1])
    names(n_tmp)[names(n_tmp) == "value"] <- "n"
    n_values <- rbind(n_values, n_tmp)

  }
  rm(desc_tmp, mean_tmp, SEM_tmp, srow_tmp, n_tmp, all_stats_tmp)
}

#adding on the variety names and saving the file
n_values <- add_column(n_values, variety_list, .before = TRUE)
names(n_values)[names(n_values) == "variety_list"] <- "variety"

#making a list of the values
variety_list <- as.tibble(
  c(
    "10LB mean_f",
    "10LB SEM_f",
    "10LB mean_m",
    "10LB SEM_m",
    "3LB mean_f",
    "3LB SEM_f",
    "3LB mean_m",
    "3LB SEM_m",
    "4LB mean_f",
    "4LB SEM_f",
    "4LB mean_m",
    "4LB SEM_m",
    "RB mean_f",
    "RB SEM_f",
    "RB mean_m",
    "RB SEM_m"
  )
)

names(variety_list)[names(variety_list) == "value"] <- "variety"

#combines data to make one data tibble
all_stats <- signif(all_stats, digits = 2)
all_stats <- cbind(variety_list, all_stats)
all_stats <- as.tibble(all_stats)

#filters table by mean values
all_mean <-
  filter(
    all_stats,
    variety == "10LB mean_f" | variety == "10LB mean_m" |
      variety == "3LB mean_f" | variety == "3LB mean_m" |
      variety == "4LB mean_f" | variety == "4LB mean_m" |
      variety == "RB mean_f" | variety == "RB mean_m"
  )

all_mean <-
  dplyr::select(
    all_mean,
    "d_probes",
    "d_walks",
    "d_cleans",
    "d_off",
    "i_probes",
    "i_walks",
    "i_cleans",
    "i_off"
  )

#filters table by sem values
sem <-
  filter(
    all_stats,
    variety == "10LB SEM_f" | variety == "10LB SEM_m" |
      variety == "3LB SEM_f" | variety == "3LB SEM_m" |
      variety == "4LB SEM_f" | variety == "4LB SEM_m" |
      variety == "RB SEM_f" | variety == "RB SEM_m"
  )

sem <-
  dplyr::select(
    sem,
    "d_probes",
    "d_walks",
    "d_cleans",
    "d_off",
    "i_probes",
    "i_walks",
    "i_cleans",
    "i_off"
  )

#changes the sames for the sem table
names(sem)[names(sem) == "d_probes"] <- "d_prb_SEM"
names(sem)[names(sem) == "d_walks"] <- "d_wlk_SEM"
names(sem)[names(sem) == "d_cleans"] <- "d_cln_SEM"
names(sem)[names(sem) == "d_off"] <- "d_off_SEM"

names(sem)[names(sem) == "i_probes"] <- "i_prb_SEM"
names(sem)[names(sem) == "i_walks"] <- "i_wlk_SEM"
names(sem)[names(sem) == "i_cleans"] <- "i_cln_SEM"
names(sem)[names(sem) == "i_off"] <- "i_off_SEM"

#gets the variety names
var_names <-
  filter(
    all_stats,
    variety == "10LB mean_f" | variety == "10LB mean_m" |
      variety == "3LB mean_f" | variety == "3LB mean_m" |
      variety == "4LB mean_f" | variety == "4LB mean_m" |
      variety == "RB mean_f" | variety == "RB mean_m"
  )

var_names <- dplyr::select(var_names, variety)

#gets the n values
n_values <- dplyr::select(n_values, n)

#creates the ± symbols in columns for later
p <- c('±', '±', '±', '±')
p <- as.tibble(cbind(p, p, p, p, p, p, p, p))

#combines the mean and sem tables
msem_table <- cbind(all_mean, sem)
sex_table <- cbind(var_names, n_values, msem_table, p)
sex_table$variety <-
  c("10LB", "10LB", "3LB", "3LB", "4LB", "4LB", "RB", "RB")
sex_table$sex <-
  c("Female",
    "Male",
    "Female",
    "Male",
    "Female",
    "Male",
    "Female",
    "Male")

#rearranges the table
sex_table <-
  as.tibble(
    dplyr::select(
      sex_table,
      'variety',
      'n',
      'sex',
      'i_probes',
      'p',
      'i_prb_SEM',
      'd_probes',
      'V2',
      'd_prb_SEM',
      'i_walks',
      'V3',
      'i_wlk_SEM',
      'd_walks',
      'V4',
      'd_wlk_SEM',
      'i_cleans',
      'V5',
      'i_cln_SEM',
      'd_cleans',
      'V6',
      'd_cln_SEM',
      'i_off',
      'V7',
      'i_off_SEM',
      'd_off',
      'V8',
      'd_off_SEM'
    )
  )
#saves a copy
write_csv(sex_table, "results/sex_table.csv")

#deleting the temporary cleaned files
file.remove(clsex_list)

#cleanup
rm(
  all_mean,
  all_stats,
  clsex_list,
  msem_table,
  n_values,
  p,
  sem,
  var_names,
  variety_list,
  file,
  sex_table,
  sex_ratio
)




####SEX RATIO BY VARIETY####
#reads in the files which we seperated by variety
data_list <-
  c(
    "data/bhvr_10LB.xlsx",
    "data/bhvr_3LB.xlsx",
    "data/bhvr_4LB.xlsx",
    "data/bhvr_RB.xlsx"
  )

# a loop that calculates the sex ratios by variety
for (file in data_list) {
  if (!exists("sex_ratio")) {
    sex_ratio <-
      read_csv("results/total_sex_ratio.csv")
  }

  if (exists("sex_ratio")) {
    sex_tmp <- readxl::read_xlsx(file)
    females <- tally(sex_tmp, sex == "f")
    males <- tally(sex_tmp, sex == "m")
    sratio_tmp <- as.tibble(females / males)
    names(sratio_tmp)[names(sratio_tmp) == "n"] <- "F:M"
    sex_ratio <- rbind(sex_ratio, sratio_tmp)

  }
  rm(females, males, sex_tmp, sratio_tmp)
}
variety <- c("Total", "10LB", "3LB", "4LB", "RB")
sex_ratio <- add_column(sex_ratio, variety, .before = TRUE)
sex_ratio <- arrange(sex_ratio, variety)
sex_ratio$`F:M` <- round(sex_ratio$'F:M', digits = 2)
names(sex_ratio)[names(sex_ratio) == "variety"] <- "Variety"

write_csv (sex_ratio, "results/total_sex_ratio.csv")
rm(sex_ratio, file, variety, data_list)
