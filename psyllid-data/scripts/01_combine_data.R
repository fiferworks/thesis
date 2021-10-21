####COMBINING INCIDENCE DATA####

#Makes a list of all .csv files in the Psyllid_Data directory
inc_files <- list.files(path = "data", pattern = "*.csv")

#adds the direcory information so we can access each file correctly in R
inc_files <- paste('data/', inc_files, sep = '')

#Using the filelist we created, we create temporary files, extract incidence values, and record them as a dataframe
#R will do the following actions for all files listed in "filelist"

#R checks if we have the temp file "dummy", if not, R creates one
for (file in inc_files) {
  if (!exists("inc_data")) {
    inc_data <- readxl::read_xlsx("data/dummy.xlsx", col_names = TRUE)
  }
  #if R finds the file "inc_data", it reads each file in the directory, and saves it as a temporary file "inc_data_temp"
  if (exists("inc_data")) {
    inc_data_temp <- read_csv(file)

    #reads off the name of the file
    print(file)
    file_name <- file
    #counts the matches of the string in quotes from the inc_data_temp file
    iprobe_temp <- tally(inc_data_temp, code == "Probe/Feed")
    iwalk_temp <- tally(inc_data_temp, code == "Walk")
    iclean_temp <- tally(inc_data_temp, code == "Cleaning")
    ioff_temp <- tally(inc_data_temp, code == "OFF Leaf")

    names(iprobe_temp)[names(iprobe_temp) == "n"] <- "i_probes"
    names(iwalk_temp)[names(iwalk_temp) == "n"] <- "i_walks"
    names(iclean_temp)[names(iclean_temp) == "n"] <- "i_cleans"
    names(ioff_temp)[names(ioff_temp) == "n"] <- "i_off"

    #combines the columns from the temp files in a row
    irow_temp <-
      cbind(file_name, iprobe_temp, iwalk_temp, iclean_temp, ioff_temp)
    #appends that row to our inc_data file
    inc_data <- rbind(inc_data, irow_temp)
    #removes the temp files
    rm(inc_data_temp,
       iprobe_temp,
       iwalk_temp,
       iclean_temp,
       ioff_temp,
       irow_temp)
  }
}

#fixing names for a new column with the plant number listed
inc_data$file_name <- gsub('data/', '', inc_data$file_name)
plant_number <- inc_data$file_name
plant_number <- gsub('_t\\d\\d.csv', '', plant_number)
plant_number <- gsub('_t\\d.csv', '', plant_number)
plant_number <- gsub('10LB_p', '', plant_number)
plant_number <- gsub('3LB_p', '', plant_number)
plant_number <- gsub('4LB_p', '', plant_number)
plant_number <- gsub('RB_p', '', plant_number)

inc_data <- add_column(inc_data, plant_number, .after = 1)

inc_data <- as.tibble(inc_data)

#cleans up the remaining temporary files
rm(inc_files, plant_number, file, file_name)

#loading and filtering master datasheet for no choice data
no_choice_master <- readxl::read_xlsx("data/no_choice_master.xlsx")
no_choice_temp <-
  filter(no_choice_master,
         Variety == "3LB" |
           Variety == "4LB" |
           Variety == "10LB" | Variety == "RB")
no_choice_extra <-
  dplyr::select(
    no_choice_temp,
    'Psyllid Number',
    '(mins) Time Away From Colony',
    Sex,
    'filelist',
    Variety,
    'Plant Size'
  )
names(no_choice_extra)[names(no_choice_extra) == '(mins) Time Away From Colony'] <-
  "mins_away"
names(no_choice_extra)[names(no_choice_extra) == 'Psyllid Number'] <-
  "psyllid"
names(no_choice_extra)[names(no_choice_extra) == 'Plant Size'] <-
  "plant_size"
names(no_choice_extra)[names(no_choice_extra) == 'Variety'] <-
  "variety"
names(no_choice_extra)[names(no_choice_extra) == 'Sex'] <-
  "sex"
names(no_choice_extra)[names(no_choice_extra) == 'filelist'] <-
  "file_name"
rm(no_choice_master, no_choice_temp)

#telling R what the columns are
inc_data$plant_number <- as.factor(inc_data$plant_number)
no_choice_extra$file_name <- as.factor(no_choice_extra$file_name)
inc_data <- inner_join(inc_data, no_choice_extra)

#renaming file so we can later join it with duration data
names(inc_data)[names(inc_data) == 'file_name'] <-
  "file"

#datasheet with sex appended
write_csv (inc_data, "results/inc_data.csv")
rm(no_choice_extra)

####COMBINING DURATION DATA####
#list of all of the data files
dur_files <- list.files(path = "data", pattern = "*.csv")

dur_files <- paste('data/', dur_files, sep = '')


#data frame to be filled
probe_total <- as_tibble()

#data fram to save names of any files that don't meet specific sort criteria
errors <- as_tibble()

#reads all of the files in the list into the loop
for (file in dur_files) {
  behave <- read_csv(file)
  probe_tmp <- filter(behave, code == "Probe/Feed")
  no_probe_tmp <- filter(behave, code == "NO FEED")
  print(file)

  #calculating values for files where there were the psyllid had more probes rows than no probes
  if (length(probe_tmp$code) > length(no_probe_tmp$code)) {
    #checks to see if the psyllid began feeding first
    start_chk <- cbind((probe_tmp$time[1] == 0))
    if (start_chk[1,] %in% TRUE) {
      file_name <- file
      probe <- filter(behave, code == "Probe/Feed")
      no_probe <- filter(behave, code == "NO FEED" | code == "END")
      row_tmp <- as_tibble(sum(no_probe$time - probe$time))
      probe_total_tmp <- cbind(file, row_tmp)
      probe_total <- rbind(probe_total, probe_total_tmp)
    }
    #if the psyllid did not begin feeding first, but the conditions match this sorting method, it saves the filename in a list of errors
    if (start_chk[1,] %in% FALSE | is.na(start_chk)) {
      errors <- rbind(errors, file)
    }
  }

  #calculating values for files with equal pairs of rows
  if (length(probe_tmp$code) == length(no_probe_tmp$code)) {
    #if the psyllid begin feeding initially, it runs this calculation
    start_chk <- cbind((probe_tmp$time[1] == 0))
    if (start_chk[1,] %in% TRUE) {
      file_name <- file
      probe <- filter(behave, code == "Probe/Feed")
      no_probe <- filter(behave, code == "NO FEED")
      row_tmp <- as_tibble(sum(no_probe$time - probe$time))
      probe_total_tmp <- cbind(file, row_tmp)
      probe_total <- rbind(probe_total, probe_total_tmp)

    }
    #otherwise it runs this one:
    if (start_chk[1,] %in% FALSE | is.na(start_chk)) {
      file_name <- file
      probe <- filter(behave, code == "Probe/Feed")
      no_probe <- filter(behave, code == "NO FEED")
      row_tmp <- as_tibble(300 - sum(probe$time - no_probe$time))
      probe_total_tmp <- cbind(file, row_tmp)
      probe_total <- rbind(probe_total, probe_total_tmp)

    }
  }

  #calculating values for files where there were the psyllid had less probes rows than no probes
  if (length(probe_tmp$code) < length(no_probe_tmp$code)) {
    start_chk <- cbind((probe_tmp$time[1] == 0))
    if (start_chk[1,] %in% TRUE) {
      errors <- rbind(errors, file)
    }
    if (start_chk[1,] %in% FALSE | is.na(start_chk)) {
      file_name <- file
      probe <- filter(behave, code == "Probe/Feed" | code == 'END')
      no_probe <- filter(behave, code == "NO FEED")
      row_tmp <- as_tibble(300 - sum(probe$time - no_probe$time))
      probe_total_tmp <- cbind(file, row_tmp)
      probe_total <- rbind(probe_total, probe_total_tmp)
    }
  }
}




####let's do it again for walk data!###
#walks data frame to be filled

walk_total <- as_tibble()

for (file in dur_files) {
  behave <- read_csv(file)
  print(file)
  walk_tmp <- filter(behave, code == "Walk")
  no_walk_tmp <- filter(behave, code == "STILL")

  if (length(walk_tmp$code) > length(no_walk_tmp$code)) {
    start_chk <- cbind((walk_tmp$time[1] == 0))
    if (start_chk[1,] %in% TRUE) {
      file_name <- file
      walk <- filter(behave, code == "Walk")
      no_walk <- filter(behave, code == "STILL" | code == "END")
      row_tmp <- as_tibble(sum(no_walk$time - walk$time))
      walk_total_tmp <- cbind(file, row_tmp)
      walk_total <- rbind(walk_total, walk_total_tmp)
    }
    if (start_chk[1,] %in% FALSE | is.na(start_chk)) {
      errors <- rbind(errors, file)
      print(errors)
    }
  }


  if (length(walk_tmp$code) == length(no_walk_tmp$code)) {
    start_chk <- cbind((walk_tmp$time[1] == 0))
    if (start_chk[1,] %in% TRUE) {
      file_name <- file
      walk <- filter(behave, code == "Walk")
      no_walk <- filter(behave, code == "STILL")
      row_tmp <- as_tibble(sum(no_walk$time - walk$time))
      walk_total_tmp <- cbind(file, row_tmp)
      walk_total <- rbind(walk_total, walk_total_tmp)

    }
    if (start_chk[1,] %in% FALSE | is.na(start_chk)) {
      file_name <- file
      walk <- filter(behave, code == "Walk")
      no_walk <- filter(behave, code == "STILL")
      row_tmp <- as_tibble(300 - sum(walk$time - no_walk$time))
      walk_total_tmp <- cbind(file, row_tmp)
      walk_total <- rbind(walk_total, walk_total_tmp)

    }
  }


  if (length(walk_tmp$code) < length(no_walk_tmp$code)) {
    start_chk <- cbind((walk_tmp$time[1] == 0))
    if (start_chk[1,] %in% TRUE) {
      errors <- rbind(errors, file)
      print(errors)
    }
    if (start_chk[1,] %in% FALSE | is.na(start_chk)) {
      file_name <- file
      walk <- filter(behave, code == "Walk" | code == 'END')
      no_walk <- filter(behave, code == "STILL")
      row_tmp <-
        as_tibble(abs(sum(walk$time - (
          lead(no_walk$time, n = 1)
        ), na.rm = T)))
      walk_total_tmp <- cbind(file, row_tmp)
      walk_total <- rbind(walk_total, walk_total_tmp)
    }
  }
}


###slightly different calculations, same process for clean data!###
#clean data frame to be filled
clean_total <- as_tibble()

for (file in dur_files) {
  behave <- read_csv(file)
  print(file)
  clean_tmp <- filter(behave, code == "Cleaning")
  no_clean_tmp <- filter(behave, code == "NO CLEAN")

  if (length(clean_tmp$code) > length(no_clean_tmp$code)) {
    start_chk <- cbind((clean_tmp$time[1] == 0))
    if (start_chk[1,] %in% TRUE) {
      file_name <- file
      clean <- filter(behave, code == "Cleaning")
      no_clean <- filter(behave, code == "NO CLEAN" | code == "END")
      row_tmp <- as_tibble(sum(no_clean$time - clean$time))
      clean_total_tmp <- cbind(file, row_tmp)
      clean_total <- rbind(clean_total, clean_total_tmp)
    }
    if (start_chk[1,] %in% FALSE | is.na(start_chk)) {
      errors <- rbind(errors, file)
      print(errors)
    }
  }


  if (length(clean_tmp$code) == length(no_clean_tmp$code)) {
    start_chk <- cbind((clean_tmp$time[1] == 0))
    if (start_chk[1,] %in% TRUE) {
      errors <- rbind(errors, file)
      print(errors)
    }
    if (start_chk[1,] %in% FALSE | is.na(start_chk)) {
      file_name <- file
      clean <- filter(behave, code == "Cleaning")
      no_clean <- filter(behave, code == "NO CLEAN")
      row_tmp <- as_tibble(300 - sum(clean$time - no_clean$time))
      clean_total_tmp <- cbind(file, row_tmp)
      clean_total <- rbind(clean_total, clean_total_tmp)
    }
  }


  if (length(clean_tmp$code) < length(no_clean_tmp$code)) {
    start_chk <- cbind((clean_tmp$time[1] == 0))
    if (start_chk[1,] %in% TRUE) {
      errors <- rbind(errors, file)
      print(errors)
    }
    if (start_chk[1,] %in% FALSE | is.na(start_chk)) {
      file_name <- file
      clean <- filter(behave, code == "Cleaning" | code == "END")
      no_clean <- filter(behave, code == "NO CLEAN")
      row_tmp <- as_tibble(300 - sum(clean$time - no_clean$time))
      clean_total_tmp <- cbind(file, row_tmp)
      clean_total <- rbind(clean_total, clean_total_tmp)
    }
  }
}



###last for off leaf times!###
#off_leaf data frame to be filled
off_leaf_total <- as_tibble()

for (file in dur_files) {
  behave <- read_csv(file)
  print(file)
  on_leaf_tmp <- filter(behave, code == "ON Leaf")
  off_leaf_tmp <- filter(behave, code == "OFF Leaf")

  if (length(on_leaf_tmp$code) > length(off_leaf_tmp$code)) {
    start_chk <- cbind((on_leaf_tmp$time[1] == 0))
    if (start_chk[1,] %in% TRUE) {
      file_name <- file
      on_leaf <- filter(behave, code == "ON Leaf")
      off_leaf <- filter(behave, code == "OFF Leaf" | code == "END")
      row_tmp <- as_tibble(300 - sum(off_leaf$time - on_leaf$time))
      off_leaf_total_tmp <- cbind(file, row_tmp)
      off_leaf_total <- rbind(off_leaf_total, off_leaf_total_tmp)
    }
    if (start_chk[1,] %in% FALSE | is.na(start_chk)) {
      errors <- rbind(errors, file)
      print(errors)
    }
  }


  if (length(on_leaf_tmp$code) == length(off_leaf_tmp$code)) {
    start_chk <- cbind((on_leaf_tmp$time[1] == 0))
    if (start_chk[1,] %in% TRUE) {
      file_name <- file
      on_leaf <- filter(behave, code == "ON Leaf")
      off_leaf <- filter(behave, code == "OFF Leaf")
      row_tmp <- as_tibble(300 - sum(off_leaf$time - on_leaf$time))
      off_leaf_total_tmp <- cbind(file, row_tmp)
      off_leaf_total <- rbind(off_leaf_total, off_leaf_total_tmp)
    }
    if (start_chk[1,] %in% FALSE | is.na(start_chk)) {
      errors <- rbind(errors, file)
      print(errors)
    }
  }

  if (length(on_leaf_tmp$code) < length(off_leaf_tmp$code)) {
    start_chk <- cbind((on_leaf_tmp$time[1] == 0))
    if (start_chk[1,] %in% TRUE) {
      errors <- rbind(errors, file)
      print(errors)
    }
    if (start_chk[1,] %in% FALSE | is.na(start_chk)) {
      errors <- rbind(errors, file)
      print(errors)
    }
  }
}


#rounding numbers
probe_total$value <- (round(probe_total$value, digits = 1))
walk_total$value <- (round(walk_total$value, digits = 1))
clean_total$value <- (round(clean_total$value, digits = 2))
off_leaf_total$value <- (round(off_leaf_total$value, digits = 2))

#making sure there are no values greater than the total recording time
probe_total$value[probe_total$value > 300] <- 300
walk_total$value[walk_total$value > 300] <- 300
clean_total$value[clean_total$value > 300] <- 300
off_leaf_total$value[off_leaf_total$value > 300] <- 300

#making sure no negative numbers (by products of milliseconds in calculations)
probe_total$value[probe_total$value < 0] <- 0
walk_total$value[walk_total$value < 0] <- 0
clean_total$value[clean_total$value < 0] <- 0
off_leaf_total$value[off_leaf_total$value < 0] <- 0

#converting to tibbles
probe_total <- as.tibble(probe_total)
walk_total <- as.tibble(walk_total)
clean_total <- as.tibble(clean_total)
off_leaf_total <- as.tibble(off_leaf_total)


#combining first datasets
ddata_tmp1 <- inner_join(probe_total, walk_total, by = "file")
names(ddata_tmp1)[names(ddata_tmp1) == "value.x"] <- "d_probes"
names(ddata_tmp1)[names(ddata_tmp1) == "value.y"] <- "d_walks"

#combining the other datasets
ddata_tmp2 <- inner_join(clean_total, off_leaf_total, by = "file")
names(ddata_tmp2)[names(ddata_tmp2) == "value.x"] <- "d_cleans"
names(ddata_tmp2)[names(ddata_tmp2) == "value.y"] <- "d_off"

#all duration data
dur_data <- inner_join(ddata_tmp1, ddata_tmp2)
dur_data$file <- gsub('data/', '', dur_data$file)

write_csv(dur_data, "results/dur_data.csv")

rm(
  ddata_tmp1,
  ddata_tmp2,
  clean,
  clean_tmp,
  no_clean,
  no_clean_tmp,
  clean_total,
  probe_total,
  clean_total_tmp,
  off_leaf,
  off_leaf_total,
  on_leaf_tmp,
  on_leaf,
  off_leaf_tmp,
  off_leaf_total_tmp,
  behave,
  walk,
  probe,
  walk_tmp,
  probe_tmp,
  walk_total,
  no_walk,
  no_probe,
  no_walk_tmp,
  no_probe_tmp,
  walk_total_tmp,
  probe_total_tmp,
  row_tmp,
  start_chk,
  dur_files,
  file_name,
  file,
  errors
)

####COMBINING DATASHEETS INTO ONE####
bhvr_data <-
  inner_join(dur_data, inc_data, by = c('file'), .after = T)
bhvr_data <-
  dplyr::select(
    bhvr_data,
    psyllid,
    sex,
    variety,
    plant_number,
    d_probes,
    d_walks,
    d_cleans,
    d_off,
    i_probes,
    i_walks,
    i_cleans,
    i_off,
    file,
    plant_size,
    mins_away
  )

write_csv(bhvr_data, "results/bhvr_data.csv")

#cleanup
rm(inc_data, dur_data)
