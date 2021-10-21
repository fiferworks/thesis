This is Austin Nathaniel Fife's Thesis presented for a MSC
at the University of Idaho in 2018.

This folder contains the files necessary for statistical analyses.
All data processing is done in R, the required packages are shown in the
scripts. The names and files in 'data' and 'scripts' folders are important,
so don't rename, delete, edit, etc. these files if you want to recreate
the data analysis I did.

--'Fife_Thesis'
Open this file with RStudio first, then open 'Psyllid_Analysis.R'

--'Psyllid_Analysis.R'
to be opened with RStudio and run to recreate my entire statistical analysis

-- 'data'
contains the output files from the CowLog behavioral recording software.
Naming structure is 'genotype_plant#_test#.csv' for these files.
'dummy' is a file necessary for the R scripts, don't delete it.
'Austin_s Fecundity Data.xlsx' and 'Jeff_s Fecundity Data.xlsx' are the
master datasheets from which both 'fecundity_master.xlsx' and
'no_choice_master' are derived. 'fecundity_master.xlsx' and
'no_choice_master' are cleaned files used by the scripts,
'Austin_s Fecundity Data.xlsx' and 'Jeff_s Fecundity Data.xlsx' are not.

-- 'figures'
this is an output file for the graphs made in R.
running the scripts again will overwrite and update them if you want to test
different parameters/edit data files

--'results'
this is the folder where R outputs are recorded from the data analysis.
This is where the data in my tables &etc comes from

--'scripts'
contains the scripts used in 'Psyllid_Analysis.R' which recreate my analysis.

-- 'R.proj.user', '_Rhistory', '_RData' and desktop
Files created&/used by R. They contain no user-servicible parts.