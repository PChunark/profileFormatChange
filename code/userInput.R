#!/usr/bin/env -S Rscript --vanilla 

# name: Users fill the path, start year and end year

# input: User Input
# Output: the varaible will fill in the function in the formatChange.R

library(tidyverse)

# User's Input
path <- "rawdata/GenerationProfile_PF.xlsx"
startyear <- "2019-01-01 00:00:00"
endyear <- "2019-12-31 23:30:00"

