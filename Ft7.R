
# Prep --------------------------------------------------------------------

# clean global evironment
rm(list = ls())
# clean console
cat("\014")
#
setwd("D:/R_wd/Twin Cities/")
# list of packages
list.packages <- c("tidyverse", "magrittr", "haven")
# check if the packages are installed
new.packages <- list.packages[!(list.packages %in% installed.packages()[ , "Package"])]
# install new packages
if(length(new.packages)) install.packages(new.packages)
# load the packages
lapply(list.packages, require, character.only = TRUE)
# remove lists
rm(list.packages, new.packages)
# options
options(scipen = 999)


# read file ---------------------------------------------------------------

df <- read_sav("working/3.雙城正式_全年級20180704final_han&Lung.sav", 
               encoding = "UTF-8", 
               user_na = FALSE)

# Sum ---------------------------------------------------------------------

l <- grep("^Ft7_[0-9]|Ft7_[1][1-3]|^rev_Ft7_", names(df), value = TRUE) %>% 
        .[-c(1, 4, 9)]; l
d <- df %>% 
        rename(Ft7_S = Ft7_sum) %>% 
        mutate(Ft7_sum = rowSums(.[l], na.rm = FALSE)) %>% 
        select(Ft7_sum) %>% 
        replace(., is.na(.), 99L)

# save file ---------------------------------------------------------------

write_sav(d, "d_Ft7.sav", compress = FALSE)
