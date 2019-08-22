
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

# sum ---------------------------------------------------------------------

l <- grep("^Bd43_", names(df), value = TRUE) %>% 
        .[-7]
d <- df %>% 
        rename(Bd43_S = Bd43_sum) %>% 
        mutate(Bd43_sum = rowSums(.[l], na.rm = FALSE)) %>% 
        select(Bd43_sum) %>% 
        replace(., is.na(.), 99L)
summary(d)

# save file ---------------------------------------------------------------

write_sav(d, "d_Bd43.sav", compress = FALSE)
