
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

# df
df <- read_sav("working/3.雙城正式_全年級20180704final_han&Lung.sav", user_na = FALSE)

# rename
df <- df %>% 
        rename(Ch4_attack = Ch4_attack_sum, 
               Ch4_soc = Ch4_soc_sum, 
               Ch4_attention = Ch4_attention_sum,
               Ch4_withdrawn = Ch4_withdrawn_sum)

# 1. attack ---------------------------------------------------------------

l1 <- grep("Ch4_1$|Ch4_3$|Ch4_9$|Ch4_11$|Ch4_12$|Ch4_16$|^Ch4_[2][2, 4, 7-9]$|^Ch4_[3][0-2, 4]", 
           names(df), value = TRUE)
d1 <- df %>% 
        mutate(Ch4_attack_sum = rowSums(.[l1], na.rm = FALSE)) %>% 
        select(Ch4_attack_sum) %>%
        replace(., is.na(.), 99L)


# 2. social ---------------------------------------------------------------

l2 <- grep("_Ch4_2$|_Ch4_[6-8]$|_Ch4_[1][0, 3-4]$|^Ch4_17$|^Ch4_21$|_Ch4_25$", 
           names(df), value = TRUE)
d2 <- df %>% 
        mutate(Ch4_soc_sum = rowSums(.[l2], na.rm = FALSE)) %>% 
        select(Ch4_soc_sum) %>% 
        replace(., is.na(.), 99L)


# 3. attention ------------------------------------------------------------

l3 <- grep("^Ch4_[4-5]$|_Ch4_15$|Ch4_18$|Ch4_20$", names(df), value = TRUE)        
d3 <- df %>% 
        mutate(Ch4_attention_sum = rowSums(.[l3], na.rm = FALSE)) %>% 
        select(Ch4_attention_sum) %>% 
        replace(., is.na(.), 99L)

# 4. Withdran -------------------------------------------------------------

l4 <- grep("Ch4_19$|Ch4_23$|Ch4_26$|Ch4_33$|Ch4_35", names(df), value = TRUE)
d4 <- df %>% 
        mutate(Ch4_withdrawn_sum = rowSums(.[l4], na.rm = FALSE)) %>% 
        select(Ch4_withdrawn_sum) %>% 
        replace(., is.na(.), 99L)


# combine -----------------------------------------------------------------

d <- bind_cols(d1, d2, d3, d4)
write_sav(d, "d_Ch4.sav", compress = FALSE)