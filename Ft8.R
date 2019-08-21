
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
df <- read_sav("working/3.雙城正式_全年級20180704final_han&Lung.sav", 
               encoding = "UTF-8", 
               user_na = FALSE)

# delete Ch8_


# 1. achievment -----------------------------------------------------------
l1 <- grep("Ft8_6|Ft8_14|Ft8_18|Ft8_23|Ft8_26|Ft8_29|Ft8_30|Ft8_31|Ft8_32|Ft8_33|Ft8_34", 
           names(df), value = FALSE)
# # d0 complete cases
# d0 <- df %>% 
#         filter(complete.cases(.[l1])) %>% 
#         mutate(Ft8_Achievement_sum = rowSums(.[l1], na.rm = TRUE))

# d1 with na.rm = FALSE
d1 <- df %>% 
        mutate(Ft8_achievement_sum = rowSums(.[l1], na.rm = FALSE)) %>% 
        select("Ft8_achievement_sum") %>% 
        replace(., is.na(.), 99L)

# 2. intimacy -------------------------------------------------------------

l2 <- grep("Ft8_1$|Ft8_2$|Ft8_5$|Ft8_9$|Ft8_21$", 
           names(df), value = FALSE)
d2 <- df %>% 
        mutate(Ft8_warmth_sum = rowSums(.[l2], na.rm = FALSE)) %>% 
        select("Ft8_warmth_sum") %>% 
        replace(., is.na(.), 99L)

# 3. monitor --------------------------------------------------------------

l3 <- grep("Ft8_13$|Ft8_15$|Ft8_22$|Ft8_24$", names(df), value = FALSE)
d3 <- df %>% 
        mutate(Ft8_monitoring_sum = rowSums(.[l3], na.rm = FALSE)) %>% 
        select("Ft8_monitoring_sum") %>% 
        replace(., is.na(.), 99L)

# 4. Harsh ----------------------------------------------------------------

l4 <- grep("Ft8_4$|Ft8_8$|Ft8_11$|Ft8_16$", names(df), value = FALSE)
d4 <- df %>% 
        mutate(Ft8_harsh_sum = rowSums(.[l4], na.rm = FALSE)) %>% 
        select("Ft8_harsh_sum") %>% 
        replace(., is.na(.), 99L)

# 5. Indulgence -----------------------------------------------------------

l5 <- grep("Ft8_17$|Ft8_20$|Ft8_25$|Ft8_28$", names(df), value = FALSE)
d5 <- df %>% 
        mutate(Ft8_indulgence_sum = rowSums(.[l5], na.rm = FALSE)) %>% 
        select("Ft8_indulgence_sum") %>% 
        replace(., is.na(.), 99L)

# 6. Autonomy -------------------------------------------------------------

l6 <- grep("Ft8_3$|Ft8_7$|Ft8_10$|Ft8_12$|Ft8_19$|Ft8_27$", names(df), value = FALSE)
d6 <- df %>% 
        mutate(Ft8_autonomy_sum = rowSums(.[l6], na.rm = FALSE)) %>% 
        select("Ft8_autonomy_sum") %>% 
        replace(., is.na(.), 99L)

# combine -----------------------------------------------------------------

d <- dplyr::bind_cols(d1, d2, d3, d4, d5, d6)
d %>% replace(., d == 99, NA_real_) %>% summary()

# save as .sav ------------------------------------------------------------

write_sav(d, "d_Ft8.sav", compress = FALSE)
