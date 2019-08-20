
# Prep --------------------------------------------------------------------

# clean global evironment
rm(list = ls())
# clean console
cat("\014")
#
setwd("D:/R_wd/Twin Cities/")
# list of packages
list.packages <- c("tidyverse", "magrittr", "haven", 
                   "gridExtra", "tabulizer", "knitr",
                   "labelled", "DescTools", "ggplot2", 
                   "stargazer", "sjPlot", "arsenal", 
                   "questionr", "descr", "PerformanceAnalytics", 
                   "car", "userfriendlyscience", "mice")
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
df <- read_sav("11. Taipei_all.sav")

# delete Ch8_


# 1. achievment -----------------------------------------------------------
l1 <- grep("Ft8_6|Ft8_14|Ft8_18|Ft8_23|Ft8_26|Ft8_29|Ft8_30|Ft8_31|Ft8_32|Ft8_33|Ft8_34", 
           names(df), value = FALSE)
# # d0 complete cases
# d0 <- df %>% 
#         filter(complete.cases(.[l1])) %>% 
#         mutate(Ft8_Achievement_sum = rowSums(.[l1], na.rm = TRUE))

# d1 with na.rm = FALSE
df <- df %>% 
        mutate(Ft8_achievement_sum = rowSums(.[l1], na.rm = FALSE))
sum(is.na(df$Ft8_achievement_sum))


# 2. intimacy -------------------------------------------------------------

l2 <- grep("Ft8_1$|Ft8_2$|Ft8_5$|Ft8_9$|Ft8_21$", 
           names(df), value = FALSE)
df <- df %>% 
        mutate(Ft8_warmth_sum = rowSums(.[l2], na.rm = FALSE))
sum(is.na(df$Ft8_warmth_sum))


# 3. monitor --------------------------------------------------------------

l3 <- grep("Ft8_13$|Ft8_15$|Ft8_22$|Ft8_24$", names(df), value = FALSE)
df <- df %>% 
        mutate(Ft8_monitoring_sum = rowSums(.[l3], na.rm = FALSE))
sum(is.na(df$Ft8_monitoring_sum))

# 4. Harsh ----------------------------------------------------------------

l4 <- grep("Ft8_4$|Ft8_8$|Ft8_11$|Ft8_16$", names(df), value = FALSE)
df <- df %>% 
        mutate(Ft8_harsh_sum = rowSums(.[l4], na.rm = FALSE))
sum(is.na(df$Ft8_harsh_sum))

# 5. Indulgence -----------------------------------------------------------

l5 <- grep("Ft8_17$|Ft8_20$|Ft8_25$|Ft8_28$", names(df), value = FALSE)
df <- df %>% 
        mutate(Ft8_indulgence_sum = rowSums(.[l5], na.rm = FALSE))
sum(is.na(df$Ft8_indulgence_sum)) 

# 6. Autonomy -------------------------------------------------------------

l6 <- grep("Ft8_3$|Ft8_7$|Ft8_10$|Ft8_12$|Ft8_19$|Ft8_27$", names(df), value = FALSE)
l6 <- grep("Ft8_3$|Ft8_7$|Ft8_10$|Ft8_12$|Ft8_19$|Ft8_27$", names(df), value = TRUE)
df <- df %>% 
        mutate(Ft8_autonomy_sum = rowSums(.[l6], na.rm = TRUE))
sum(is.na(df$Ft8_autonomy_sum))


# remove Ch8_ -------------------------------------------------------------

df <- df %>% 
        select(- contains("Ch8_"))

# save as .sav ------------------------------------------------------------

write_sav(df, "temp_Taipei_all.sav", compress = FALSE)
