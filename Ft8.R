
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
# df <- df %>% 
#         select(-contains("Ch8_"))

# 1. achievment -----------------------------------------------------------
l1 <- grep("Ft8_6|Ft8_14|Ft8_18|Ft8_23|Ft8_26|Ft8_29|Ft8_30|Ft8_31|Ft8_32|Ft8_33|Ft8_34", 
           names(df), value = FALSE)
# # d0 complete cases
# d0 <- df %>% 
#         filter(complete.cases(.[l1])) %>% 
#         mutate(Ft8_Achievement_sum = rowSums(.[l1], na.rm = TRUE))

# d1 with na.rm = FALSE
df <- df %>% 
        mutate(Ft8_Achievement_sum = rowSums(.[l1], na.rm = FALSE))
sum(is.na(df$Ft8_Achievement_sum))


# 2. intimacy -------------------------------------------------------------
