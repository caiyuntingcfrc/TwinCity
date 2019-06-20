
# Prep and Options --------------------------------------------------------

# rm
rm(list = ls())
# set working directory
setwd("D:/R_wd/Twin Cities/")
# list of packages
list.packages <- c("tidyverse", "magrittr", "haven", 
                   "gridExtra", "summarytools", "tabulizer", 
                   "labelled", "DescTools", "userfriendlyscience", 
                   "stargazer", "sjPlot")
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
# options for summarytools
st_options(style = "simple",
           round.digits = 4,
           ctable.prop = "c")

# df
df <- read_sav("11. Taipei_all.sav")
d <- df %>% filter(Ch3 == 1 & !is.na(Ch3說明)) %>% 
        select("SID", "Ch3", "Ch3說明")
write_excel_csv(d, "illness.csv")
new <- read.csv("illness.csv", encoding = "CP950")


df <- df %>% select(-"illness")
df$SID <- as.character(df$SID)
new$SID <- as.character(new$SID)
new <- new %>% select(-c("Ch3", "Ch3說明"))
dd <- left_join(df, new, by = "SID")
ddd <- dd %>% filter(Ch3)

write_sav(dd, "12.Taipei_all.sav")
