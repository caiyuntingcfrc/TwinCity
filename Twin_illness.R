
# prep and options --------------------------------------------------------

# set working directory
setwd("D:/R_wd/")
# clear objects
rm(list = ls())
# load the packages
l <- c("tidyverse", "DescTools", "ggplot2", 
       "car", "userfriendlyscience", "summarytools", 
       "magrittr", "haven", "gridExtra", 
       "readxl", "sjPlot")
lapply(l, require, character.only= TRUE)
rm(l)
# options
options(scipen = 999)
# options for summarytools
st_options(style = "simple",
           round.digits = 4,
           ctable.prop = "c")

# import excel file -------------------------------------------------------

xlsx_path <- "misc/疾病狀況_疾病說明new_rev.xlsx"
re <- read_xlsx(xlsx_path, sheet = 5, range = "A2:D91", col_names = FALSE)
colnames(re) <- c("health_related", "illness", "major_illness", "others")


# read sav file -----------------------------------------------------------

sav_path <- "Twin Cities/Taipei_all.sav"
df <- read_sav(sav_path, encoding = "UTF-8")


# recode ------------------------------------------------------------------

df <- df %>% mutate(illness = case_when(Ch3 == 1 & Ch3說明 %in% re$health_related ~ 1, 
                                        Ch3 == 1 & Ch3說明 %in% re$illness ~ 2,
                                        Ch3 == 1 & Ch3說明 %in% re$major_illness ~ 3, 
                                        Ch3 == 1 & Ch3說明 %in% re$others ~ 4, 
                                        TRUE ~ 99))
# attributes
attr(df$illness, "label") <- "身心狀況分類"
attr(df$illness, "labels") <- c(1:4, 99)
names(attr(df$illness, "labels")) <- c("健康問題", "疾病", "重大疾病", "無法歸類")

# frequencies -------------------------------------------------------------
# plot
sjt.xtab(df$illness, 
         df$ovrw, 
         encoding = "UTF-8", digits = 2, 
         title = "雙城大台北地區 — 身心狀況分類 X BMI標準", 
         show.cell.prc = TRUE, show.legend = TRUE, 
         string.total = "總和", emph.total = TRUE, show.na = FALSE)
a <- df %>% filter(illness == 99 & Ch3 == 1)
# test --------------------------------------------------------------------

