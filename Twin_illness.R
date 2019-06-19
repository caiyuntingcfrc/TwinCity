
# Prep and Options --------------------------------------------------------

# rm
rm(list = ls())
# set working directory
setwd("D:/R_wd/")

# list of packages
list.packages <- c("tidyverse", "magrittr", "haven", "readxl",
                   "gridExtra", "summarytools", "tabulizer", 
                   "labelled", "DescTools", "userfriendlyscience", "stargazer", 
                   "sjlabelled", "sjmisc", "sjstats", "ggeffects", "sjPlot")
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

# import excel file -------------------------------------------------------

xlsx_path <- "misc/疾病狀況_疾病說明new_rev.xlsx"
re <- read_xlsx(xlsx_path, sheet = 5, range = "A2:D91", col_names = FALSE)
colnames(re) <- c("health_related", "illness", "major_illness", "others")


# read sav file -----------------------------------------------------------

sav_path <- "Twin Cities/11. Taipei_all.sav"
df <- read_sav(sav_path, encoding = "UTF-8")


# recode ------------------------------------------------------------------

df <- df %>% mutate(illness = case_when(Ch3 == 1 & Ch3說明 %in% re$health_related ~ 1, 
                                        Ch3 == 1 & Ch3說明 %in% re$illness ~ 2,
                                        Ch3 == 1 & Ch3說明 %in% re$major_illness ~ 3, 
                                        Ch3 == 1 & Ch3說明 %in% re$others ~ 4, 
                                        is.na(Ch3) | is.na(Ch3說明) ~ NA_real_))
# attributes
df$illness <- labelled(df$illness, c("健康問題" = 1, 
                                     "疾病" = 2, 
                                     "重大疾病" = 3, 
                                     "無法歸類" = 4), 
                       label = "身心狀況分類")
attr(df$illness, "format.spss") <- "F8.2"


# save the file -----------------------------------------------------------

write_sav(df, "Twin Cities/Taipei_all.sav")

# Desc --------------------------------------------------------------------

# illness X grade
sjt.xtab(var.row = df$illness, var.col = df$grade, 
         var.labels = c("身心狀況分類", "年級"), 
         title = "身心狀況分類 X 年級", 
         string.total = "總和", 
         show.cell.prc = TRUE, show.row.prc = TRUE, show.col.prc = TRUE,
         show.summary = FALSE, show.legend = TRUE, 
         emph.total = TRUE,
         digits = 2,
         encoding = "UTF-8")
sjp.xtab(x = df$grade, grp = df$illness, 
         title = "身心狀況分類 X 年級", 
         show.total = TRUE, show.n = TRUE, show.prc = TRUE) + theme_sjplot()

# illness X sex
sjt.xtab(var.row = df$illness, var.col = df$Bd23, 
         var.labels = c("身心狀況分類", "性別"), 
         title = "身心狀況分類 X 性別", 
         string.total = "總和", 
         show.cell.prc = TRUE, show.row.prc = TRUE, show.col.prc = TRUE,
         show.summary = FALSE, show.legend = TRUE, 
         emph.total = TRUE,
         digits = 2,
         encoding = "UTF-8")
sjp.xtab(x = df$illness, grp = df$Bd23, 
         title = "身心狀況分類 X 性別", 
         show.total = TRUE, show.n = TRUE, show.prc = TRUE) + theme_sjplot()


# illness X bmicat
sjt.xtab(df$illness, 
         df$bmicat, 
         encoding = "UTF-8", digits = 2, 
         title = "雙城大台北地區 — 身心狀況分類 X BMI標準", 
         show.cell.prc = TRUE, show.row.prc = TRUE, show.col.prc = TRUE,
         show.legend = TRUE, 
         string.total = "總和", emph.total = TRUE, show.na = FALSE)

# test --------------------------------------------------------------------

