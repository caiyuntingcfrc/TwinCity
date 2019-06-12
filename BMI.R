
# prep and options --------------------------------------------------------

# rm
rm(list = ls())
# set working directory
setwd("~/R_wd/twin_cities/")
# list of packages
list.packages <- c("tidyverse", "magrittr", "haven", 
                   "gridExtra", "summarytools", "tabulizer", 
                   "labelled")
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

# read sav file -----------------------------------------------------------

filepath <- "3.雙城正式_全年級20180704final_han&Lung.sav"
df <- read_sav(filepath, encoding = "UTF-8", user_na = FALSE)
var_lables <- sapply(df, attr, "label")
vlu_lables <- sapply(df, attr, "labels")
rm(filepath)


# extract the BMI table ---------------------------------------------------

# read the pdf file
filepath <- "~/Downloads/衛福部_2013_兒童與青少年生長身體質量指數(BMI)建議值.pdf"
tbl_BMI <- extract_tables(file = filepath, encoding = "UTF-8") %>% 
        .[[1]] %>% 
        .[-(1:4), ]
# normal range of male children
m_low <- tbl_BMI %>% .[ , 3] %>% 
        str_split("-", simplify = TRUE) %>% .[ , 1]
m_upp <- tbl_BMI %>% .[ , 3] %>% 
        str_split("-", simplify = TRUE) %>% .[ , 2]
# normal range of female children
f_low <- tbl_BMI %>% .[ , 7] %>% 
        str_split("-", simplify = TRUE) %>% .[ , 1]
f_upp <- tbl_BMI %>% .[ , 7] %>% 
        str_split("-", simplify = TRUE) %>% .[ , 2] %>% 
        str_split("\\s", simplify = TRUE) %>% .[ , 1]
# combine the vectors
tbl_BMI <- cbind(tbl_BMI[ , 1], m_low, m_upp, tbl_BMI[ , 5], 
                 f_low, f_upp, tbl_BMI[ , 8]) %>% 
        as_tibble(.name_repair = NULL) %>% 
        mutate_if(is.character, as.numeric) %>% 
        # filter age between 6, 13.5
        filter(between(V1, 6, 14))
names(tbl_BMI) <- c("age", "m_low", "m_upp", "m_ovr", 
                    "f_low", "f_upp", "f_ovr")


# calculate BMIs ----------------------------------------------------------

df <- df %>% mutate(BMI = w_kg / ((h_cm / 100) ** 2), 
                    ovrw = NA)


# for loop ----------------------------------------------------------------

for(i in 2:nrow(tbl_BMI)) {
        # male children
        for(j in 1:nrow(df)) {
                b <- !is.na(df$BMI[j]) & !is.na(df$realage[j]) & !is.na(df$Bd23[j])
                # sex == 2
                s <- df$Bd23[j] == 2
                # age
                m <- df$realage[j] < tbl_BMI$age[i] & df$realage[j] >= tbl_BMI$age[i - 1]
                # below average
                c1 <- df$BMI[j] < tbl_BMI$m_low[i - 1]
                # normal
                c2 <- df$BMI[j] > tbl_BMI$m_low[i - 1] & df$BMI[j] < tbl_BMI$m_upp[i - 1]
                # overweight
                c3 <- df$BMI[j] >= tbl_BMI$m_upp[i - 1] & df$BMI[j] < tbl_BMI$m_ovr[i - 1]
                # more than overweight
                c4 <- df$BMI[j] >= tbl_BMI$m_ovr[i - 1]
                if(b & s & m & c1) { df$ovrw[j] <- 1 } 
                if(b & s & m & c2) { df$ovrw[j] <- 2 } 
                if(b & s & m & c3) { df$ovrw[j] <- 3 } 
                if(b & s & m & c4) { df$ovrw[j] <- 4 } 
                }
        # female children
        for(j in 1:nrow(df)) {
                b <- !is.na(df$BMI[j]) & !is.na(df$realage[j]) & !is.na(df$Bd23[j])
                s <- df$Bd23[j] == 1
                f <- df$realage[j] < tbl_BMI$age[i] & df$realage[j] >= tbl_BMI$age[i - 1]
                c1 <- df$BMI[j] < tbl_BMI$f_low[i - 1]
                c2 <- df$BMI[j] > tbl_BMI$f_low[i - 1] & df$BMI[j] < tbl_BMI$f_upp[i - 1]
                c3 <- df$BMI[j] >= tbl_BMI$f_upp[i - 1] & df$BMI[j] < tbl_BMI$f_ovr[i - 1]
                c4 <- df$BMI[j] >= tbl_BMI$f_ovr[i - 1]
                if(b & s & f & c1) { df$ovrw[j] <- 1 } 
                if(b & s & f & c2) { df$ovrw[j] <- 2 } 
                if(b & s & f & c3) { df$ovrw[j] <- 3 } 
                if(b & s & f & c4) { df$ovrw[j] <- 4 } 
                }
        }


# save file ---------------------------------------------------------------
write_sav(df, "~/Desktop/tp_all.sav", compress = FALSE)
read_sav("~/Desktop/tp_all.sav", encoding = "UTF-8")
