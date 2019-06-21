
# prep and options --------------------------------------------------------

# rm
rm(list = ls())
# set working directory
setwd("D:/R_wd/Twin Cities/")
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

# data from Shanghai
filepath <- "1.上海_2014r2.sav"
df <- read_spss(filepath)
rm(filepath)

# compute realage


# extract the bmi table ---------------------------------------------------

# read the pdf file
filepath <- "bmi/衛福部_2013_兒童與青少年生長身體質量指數(bmi)建議值.pdf"
tbl_bmi <- extract_tables(file = filepath, encoding = "UTF-8") %>% 
        .[[1]] %>% 
        .[-(1:4), ]
# normal range of male children
m_low <- tbl_bmi %>% .[ , 3] %>% 
        str_split("-", simplify = TRUE) %>% .[ , 1]
m_upp <- tbl_bmi %>% .[ , 3] %>% 
        str_split("-", simplify = TRUE) %>% .[ , 2]
# normal range of female children
f_low <- tbl_bmi %>% .[ , 7] %>% 
        str_split("-", simplify = TRUE) %>% .[ , 1]
f_upp <- tbl_bmi %>% .[ , 7] %>% 
        str_split("-", simplify = TRUE) %>% .[ , 2] %>% 
        str_split("\\s", simplify = TRUE) %>% .[ , 1]
# combine the vectors
tbl_bmi <- cbind(tbl_bmi[ , 1], m_low, m_upp, tbl_bmi[ , 5], 
                 f_low, f_upp, tbl_bmi[ , 8]) %>% 
        as_tibble(.name_repair = NULL) %>% 
        mutate_if(is.character, as.numeric) %>% 
        # filter age between 6, 13.5
        filter(between(V1, 6, 14))
names(tbl_bmi) <- c("age", "m_low", "m_upp", "m_ovr", 
                    "f_low", "f_upp", "f_ovr")


# calculate bmis ----------------------------------------------------------

df <- df %>% mutate(bmi = w_kg / ((h_cm / 100) ** 2), 
                    bmicat = NA)


# for loop ----------------------------------------------------------------

for(i in 2:nrow(tbl_bmi)) {
        # male children
        for(j in 1:nrow(df)) {
                b <- !is.na(df$bmi[j]) & !is.na(df$realage[j]) & !is.na(df$Bd23[j])
                # sex == 2
                s <- df$Bd23[j] == 2
                # age
                m <- df$realage[j] < tbl_bmi$age[i] & df$realage[j] >= tbl_bmi$age[i - 1]
                # below average
                c1 <- df$bmi[j] < tbl_bmi$m_low[i - 1]
                # normal
                c2 <- df$bmi[j] > tbl_bmi$m_low[i - 1] & df$bmi[j] < tbl_bmi$m_upp[i - 1]
                # overweight
                c3 <- df$bmi[j] >= tbl_bmi$m_upp[i - 1] & df$bmi[j] < tbl_bmi$m_ovr[i - 1]
                # more than overweight
                c4 <- df$bmi[j] >= tbl_bmi$m_ovr[i - 1]
                if(b & s & m & c1) { df$bmicat[j] <- 1 } 
                if(b & s & m & c2) { df$bmicat[j] <- 2 } 
                if(b & s & m & c3) { df$bmicat[j] <- 3 } 
                if(b & s & m & c4) { df$bmicat[j] <- 4 } 
                }
        # female children
        for(j in 1:nrow(df)) {
                b <- !is.na(df$bmi[j]) & !is.na(df$realage[j]) & !is.na(df$Bd23[j])
                s <- df$Bd23[j] == 1
                f <- df$realage[j] < tbl_bmi$age[i] & df$realage[j] >= tbl_bmi$age[i - 1]
                c1 <- df$bmi[j] < tbl_bmi$f_low[i - 1]
                c2 <- df$bmi[j] > tbl_bmi$f_low[i - 1] & df$bmi[j] < tbl_bmi$f_upp[i - 1]
                c3 <- df$bmi[j] >= tbl_bmi$f_upp[i - 1] & df$bmi[j] < tbl_bmi$f_ovr[i - 1]
                c4 <- df$bmi[j] >= tbl_bmi$f_ovr[i - 1]
                if(b & s & f & c1) { df$bmicat[j] <- 1 } 
                if(b & s & f & c2) { df$bmicat[j] <- 2 } 
                if(b & s & f & c3) { df$bmicat[j] <- 3 } 
                if(b & s & f & c4) { df$bmicat[j] <- 4 } 
                }
        }


# Attributes --------------------------------------------------------------
# bmi
attr(df$bmi, "label") <- "Body Mass Index scores"
# bmi cat
df$bmicat <- labelled(df$bmicat, c("underweight" = 1, 
                                   "normal weight" = 2,
                                   "overweight" = 3, 
                                   "obese" = 4), 
                      label = "Body Mass Index categories")
attr(df$bmicat, "format.spss") <- "F8.2"

# save file ---------------------------------------------------------------
Taipei_all <- df
write_sav(df, "Taipei_all.sav", compress = FALSE)