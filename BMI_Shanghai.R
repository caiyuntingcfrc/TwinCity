
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
        mutate_if(is.character, as.numeric)
# filter age between 6, 13.5
# filter(between(V1, 6, 14))
names(tbl_bmi) <- c("age", "m_low", "m_upp", "m_ovr", 
                    "f_low", "f_upp", "f_ovr")


# Shanghai 2014 -----------------------------------------------------------

# data from Shanghai
filepath <- "1.上海_2014r2.sav"
df <- read_spss(filepath)
rm(filepath)

# compute realage
Birthdyear <- df$Birthyear
Birthdmonth <- df$Birthmonth
Birthdate <- df$Birthdate
Age <- df$Age

# 2014 march
S_year <- 2014.25
d <- df %>% mutate( yr = Birthdyear + (((Birthdmonth - 1) / 12)),
                    realage = S_year - yr)
# d$realage[is.na(d$realage)] <- d$Age[is.na(d$realage)]


# calculate bmis ----------------------------------------------------------

d <- d %>% mutate(bmicat_tp = NA)


# for loop ----------------------------------------------------------------

for(i in 2:nrow(tbl_bmi)) {
        # male children
        for(j in 1:nrow(d)) {
                b <- !is.na(d$bmi[j]) & !is.na(d$realage[j]) & !is.na(d$Gender[j])
                # sex == 1
                s <- d$Gender[j] == 1
                # age
                m <- d$realage[j] < tbl_bmi$age[i] & d$realage[j] >= tbl_bmi$age[i - 1]
                # below average
                c1 <- d$bmi[j] < tbl_bmi$m_low[i - 1]
                # normal
                c2 <- d$bmi[j] > tbl_bmi$m_low[i - 1] & d$bmi[j] < tbl_bmi$m_upp[i - 1]
                # overweight
                c3 <- d$bmi[j] >= tbl_bmi$m_upp[i - 1] & d$bmi[j] < tbl_bmi$m_ovr[i - 1]
                # more than overweight
                c4 <- d$bmi[j] >= tbl_bmi$m_ovr[i - 1]
                if(b & s & m & c1) { d$bmicat_tp[j] <- 1 } 
                if(b & s & m & c2) { d$bmicat_tp[j] <- 2 } 
                if(b & s & m & c3) { d$bmicat_tp[j] <- 3 } 
                if(b & s & m & c4) { d$bmicat_tp[j] <- 4 } 
        }
        # female children
        for(j in 1:nrow(df)) {
                b <- !is.na(d$bmi[j]) & !is.na(d$realage[j]) & !is.na(d$Gender[j])
                # sex == 0
                s <- d$Gender[j] == 0
                f <- d$realage[j] < tbl_bmi$age[i] & d$realage[j] >= tbl_bmi$age[i - 1]
                c1 <- d$bmi[j] < tbl_bmi$f_low[i - 1]
                c2 <- d$bmi[j] > tbl_bmi$f_low[i - 1] & d$bmi[j] < tbl_bmi$f_upp[i - 1]
                c3 <- d$bmi[j] >= tbl_bmi$f_upp[i - 1] & d$bmi[j] < tbl_bmi$f_ovr[i - 1]
                c4 <- d$bmi[j] >= tbl_bmi$f_ovr[i - 1]
                if(b & s & f & c1) { d$bmicat_tp[j] <- 1 } 
                if(b & s & f & c2) { d$bmicat_tp[j] <- 2 } 
                if(b & s & f & c3) { d$bmicat_tp[j] <- 3 } 
                if(b & s & f & c4) { d$bmicat_tp[j] <- 4 } 
        }
}


# attributes --------------------------------------------------------------

# bmi cat
d$bmicat_tp <- labelled(d$bmicat_tp, c("過輕" = 1, 
                                       "正常範圍" = 2,
                                       "過重" = 3, 
                                       "肥胖" = 4), 
                        label = "身體質量指數分類")
attr(d$bmicat_tp, "format.spss") <- "F8.2"

# save file ---------------------------------------------------------------
Taipei_all <- d
write_sav(Taipei_all, "Shanghai_2014.sav", compress = FALSE)


# Shanghai 2016 -----------------------------------------------------------
# data from Shanghai
filepath <- "2.Children2016.sav"
df <- read_spss(filepath)
rm(filepath)

# compute realage
Birthdyear <- df$Birthyear
Birthdmonth <- df$Birthmonth
Birthdate <- df$Birthdate
Age <- df$Age

# 2016 march
S_year <- 2016.25
d <- df %>% mutate( yr = Birthdyear + (((Birthdmonth - 1) / 12)),
                    realage = S_year - yr)


# calculate bmi -----------------------------------------------------------

d <- d %>% mutate(bmi = cweight / ((cheight / 100) ** 2 ), 
                  bmicat_tp = NA)
attr(d$bmi, "label") <- "身體質量指數"

# for loop ----------------------------------------------------------------

for(i in 2:nrow(tbl_bmi)) {
        # male children
        for(j in 1:nrow(d)) {
                b <- !is.na(d$bmi[j]) & !is.na(d$realage[j]) & !is.na(d$Gender[j])
                # sex == 1
                s <- d$Gender[j] == 1
                # age
                m <- d$realage[j] < tbl_bmi$age[i] & d$realage[j] >= tbl_bmi$age[i - 1]
                # below average
                c1 <- d$bmi[j] < tbl_bmi$m_low[i - 1]
                # normal
                c2 <- d$bmi[j] > tbl_bmi$m_low[i - 1] & d$bmi[j] < tbl_bmi$m_upp[i - 1]
                # overweight
                c3 <- d$bmi[j] >= tbl_bmi$m_upp[i - 1] & d$bmi[j] < tbl_bmi$m_ovr[i - 1]
                # more than overweight
                c4 <- d$bmi[j] >= tbl_bmi$m_ovr[i - 1]
                if(b & s & m & c1) { d$bmicat_tp[j] <- 1 } 
                if(b & s & m & c2) { d$bmicat_tp[j] <- 2 } 
                if(b & s & m & c3) { d$bmicat_tp[j] <- 3 } 
                if(b & s & m & c4) { d$bmicat_tp[j] <- 4 } 
        }
        # female children
        for(j in 1:nrow(df)) {
                b <- !is.na(d$bmi[j]) & !is.na(d$realage[j]) & !is.na(d$Gender[j])
                # sex == 0
                s <- d$Gender[j] == 0
                f <- d$realage[j] < tbl_bmi$age[i] & d$realage[j] >= tbl_bmi$age[i - 1]
                c1 <- d$bmi[j] < tbl_bmi$f_low[i - 1]
                c2 <- d$bmi[j] > tbl_bmi$f_low[i - 1] & d$bmi[j] < tbl_bmi$f_upp[i - 1]
                c3 <- d$bmi[j] >= tbl_bmi$f_upp[i - 1] & d$bmi[j] < tbl_bmi$f_ovr[i - 1]
                c4 <- d$bmi[j] >= tbl_bmi$f_ovr[i - 1]
                if(b & s & f & c1) { d$bmicat_tp[j] <- 1 } 
                if(b & s & f & c2) { d$bmicat_tp[j] <- 2 } 
                if(b & s & f & c3) { d$bmicat_tp[j] <- 3 } 
                if(b & s & f & c4) { d$bmicat_tp[j] <- 4 } 
        }
}


# attributes --------------------------------------------------------------

# bmi cat
d$bmicat_tp <- labelled(d$bmicat_tp, c("過輕" = 1, 
                                       "正常範圍" = 2,
                                       "過重" = 3, 
                                       "肥胖" = 4), 
                        label = "身體質量指數分類")
attr(d$bmicat_tp, "format.spss") <- "F8.2"

# save file ---------------------------------------------------------------
write_sav(d, "Shanghai_2016.sav", compress = FALSE)
