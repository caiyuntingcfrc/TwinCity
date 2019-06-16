##### BMI Shanghai #####

# Prep --------------------------------------------------------------------

# set working directory
setwd("~/Documents/Rdata/twin cities/")
# loading packages
# expss must be loaded after haven
list.packages <- c("tidyverse", "magrittr", "haven", 
                   "gridExtra", "summarytools", "tabulizer", 
                   "labelled")
# check if the packages are installed
l <- !(list.packages %in% installed.packages()[ , "Package"])
new.packages <- list.packages[l]
# install new packages
if(length(new.packages)) install.packages(new.packages)
# load the packages
lapply(list.packages, require, character.only = TRUE)
# remove lists
rm(l, list.packages, new.packages)

# options
options(readr.show_progress = TRUE)
# do not show scientific notation
options(scipen = 999)


# Import the file ---------------------------------------------------------

filepath <- "資料檔/1.上海_2014r2.sav"
df <- read_sav(filepath, encoding = "UTF-8")


# Extract the BMI table ---------------------------------------------------

# extract the BMI table ---------------------------------------------------

# read the pdf file
filepath <- "misc/BMI.pdf"
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
names(tbl_BMI) <- c("age", "m_low", "m_upp", "m_ovr", #@#
                    "f_low", "f_upp", "f_ovr")

# for loop ----------------------------------------------------------------

df <- df %>% mutate(ovrw = NA)
        
for(i in 2:nrow(tbl_BMI)) {
        # male children
        for(j in 1:nrow(df)) {
                b <- !is.na(df$bmi[j]) & !is.na(df$Age[j]) & !is.na(df$Gender[j])
                # sex == 2
                s <- df$Gender[j] == 2
                # age
                m <- df$Age[j] < tbl_BMI$age[i] & df$Age[j] >= tbl_BMI$age[i - 1]
                # below average
                c1 <- df$bmi[j] < tbl_BMI$m_low[i - 1]
                # normal
                c2 <- df$bmi[j] > tbl_BMI$m_low[i - 1] & df$bmi[j] < tbl_BMI$m_upp[i - 1]
                # overweight
                c3 <- df$bmi[j] >= tbl_BMI$m_upp[i - 1] & df$bmi[j] < tbl_BMI$m_ovr[i - 1]
                # more than overweight
                c4 <- df$bmi[j] >= tbl_BMI$m_ovr[i - 1]
                if(b & s & m & c1) { df$ovrw[j] <- 1 } 
                if(b & s & m & c2) { df$ovrw[j] <- 2 } 
                if(b & s & m & c3) { df$ovrw[j] <- 3 } 
                if(b & s & m & c4) { df$ovrw[j] <- 4 } 
        }
        # female children
        for(j in 1:nrow(df)) {
                b <- !is.na(df$bmi[j]) & !is.na(df$Age[j]) & !is.na(df$Gender[j])
                s <- df$Gender[j] == 1
                f <- df$Age[j] < tbl_BMI$age[i] & df$Age[j] >= tbl_BMI$age[i - 1]
                c1 <- df$bmi[j] < tbl_BMI$f_low[i - 1]
                c2 <- df$bmi[j] > tbl_BMI$f_low[i - 1] & df$bmi[j] < tbl_BMI$f_upp[i - 1]
                c3 <- df$bmi[j] >= tbl_BMI$f_upp[i - 1] & df$bmi[j] < tbl_BMI$f_ovr[i - 1]
                c4 <- df$bmi[j] >= tbl_BMI$f_ovr[i - 1]
                if(b & s & f & c1) { df$ovrw[j] <- 1 } 
                if(b & s & f & c2) { df$ovrw[j] <- 2 } 
                if(b & s & f & c3) { df$ovrw[j] <- 3 } 
                if(b & s & f & c4) { df$ovrw[j] <- 4 } 
        }
}