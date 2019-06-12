
# prep and options --------------------------------------------------------

# rm
rm(list = ls())
# set working directory
setwd("D:/R_wd/")
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

filepath <- "Twin Cities/3.雙城正式_全年級20180704final_han&Lung.sav"
df <- read_sav(filepath, encoding = "UTF-8", user_na = FALSE)
var_lables <- sapply(df, attr, "label")
vlu_lables <- sapply(df, attr, "labels")
rm(filepath)


# extract the BMI table ---------------------------------------------------

# read the pdf file
filepath <- "Twin Cities/BMI/衛福部_2013_兒童與青少年生長身體質量指數(BMI)建議值.pdf"
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
# male children

df <- df %>% mutate(BMI = w_kg / ((h_cm / 100) ** 2), 
                    ovrw = 0)

for(i in 2:6) {
        i <- 6
        m <- df$realage < tbl_BMI$age[i] & df$realage >= tbl_BMI$age[i - 1] & df$Bd23 == 2
        a <- df[m, ] %>%
                mutate(., ovrw = case_when(BMI < tbl_BMI$m_low[i - 1] ~ 1,
                                           BMI > tbl_BMI$m_low[i - 1] & BMI < tbl_BMI$m_upp[i - 1] ~ 2, 
                                           BMI >= tbl_BMI$m_upp[i - 1] & BMI < tbl_BMI$m_ovr[i - 1] ~ 3, 
                                           BMI >= tbl_BMI$m_ovr[i - 1] ~ 4)
                )
        df[a$SID, ]$ovrw <- a$ovrw
        }
        
        
        f <- df$realage < tbl_BMI$age[i] & df$realage >= tbl_BMI$age[i - 1] & df$Bd23 == 1 
        b <- filter(df, f) %>% 
                mutate(., ovrw = case_when(BMI < tbl_BMI$f_low[i - 1] ~ 1,
                                        BMI > tbl_BMI$f_low[i - 1] & BMI < tbl_BMI$f_upp[i - 1] ~ 2, 
                                        BMI >= tbl_BMI$f_upp[i - 1] & BMI < tbl_BMI$f_ovr[i - 1] ~ 3, 
                                        BMI >= tbl_BMI$f_ovr[i - 1] ~ 4)
                       )
        df[b$SID, 284] <- b$ovrw
        }



m <- df %>% 
        filter(realage < tbl_BMI$age[14] & realage >= tbl_BMI$age[13]) %>% 
        filter(Bd23 == 2) %>% 
        mutate(ovrw = case_when(BMI < tbl_BMI$m_low[13] ~ 1,
                                BMI > tbl_BMI$m_low[13] & BMI < tbl_BMI$m_upp[13] ~ 2, 
                                BMI >= tbl_BMI$m_upp[13] & BMI < tbl_BMI$m_ovr[13] ~ 3, 
                                BMI >= tbl_BMI$m_ovr[13] ~ 4)
        )
df$ovrw[df$SID %in% m$SID] <- m$ovrw
