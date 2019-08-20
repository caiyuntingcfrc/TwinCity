
# Prep and Options --------------------------------------------------------

# rm
rm(list = ls())
# set working directory
setwd("D:/R_wd/Twin Cities/")
# list of packages
list.packages <- c("tidyverse", "magrittr", "haven", 
                   "gridExtra", "summarytools", "tabulizer", 
                   "labelled", "DescTools", "userfriendlyscience", 
                   "stargazer", "sjPlot", "sjlabelled")
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

# Read the Data File ------------------------------------------------------

# read .sav file
filepath <- "3.雙城正式_全年級20180704final_han&Lung.sav"
df <- haven::read_sav(filepath, user_na = TRUE, encoding = "UTF-8")
# df <- foreign::read.spss(filepath, use.missings = FALSE, reencode = "UTF-8", 
#                          to.data.frame = TRUE, use.value.labels = FALSE, 
#                          sub = NA)
# n <- names(df)
# for(i in 1:length(n)) {
#         attr(df[[n[i]]], "na_values") <- NULL 
#         attr(df[[n[i]]], "format.spss") <- NULL 
#         attr(df[[n[i]]], "display_width") <- NULL 
#         }
# Structure of Families ---------------------------------------------------

# recode parents and grand parents
df <- df %>% mutate(
        # father
        bd25_dad = case_when(Bd25_1.1 == 1 | Bd25_1.2 == 1 | Bd25_1.3 == 1 ~ 1, 
                             is.na(Bd25_1.1) | is.na(Bd25_1.2) | is.na(Bd25_1.3) ~ NA_real_, 
                             TRUE ~ 0),
        # mother
        bd25_mom = case_when(Bd25_2.1 == 1 | Bd25_2.2 == 1 | Bd25_2.3 == 1 ~ 1, 
                             is.na(Bd25_2.1) | is.na(Bd25_2.2) | is.na(Bd25_2.3) ~ NA_real_, 
                             TRUE ~ 0),
        # grand parents
        bd25_grand = case_when(Bd25_3.1 == 1 | Bd25_3.2 == 1 | Bd25_3.3 == 1 |
                                       Bd25_3.4 == 1 ~ 1,
                               is.na(Bd25_3.1) | is.na(Bd25_3.2) | 
                                       is.na(Bd25_3.3) | is.na(Bd25_3.4) ~ NA_real_, 
                               TRUE ~ 0)
        )
# recode structure of families (stand alone)
df <- df %>% mutate(
        # nuclear family
        Sf_nuc = case_when(bd25_dad == 1 & bd25_mom == 1 & bd25_grand == 0 ~ 1, 
                           is.na(bd25_dad) | is.na(bd25_mom) | is.na(bd25_grand) ~ NA_real_, 
                           TRUE ~ 0), 
        # grandparenting family
        Sf_grand = case_when(bd25_dad == 0 & bd25_mom == 0 & bd25_grand == 1 ~ 1, 
                             is.na(bd25_dad) | is.na(bd25_mom) | is.na(bd25_grand) ~ NA_real_, 
                             TRUE ~ 0), 
        # three generation family
        Sf_three = case_when((bd25_dad == 1 | bd25_mom == 1) & bd25_grand == 1 ~ 1, 
                             is.na(bd25_dad) | is.na(bd25_mom) | is.na(bd25_grand) ~ NA_real_, 
                             TRUE ~ 0), 
        # single parent family
        sp = case_when((bd25_dad == 1 & bd25_mom == 0 & bd25_grand == 0) | 
                                  (bd25_dad == 0 & bd25_mom == 1 & bd25_grand == 0) ~ 1, 
                       is.na(bd25_dad) | is.na(bd25_mom) | is.na(bd25_grand) ~ NA_real_, 
                       TRUE ~ 0),
        Sf_sp = case_when(sp == 1 & Bd25_6 == 0 ~ 1, 
                          # cohabitation (a single parent with a cohabitating partner)
                          sp == 1 & Bd25_6 == 1 ~ 2, 
                          is.na(sp) | is.na(Bd25_6) ~ NA_real_, 
                          TRUE ~ 0),
        # others
        Sf_other = case_when(Sf_nuc == 0 & Sf_grand == 0 & Sf_three == 0 & 
                                     Sf_sp == 0 ~ 1)
        )

df <- df %>% mutate(
        Sf = case_when(Sf_nuc == 1 ~ 2, 
                       Sf_grand == 1 ~ 4, 
                       Sf_three == 1 ~ 3,
                       Sf_sp == 1 ~ 1, 
                       Sf_sp == 2 ~ 6, 
                       Sf_other == 1 ~ 5
                       )
        )
# drop variables
df <- df %>% select(-c(bd25_dad, bd25_mom, bd25_grand, 
                       Sf_nuc, Sf_grand, Sf_three, 
                       Sf_sp, Sf_other, sp))

# add attributes
df$Sf <- labelled(df$Sf, c("single parent" = 1,
                           "nuclear" = 2,
                           "three generation" = 3,
                           "grand parenting" = 4,
                           "others" = 5,
                           "single_cohabitating" = 6),
                  label = "Structure of Families")
attr(df$Sf, "format.spss") = "F8.2"

d <- enframe(df$Sf, name = NULL) %>% replace(., is.na(.), 99)

# df[] <-  lapply(df, function(x) iconv(x, to="UTF-8"))
# save the file
# sjlabelled::write_spss(df, path = "D:/t.sav")
write_sav(d, "d_sf.sav", compress = F)
