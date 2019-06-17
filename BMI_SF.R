
# Prep and Options --------------------------------------------------------

# rm
rm(list = ls())
# set working directory
setwd("D:/R_wd/Twin Cities/")
library(devtools)
# devtools::install_version("sjPlot", version = "1.7")
# install.packages("sjPlot", lib = "c:/Program Files/R/library/")

# list of packages
list.packages <- c("tidyverse", "magrittr", "haven", 
                   "gridExtra", "summarytools", "tabulizer", 
                   "labelled", "DescTools", "userfriendlyscience", "stargazer", 
                   "sjlabelled", "sjmisc", "sjstats", "ggeffects")
# check if the packages are installed
new.packages <- list.packages[!(list.packages %in% installed.packages()[ , "Package"])]
# install new packages
if(length(new.packages)) install.packages(new.packages)
# load the packages
lapply(list.packages, require, character.only = TRUE)
# remove lists
rm(list.packages, new.packages)
# 
library(sjPlot, lib.loc = "c:/Program Files/R/myLibrary/")
# options
options(scipen = 999, 
        autoSetValueLabels = TRUE, 
        autoSetVariableLabels = TRUE)
# options for summarytools
st_options(style = "simple",
           round.digits = 4,
           ctable.prop = "c")

# Read the Data File ------------------------------------------------------

# read .sav file
filepath <- "11. Taipei_all.sav"
df <- read_sav(filepath, encoding = "UTF-8")


# bmi z-score -------------------------------------------------------------



# Desc --------------------------------------------------------------------
t <- table(BMI = df$bmicat, 
           Grade = df$grade, df$Bd23)

sjt.xtab(var.row = df$bmicat, var.col = df$grade,  
         var.grp = df$Bd23, 
         variableLabels = c("BMI 分類", 
                            "年級", 
                            "性別"),
         stringTotal = "總和",
         showHorizontalLine = TRUE,
         showCellPerc = TRUE,
         showLegend = FALSE,
         showSummary = FALSE,
         highlightTotal = TRUE,
         encoding = "CP950")
# detach sjPlot 1.7
detach("package:sjPlot", unload = TRUE)
# load sjPlot 2.3
library(sjPlot, lib.loc = "c:/Program Files/R/library")
