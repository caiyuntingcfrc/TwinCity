rm(list = ls())

# load packages
library(tidyverse)
library(stargazer)
library(summarytools)
library(sjlabelled)
library(ggplot2)
library(haven)

# load 2016 shanghai data set
df.shanghai.2016 <- read_spss("Twin Cities/2.Children2016.sav") %>% remove_all_labels()
