
# Prep --------------------------------------------------------------------

# clean global evironment
rm(list = ls())
# clean console
cat("\014")
#
setwd("~/R data/twin/資料檔/")
# list of packages
list.packages <- c("tidyverse", "magrittr", "haven", 
                   "gridExtra", "tabulizer", "knitr",
                   "labelled", "DescTools", "ggplot2", 
                   "stargazer", "sjPlot", "arsenal", 
                   "questionr", "descr", "PerformanceAnalytics", 
                   "car", "userfriendlyscience")
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


# read file ---------------------------------------------------------------

# df
df <- read_sav("11. Taipei_all.sav")

# # Ch2
# df$Ch2 <- factor(df$Ch2, 
#                  levels = c(1, 2, 3, 4, 5), 
#                  labels = c("極好", "非常好", "好", "一般", "不好"))

# Ch3
df$Ch3 <- factor(df$Ch3, 
                 levels = c(0, 1), 
                 labels = c("無", "有"))

# Ft9
df$Ft9_combin <- factor(df$Ft9_combin,
                        levels = attr(df$Ft9_combin, "labels"), 
                        labels = names(attr(df$Ft9_combin, "labels")))

# vanilla automation ------------------------------------------------------

d <- df %>% filter(Ft9_combin %in% c("母親", "父親", "父母"))
var1 <- "Bd44"
group <- "Ft9_combin"
sig <- 0.05

var <- LeveneTest(d[[var1]], group = d[[group]])
print(var)

if(var[ , 3][1] < sig) {
        # HoV = False
        a <- oneway.test(d[[var1]] ~ d[[group]])
        print(a)
        # if sig. then post-hoc
        if(a[3] < sig) {
                out_post <- aov(d[[var1]] ~ d[[group]])
                PostHocTest(out_post, method = "scheffe") }
        # Hov = TRUE
        } else { 
                a <- summary(aov(d[[var1]] ~ d[[group]]))
                print(a)
                # if sig. then post-hoc
                if(a[[1]]$`Pr(>F)`[1] < sig) {
                        out_post <- aov(d[[var1]] ~ d[[group]])
                        PostHocTest(out_post, method = "scheffe") }
                }