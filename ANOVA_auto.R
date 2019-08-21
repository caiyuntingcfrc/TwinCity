
# Prep --------------------------------------------------------------------

# clean global evironment
rm(list = ls())
# clean console
cat("\014")
#
setwd("D:/R_wd/Twin Cities/working/")
# list of packages
list.packages <- c("tidyverse", "magrittr", "haven", 
                   "gridExtra", "tabulizer", "knitr",
                   "labelled", "DescTools", "ggplot2", 
                   "sjPlot", "arsenal", "ggstatsplot", 
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
df <- read_sav("11. Taipei_all_temp.sav")

# # Ch2
# df$Ch2 <- factor(df$Ch2, 
#                  levels = c(1, 2, 3, 4, 5), 
#                  labels = c("極好", "非常好", "好", "一般", "不好"))

# Ch3
df$Ch3 <- factor(df$Ch3, 
                 levels = c(0, 1), 
                 labels = c("無", "有"))

# # Ft9
# df$Ft9 <- factor(df$Ft9,
#                         levels = attr(df$Ft9, "labels"), 
#                         labels = names(attr(df$Ft9, "labels")))

# vanilla automation ------------------------------------------------------

d <- df %>% filter(Ft9 %in% c(1, 2, 3)) %>% filter(Bd30 == 1)
var1 <- "Ch4_attention_sum"
group <- "Ft9"
sig <- 0.05

var <- LeveneTest(d[[var1]], group = d[[group]])
print(var)

if(var[ , 3][1] < sig) {
        # HoV = False
        a <- stats::oneway.test(d[[var1]] ~ d[[group]])
        print(a)
        # if sig. then post-hoc
        if(a[3] < sig) {
                d <- d %>% filter(!is.na(d[[var1]]) & !is.na(d[[group]]))
                userfriendlyscience::posthocTGH(y = d[[var1]], 
                                            x = as.factor(d[[group]]),
                                            digits = 3,
                                            method = "games-howell")
                }
        # Hov = TRUE
        } else { 
                a <- summary(aov(d[[var1]] ~ d[[group]]))
                print(a)
                # if sig. then post-hoc
                if(a[[1]]$`Pr(>F)`[1] < sig) {
                        out_post <- aov(d[[var1]] ~ as.factor(d[[group]]))
                        DescTools::PostHocTest(out_post, method = "scheffe") }
        }
