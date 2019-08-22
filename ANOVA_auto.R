
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

d <- df %>% 
        filter(Ft9 %in% c(1, 2, 3)) %>% 
        filter(Bd21 %in% c(1, 2)) %>% 
        filter(Bd30 == 1)

# list of packages
list.packages <- c("tidyverse", "magrittr", "DescTools", "userfriendlyscience")
# check if the packages are installed
new.packages <- list.packages[!(list.packages %in% installed.packages()[ , "Package"])]
# install new packages
if(length(new.packages)) install.packages(new.packages)
# remove lists
rm(list.packages, new.packages)
# options
options(scipen = 999)

aov_auto <- function(data, var1, group, sig = 0.05) {
        # Test of Homogeneity of Variance
        var <- DescTools::LeveneTest(data[[var1]], 
                                     group = as.factor(data[[group]]))
        print(var)
        
        if(var[ , 3][1] < sig) {
                # Homogeneity of Variance = FALSE
                out_aov <- stats::oneway.test(data[[var1]] ~ data[[group]])
                print(out_aov)
                # if sig. then post-hoc (Games-Howell)
                if(out_aov[3] < sig) {
                        data <- data %>% 
                                filter(!is.na(data[[var1]]) & !is.na(data[[group]]))
                        userfriendlyscience::posthocTGH(y = data[[var1]], 
                                                        x = as.factor(data[[group]]),
                                                        digits = 3,
                                                        method = "games-howell")
                        }
                # Homogeneity of Variance = TRUE
                } else { 
                        out_aov <- summary(aov(d[[var1]] ~ d[[group]]))
                        print(out_aov)
                        # if sig. then post-hoc
                        if(out_aov[[1]]$`Pr(>F)`[1] < sig) {
                                out_post <- aov(d[[var1]] ~ as.factor(d[[group]]))
                                DescTools::PostHocTest(out_post, method = "scheffe") 
                                }
                        }
        }

var <- DescTools::LeveneTest(data[[var1]], group = data[[group]])
print(var)

if(var[ , 3][1] < sig) {
        # Homogeneity of Variance = FALSE
        out_aov <- stats::oneway.test(data[[var1]] ~ data[[group]])
        print(out_aov)
        # if sig. then post-hoc (Games-Howell)
        if(out_aov[3] < sig) {
                data <- data %>% 
                        filter(!is.na(data[[var1]]) & !is.na(data[[group]]))
                userfriendlyscience::posthocTGH(y = data[[var1]], 
                                            x = as.factor(data[[group]]),
                                            digits = 3,
                                            method = "games-howell")
                }
        # Homogeneity of Variance = TRUE
        } else { 
                a <- summary(aov(d[[var1]] ~ d[[group]]))
                print(a)
                # if sig. then post-hoc
                if(a[[1]]$`Pr(>F)`[1] < sig) {
                        out_post <- aov(d[[var1]] ~ as.factor(d[[group]]))
                        DescTools::PostHocTest(out_post, method = "scheffe") }
        }

