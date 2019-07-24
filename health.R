
# Prep and Options --------------------------------------------------------

# rm
rm(list = ls())
# set working directory
setwd("D:/R_wd/Twin Cities/data")
# list of packages
list.packages <- c("tidyverse", "magrittr", "haven", 
                   "gridExtra", "summarytools", "tabulizer", 
                   "labelled", "DescTools", "ggplot2", 
                   "stargazer", "sjPlot", "arsenal")
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


# Load the data file ------------------------------------------------------

df <- read_sav("twin cities/資料檔/11. Taipei_all.sav", encoding = "UTF-8")
d <- df %>% filter(grade == 1)
ggplot(aes(x = Ft11_1hr, y = Ch4_attention_sum), data = df) + 
        geom_point(shape = 1, na.rm = TRUE, stroke = .1) + 
        geom_smooth(method = lm, se = TRUE, na.rm = TRUE) + 
        theme_classic()

cor.test(x = d$Ft11_1hr, y = d$Ch4_attention_sum, method = "spearman")
dd <- tibble(d$Ft11_1hr, d$Ch4_attention_sum)
sjt.corr(data = dd, 
         corr.method = "pearson", 
         var.labels = c("週間看電視時數", "注意力總分"), 
         p.numeric = TRUE, 
         na.deletion = "listwise", 
         encoding = "UTF-8")

# sjt.xtab(var.row = dd$`d$Ft11_1hr`, var.col = dd$`d$Ch4_attention_sum`, 
#          var.labels = c("週間看電視時數", "注意力總分"), 
#          statistics = "spearman", 
#          encoding = "CP950", 
#          show.row.prc = TRUE, 
#          show.col.prc = TRUE)
