
# prep and options --------------------------------------------------------
# rm
rm(list = ls()); cat("\14")
source("~/Github_CFRC/misc/func_ins.pack.R")
# setwd
setwd("d:/R_wd/")
# option
options(scipen = 999)
# ins.pak
ins.pack("tidyverse", "feather", "ggpubr", "ggExtra", 
         "sjstats", "pastecs", "stargazer")

# read data file ----------------------------------------------------------

df <- read_feather("Twin Cities Data/tp_all.feather")

# desc-vanilla ------------------------------------------------------------

# Ch4_
d <- df %>% 
        select(matches("^Ch4_.*_sum")) %>% 
        as.data.frame()
# desc
desc <- stat.desc(d, desc = TRUE) %>% 
        round(2)
# boxplot
boxplot(d)

qplot(d$Ch4_attention_sum, binwidth = .5) + sjPlot::theme_sjplot2()
qplot(d$Ch4_soc_sum, binwidth = .5) + sjPlot::theme_sjplot2()

# desc-filter -------------------------------------------------------------
# filter 
d <- df %>% 
        # caregiver: parents(1), the father(2) and the mother(3)
        filter(Ft9 %in% c(1, 2, 3)) %>% 
        # respondent: the mother(1) and the father(2)
        filter(Bd21 %in% c(1, 2))
str(df)
boxplot()

