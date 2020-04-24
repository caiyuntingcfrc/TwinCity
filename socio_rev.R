
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
        round(2) %>% 
        .[-7, ]

# plots-vanilla -----------------------------------------------------------

# boxplot
boxplot(d)

# bar plot
p1 <- ggplot(d, aes(x = Ch4_aggressive_sum)) + 
        geom_bar() + 
        theme_pubclean()

p2 <- ggplot(d, aes(x = Ch4_attention_sum)) + 
        geom_bar() + 
        theme_pubclean()

p3 <- ggplot(d, aes(x = Ch4_soc_sum)) + 
        geom_bar() + 
        theme_pubclean()

p4 <- ggplot(d, aes(x = Ch4_withdrawal_sum)) + 
        geom_bar() + 
        theme_pubclean()

gridExtra::grid.arrange(p1, p2, p3, p4, nrow = 2)

# desc-filter -------------------------------------------------------------
# filter Ft9, Bd21
# n: 5666
d <- df %>% 
        # caregiver: parents(1), the father(2) and the mother(3)
        filter(Ft9 %in% c(1, 2, 3)) %>% 
        # respondent: the mother(1) and the father(2)
        filter(Bd21 %in% c(1, 2))
# select Ch4_
d1 <- d %>% 
        select(matches("^Ch4_.*_sum")) %>% 
        as.data.frame()
# desc
desc <- stat.desc(d1, desc = TRUE) %>% 
        round(2) %>% 
        .[-7, ]
# boxplot
boxplot(d1)

# bar plot
p1 <- ggplot(d1, aes(x = Ch4_aggressive_sum)) + 
        geom_bar() + 
        theme_pubclean()

p2 <- ggplot(d1, aes(x = Ch4_attention_sum)) + 
        geom_bar() + 
        theme_pubclean()

p3 <- ggplot(d1, aes(x = Ch4_soc_sum)) + 
        geom_bar() + 
        theme_pubclean()

p4 <- ggplot(d1, aes(x = Ch4_withdrawal_sum)) + 
        geom_bar() + 
        theme_pubclean()

gridExtra::grid.arrange(p1, p2, p3, p4, nrow = 2)

