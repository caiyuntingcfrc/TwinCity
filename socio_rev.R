
# prep and options --------------------------------------------------------
# rm
rm(list = ls()); cat("\14")
source("~/Github_CFRC/misc/func_ins.pack.R")
# setwd
setwd("d:/R_wd/")
# option
options(scipen = 999)
# ins.pak
ins.pack("tidyverse", "feather", "ggpubr", "ggExtra", "sjstats", "pastecs")

# read data file ----------------------------------------------------------
df <- read_feather("Twin Cities Data/tp_all.feather")
utils::View(df)
d <- df %>% select(Ch4_attention_sum, Ch4_soc_sum)
desc <- stat.desc(d, desc = FALSE, norm = TRUE)
shapiro.test()
