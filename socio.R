
# prep --------------------------------------------------------------------

rm(list = ls())
# soure func: ins.pack
devtools::source_url("https://raw.githubusercontent.com/caiyuntingcfrc/misc/function_poverty/func_ins.pack.R")
ins.pack("haven", "tidyverse", "magrittr", "sjPlot", "feather")
setwd("~/R_wd/Twin Cities Data/")

# read file ---------------------------------------------------------------
# df <- read_sav("11. Taipei_all_rev.sav")
# # recode: sum Bd43 (social support)
# df %<>% mutate(socio = Bd43_1 + Bd43_2 + Bd43_3 +
#                        Bd43_4 + Bd43_5 + Bd43_6)
# setdiff(df$socio, df$Bd43_sum)
# write_feather(df, "Twin Cities Data/tp_all.feather")
df <- read_feather("tp_all.feather")
plot_frq(df$socio)
# plot Bd43 likert
d43 <- df %>% 
        select(matches("^Bd43_")) %>% 
        select(-Bd43_sum)
plot_likert(d43, catcount = 3) + theme_sjplot2()
