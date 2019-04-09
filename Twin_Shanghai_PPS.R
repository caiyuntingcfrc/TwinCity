rm(list = ls())

# load packages
library(tidyverse)
library(stargazer)
library(summarytools)
library(sjlabelled)
library(ggplot2)
library(haven)
library(gridExtra)
library(knitr)
library(pps)

# load shanghai data set
# df_shanghai_2014_all <- read_spss("Twin Cities/1.上海_2014r2.sav") %>% remove_all_labels()
# df_shanghai_2016_all <- read_spss("Twin Cities/2.Children2016.sav") %>% remove_all_labels()

# save as .RData
# save(df_shanghai_2014_all, df_shanghai_2016_all, file = "Twin Cities/df_shanghai_2014_2016_all.RData")
# save(df_shanghai_2016_all, file = "Twin Cities/df_shanghai_2016_all.RData")

# read RData file
load("Twin Cities/df_shanghai_2014_2016_all.RData")

# 2014
# n <- table(df_shanghai_2014_all$D_ID) ; n
# x <- n / nrow(df_shanghai_2014_all) * 100; x
# p <- data.frame(x)
# p$Freq <- p$Freq %>% round(2); p

# 2016
p_2016 <- table(df_shanghai_2016_all$D_ID) %>% data.frame()
# x <- n / nrow(df_shanghai_2016_all) * 100; x
# p <- data.frame(x)
# p$Freq <- p$Freq %>% round(2); p

# Shanghai districts
path <- "Twin Cities/03. 上海分層抽樣/20190409_2016上海統計年鑑_表20.15_各區縣小學普通情況.csv"
name <- c("district", "n_sc", "n_grad", "n_recruit", "n_insch", "n_empl", "n_teachr")
type <- rep("numeric", 6) %>% c("factor", .)
xl <- read.csv(path, col.names = name, colClasses = type) %>% 
        mutate(
                D_ID = case_when(
                        district == "崇明縣" ~ 1, 
                        district == "徐匯區" ~ 2, 
                        district == "長寧區" ~ 3,
                        district == "普陀區" ~ 4,
                        district == "靜安區" ~ 5,
                        district == "閘北區" ~ 6,
                        district == "閔行區" ~ 7,
                        district == "嘉定區" ~ 8,
                        TRUE ~ 99
                        )
                ) %>% 
        filter(D_ID != 99 & D_ID !=2) %>%
        mutate(prop_insch = prop.table(n_insch), 
               size_insch = nrow(df_shanghai_2016_all) * prop_insch) %>% 
        group_by(D_ID)

# print table xl
kable(xl, digits = 2, format = "html")
grid.table(xl)
dev.off()

# pps
n <- nrow(df_shanghai_2016_all)
