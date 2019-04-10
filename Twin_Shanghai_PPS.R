rm(list = ls())

# load packages
l <- c("tidyverse", "stargazer", "summarytools", "sjlabelled", "ggplot2", 
       "haven", "gridExtra", "knitr", "microbenchmark")
lapply(l, require, character.only = TRUE); rm(l)

#read .sav files
df_shanghai_2016_all <- read_sav("Twin Cities/2.Children2016.sav") %>% remove_all_labels()
# save file
save(df_shanghai_2016_all, file = "Twin Cities/df_shanghai_2016_all.RData")
# read RData file
load("Twin Cities/df_shanghai_2016_all.RData")

# 2016
p_2016 <- table(df_shanghai_2016_all$D_ID) %>% data.frame()
names(p_2016) <- c("D_ID", "Freq_2016")
p_2016 <- p_2016 %>% mutate(
        prop_2016 = Freq_2016 / sum(Freq_2016)
        )
prop.table(p_2016$Freq)

# Shanghai districts
path <- "Twin Cities/03. 上海分層抽樣/20190409_2016上海統計年鑑_表20.15_各區縣小學普通情況.csv"
name <- c("district", "n_sc", "n_grad", "n_recruit", "n_insch", "n_empl", "n_teachr")
type <- rep("numeric", 6) %>% c("character", .)
xl <- read.csv(path, col.names = name, colClasses = type) %>% 
        mutate(
                D_ID = case_when(
                        district == "崇明縣" ~ "1", 
                        district == "徐匯區" ~ "2", 
                        district == "長寧區" ~ "3",
                        district == "普陀區" ~ "4",
                        district == "靜安區" ~ "5",
                        district == "閘北區" ~ "6",
                        district == "閔行區" ~ "7",
                        district == "嘉定區" ~ "8",
                        TRUE ~ "99"
                        )
                ) %>% 
        filter(D_ID != 99 & D_ID !=2) %>%
        mutate(prop_insch = prop.table(n_insch), 
               size_insch = nrow(df_shanghai_2016_all) * prop_insch) %>% 
        group_by(D_ID)

# left_join by D_ID
xl <- left_join(xl, p_2016, by = "D_ID")
# attempted sample size that is larger than population.
# find maximum differences
# z <- xl[with(xl, size_insch > Freq_2016), ] %>% 
#         .[which.max(with(., size_insch - Freq_2016)), ]
# calculate adjusted sample size
xl <- xl %>% mutate(
        adj_N = Freq_2016 / prop_insch
        )
# adj <- with(z, Freq_2016 / prop_insch) %>% round(., 0)
a <- xl %>% .[which(with(., adj_N < nrow(df_shanghai_2016_all) & adj_N > min(adj_N))), ]; a
adj <- a$adj_N

# calculate adjusted n in each district
xl <- xl %>%  
        mutate(
                adj_size = round(adj * prop_insch)
                ) %>% 
        .[order(.$D_ID), ]
# rm
rm(list = c("p_2016", "z"))

# order
df <- df_shanghai_2016_all %>%  .[order(.$D_ID), ] %>% group_by(D_ID)

# sampling
l <- vector("list", nrow(xl))
for(i in 1:nrow(xl)){
        l[[i]] <- df[grep(xl$D_ID[i], df$D_ID) %>% sample(xl$adj_size[i]), ]
        df_pps <- bind_rows(l)
        }
microbenchmark(
        for(i in 1:nrow(xl)){
        l[[i]] <- df[grep(xl$D_ID[i], df$D_ID) %>% sample(xl$adj_size[i]), ]
        df_pps <- bind_rows(l)
        }
) %>% autoplot()

table(df_pps$D_ID)

# print table xl
x <- kable(xl, digits = 2, format = "html"); cat(x, sep = "\n")
save(x, file = "Twin Cities/03. 上海分層抽樣/test.html")
grid.table(xl)
dev.off()
