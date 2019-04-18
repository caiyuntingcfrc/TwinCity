rm(list = ls())

# load packages
l <- c("tidyverse", "stargazer", "summarytools", "sjlabelled", "ggplot2", 
       "haven", "gridExtra", "knitr", "microbenchmark", "xlsx")
lapply(l, require, character.only = TRUE); rm(l)

##### import files #####
# read .sav files
# df_shanghai_2016_all <- read_sav("Twin Cities/2.Children2016.sav")
# save file
# save(df_shanghai_2016_all, file = "Twin Cities/df_shanghai_2016_all.RData")
# read RData file
load("Twin Cities/df_shanghai_2016_all.RData")

# 2016
p_2016 <- table(df_shanghai_2016_all$D_ID) %>% data.frame()
names(p_2016) <- c("D_ID", "Freq_2016")
p_2016 <- p_2016 %>% mutate(
        prop_2016 = Freq_2016 / sum(Freq_2016)
        )
prop.table(p_2016$Freq)

# recode Shanghai districts D_ID
path <- "Twin Cities/03. 上海分層抽樣/2012sampling_rev.xlsx"
name <- c("district", "n_sc", "n_grad", "n_recruit", "n_insch", "n_empl", "n_teachr")
type <- rep("numeric", 6) %>% c("text", .)
xl <- readxl::read_xlsx(path, range = "A1:G17", col_names = name, col_types = type)
Encoding(xl$district)
# xl$district <- xl$district %>% iconv(from = "UTF-8", to = "CP950")
xl <- xl %>% 
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

# calculate sample size by each district
xl <- xl %>% mutate(
        adj_N = Freq_2016 / prop_insch
        )
adj <- xl %>% .[which(with(., adj_N < nrow(df_shanghai_2016_all))), ] %>% 
        .$adj_N; adj

adj_1164 <- adj[1]
adj_832 <- adj[2]
adj_1674 <- adj[3]

# calculate adjusted n in each district. adj = 1124
xl <- xl %>%  
        mutate(
                adj_size_1164 = round(adj_1164 * prop_insch), 
                adj_size_832 = round(adj_832 * prop_insch), 
                adj_size_1674 = round(adj_1674 * prop_insch)
                )

# determine in which district that the adjusted sample size is larger than original one.
b_1164 <- xl %>% .[which(with(., adj_size_1164 >= Freq_2016)), ] %>% .$D_ID
b_832 <- xl %>% .[which(with(., adj_size_832 >= Freq_2016)), ] %>% .$D_ID
b_1674 <- xl %>% .[which(with(., adj_size_1674 >= Freq_2016)), ] %>% .$D_ID

# save xl to excel file
xl <-  xl %>% .[order(.$D_ID), ] %>% as.data.frame()
write.xlsx(xl, file = "Twin Cities/03. 上海分層抽樣/sampling_frame.xlsx", 
           row.names = FALSE, col.names = TRUE)

# skip the district (all of the cases will be sampled)
df <- df_shanghai_2016_all %>% filter(!(D_ID %in% b)) %>% 
        .[order(.$D_ID), ] %>% group_by(D_ID)
df_ <- df_shanghai_2016_all %>% filter(D_ID %in% b) %>% 
        .[order(.$D_ID), ] %>% group_by(D_ID)
x <- xl %>% filter(!(D_ID %in% b))

##### PPS #####
l <- vector("list", nrow(x))
for(i in 1:nrow(x)){
        l[[i]] <- df[grep(x$D_ID[i], df$D_ID) %>% sample(x$adj_size[i]), ]
        d <- bind_rows(l)
        }
# bind rows and order
df_pps_2016 <- bind_rows(df_, d) %>% .[order(.$D_ID), ]

# rm
r <- ls() %>% .[!(. %in% c("df_pps_2016", "df_shanghai_2016_all", "xl"))]
rm(list = r)

# save
# save(df_pps_2016, file = "Twin Cities/03. 上海分層抽樣/2016_PPS.RData")
# write_sav(df_pps_2016, "Twin Cities/03. 上海分層抽樣/2016_PPS.sav")

##### plot #####

# table
p <- table(df_pps_2016$D_ID) %>% prop.table() %>% data.frame()
names(p) <- c("D_ID", "prop_PPS")
xl <- left_join(xl, p, by = "D_ID")

p <- ggplot(xl, aes(x = "D_ID", y = "prop"))

p1 <- qplot(xl$D_ID, xl$prop_insch)
ggsave("p1.png", device = "png", plot = p1, path = "Twin Cities/03. 上海分層抽樣/")
p2 <- qplot(xl$D_ID, xl$prop_PPS)
ggsave("p2.png", device = "png", plot = p2, path = "Twin Cities/03. 上海分層抽樣/")

table(df_pps_2016$D_ID) %>% prop.table() %>% plot()
xl$prop_insch %>% plot()

# print table xl
# x <- kable(xl, digits = 2, format = "html")
# cat(x, file = "Twin Cities/03. 上海分層抽樣/test.html")
pdf(file = "Twin Cities/03. 上海分層抽樣/test.pdf", height = 8.5, width = 13)
grid.table(xl)
dev.off()
