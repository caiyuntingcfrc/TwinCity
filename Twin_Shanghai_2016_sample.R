rm(list = ls())
##### load packages #####
l <- c("tidyverse", "stargazer", "summarytools", "sjlabelled", "ggplot2", 
       "haven", "gridExtra", "knitr", "microbenchmark", "Hmisc")
lapply(l, require, character.only = TRUE); rm(l)

##### read the RData file #####
load("Twin Cities/03. 上海分層抽樣/2016_PPS.RData")

##### descriptive analysis #####

# D_ID (district id)
df_pps_2016 %>% 
        group_by(D_ID) %>% 
        summarise(n = n()) %>% 
        mutate(prop = n / sum(n) * 100)


# Gender (children's gender)
df_pps_2016 %>% 
        group_by(Gender) %>% 
        summarise(n = n()) %>% 
        mutate(prop = n / sum(n) * 100)

# mother's education
df_pps_2016 %>% 
        group_by(SP_Educ) %>% 
        summarise(n = n()) %>% 
        mutate(prop = n / sum(n) * 100)

# wkhr (working hours per week)
df_pps_2016 %>% 
        filter(!(wkhr %in% c(777, 888, 999))) %>% 
        .$wkhr %>% 
        summary()

# SP_WKHR (spouse's working hour per week)
# wkhr (working hours per week)
df_pps_2016 %>% 
        filter(!(SP_WKHR %in% c(777, 888, 999))) %>% 
        .$SP_WKHR %>% 
        summary()

# annual earning
# 