rm(list = ls())
cat("\014")
library(tidyverse)
library(haven)

source("C:/Users/user/Documents/Github_CFRC/TwinCity/AOV_auto.R")

# standardized scores -----------------------------------------------------

df <- read_sav("Twin Cities/working/11. Taipei_all_temp.sav", encoding = "UTF-8") 
        # mutate(Ch4_aggressive_sum_s = scale(Ch4_aggressive_sum)[ , 1],
        #        Ch4_soc_sum_s = scale(Ch4_soc_sum)[ , 1],
        #        Ch4_attention_sum_s = scale(Ch4_attention_sum)[ , 1],
        #        Ch4_soc_sum_s = scale(Ch4_soc_sum)[ , 1])


# filter ------------------------------------------------------------------

d <- df %>% 
        filter(Ft9 %in% c(1, 2, 3)) %>% 
        filter(Bd21 %in% c(1, 2)) %>% 
        filter(Bd30 == 1)

# corr --------------------------------------------------------------------

d.corr <- d %>% 
        select(contains("_sum")) %>% 
        select(-c(Bd43_sum, Ft7_sum))

PerformanceAnalytics::chart.Correlation(d.corr, 
                                        histogram = FALSE,
                                        method = "pearson")

# 1. aggressive -----------------------------------------------------------

# remove outliers agg
agg.IQR <- IQR(d$Ch4_aggressive_sum, na.rm = TRUE)
fence_u <- quantile(d$Ch4_aggressive_sum, na.rm = TRUE)[4] + 1.5 * agg.IQR
fence_l <- quantile(d$Ch4_aggressive_sum, na.rm = TRUE)[2] - 1.5 * agg.IQR

d1 <- d %>% 
        filter(between(Ch4_aggressive_sum, fence_l, fence_u))

aov_auto(data = d, var1 = "Ch4_aggressive_sum", group = "Ft9")
aov_auto(data = d1, var1 = "Ch4_aggressive_sum", group = "Ft9")

# ggbetweenstats(data = d1, 
#                x = Ft9, 
#                y = Ch4_aggressive_sum_s, 
#                type = "p", 
#                pairwise.comparisons = TRUE, 
#                pairwise.display = TRUE,
#                p.adjust.method = "none")


# 2. soc ------------------------------------------------------------------

# remove outliers agg
soc.IQR <- IQR(d$Ch4_soc_sum, na.rm = TRUE)
fence_u <- quantile(d$Ch4_soc_sum, na.rm = TRUE)[4] + 1.5 * soc.IQR
fence_l <- quantile(d$Ch4_soc_sum, na.rm = TRUE)[2] - 1.5 * soc.IQR

d2 <- d %>% 
        filter(between(Ch4_soc_sum, fence_l, fence_u))

aov_auto(data = d, var1 = "Ch4_soc_sum", group = "Ft9")
aov_auto(data = d2, var1 = "Ch4_soc_sum", group = "Ft9")


# 3. attention ------------------------------------------------------------

# remove outliers agg
attention.IQR <- IQR(d$Ch4_attention_sum, na.rm = TRUE)
fence_u <- quantile(d$Ch4_attention_sum, na.rm = TRUE)[4] + 1.5 * attention.IQR
fence_l <- quantile(d$Ch4_attention_sum, na.rm = TRUE)[2] - 1.5 * attention.IQR

d3 <- d %>% 
        filter(between(Ch4_attention_sum, fence_l, fence_u))

aov_auto(data = d, var1 = "Ch4_attention_sum", group = "Ft9")
aov_auto(data = d3, var1 = "Ch4_attention_sum", group = "Ft9")


# 4. withdrawal -----------------------------------------------------------

# remove outliers agg
withdrawal.IQR <- IQR(d$Ch4_withdrawal_sum, na.rm = TRUE)
fence_u <- quantile(d$Ch4_withdrawal_sum, na.rm = TRUE)[4] + 1.5 * withdrawal.IQR
fence_l <- quantile(d$Ch4_withdrawal_sum, na.rm = TRUE)[2] - 1.5 * withdrawal.IQR

d4 <- d %>% 
        filter(between(Ch4_withdrawal_sum, fence_l, fence_u))

aov_auto(data = d, var1 = "Ch4_withdrawal_sum", group = "Ft9")
aov_auto(data = d4, var1 = "Ch4_withdrawal_sum", group = "Ft9")


# Ft8 ---------------------------------------------------------------------

aov_auto(data = d, var1 = "Ft8_achievement_sum", group = "Ft9")
aov_auto(data = d, var1 = "Ft8_warmth_sum", group = "Ft9")
aov_auto(data = d, var1 = "Ft8_monitoring_sum", group = "Ft9")
aov_auto(data = d, var1 = "Ft8_harsh_sum", group = "Ft9")
aov_auto(data = d, var1 = "Ft8_indulgence_sum", group = "Ft9")
aov_auto(data = d, var1 = "Ft8_autonomy_sum", group = "Ft9")


# compare co-p ------------------------------------------------------------

# data
d.co <- df %>% 
        filter(Ft9 == 1) %>% 
        filter(Bd21 %in% c(1, 2)) %>% 
        filter(Bd30 == 1)

# 1. agg
DescTools::LeveneTest(y = d.co$Ch4_aggressive_sum, 
                      group = d.co$Bd21)
t1 <- d.co %>% filter(Bd21 == 1) %>% .$Ch4_aggressive_sum
t2 <- d.co %>% filter(Bd21 == 2) %>% .$Ch4_aggressive_sum
t.test(t1, t2, alternative = "less", var.equal = FALSE)
# eff
effsize::cohen.d(as.numeric(Ch4_aggressive_sum) ~ Bd21, 
                 data = d.co,
                 pooled = TRUE, 
                 na.rm = TRUE, 
                 paired = FALSE)

# 2. soc
DescTools::LeveneTest(y = d.co$Ch4_soc_sum, 
                      group = d.co$Bd21)
t1 <- d.co %>% filter(Bd21 == 1) %>% .$Ch4_soc_sum
t2 <- d.co %>% filter(Bd21 == 2) %>% .$Ch4_soc_sum
t.test(t1, t2, alternative = "less", var.equal = TRUE)
# eff
effsize::cohen.d(as.numeric(Ch4_soc_sum) ~ Bd21, 
                 data = d.co,
                 pooled = TRUE, 
                 na.rm = TRUE, 
                 paired = FALSE)

# 3. att
DescTools::LeveneTest(y = d.co$Ch4_attention_sum, 
                      group = d.co$Bd21)
t1 <- d.co %>% filter(Bd21 == 1) %>% .$Ch4_attention_sum
t2 <- d.co %>% filter(Bd21 == 2) %>% .$Ch4_attention_sum
t.test(t1, t2, alternative = "less", var.equal = TRUE)
# eff
effsize::cohen.d(as.integer(Ch4_attention_sum) ~ Bd21, 
                 data = d.co,
                 na.rm = TRUE, 
                 paired = FALSE)

# 4. Withdrawal
DescTools::LeveneTest(y = d.co$Ch4_withdrawal_sum, 
                      group = d.co$Bd21)
t1 <- d.co %>% filter(Bd21 == 1) %>% .$Ch4_withdrawal_sum
t2 <- d.co %>% filter(Bd21 == 2) %>% .$Ch4_withdrawal_sum
t.test(t1, t2, alternative = "less", var.equal = FALSE)
# eff
effsize::cohen.d(as.integer(Ch4_withdrawal_sum) ~ Bd21, 
                 data = d.co,
                 na.rm = TRUE, 
                 paired = FALSE)

aov_auto(d.co, var1 = "Ch4_aggressive_sum", group = "Bd21")
aov_auto(d.co, var1 = "Ch4_soc_sum", group = "Bd21")
aov_auto(d.co, var1 = "Ch4_attention_sum", group = "Bd21")
aov_auto(d.co, var1 = "Ch4_withdrawal_sum", group = "Bd21")



# compare co-p-m ----------------------------------------------------------

# data
d.co.m <- df %>% 
        filter(Ft9 %in% c(1, 3)) %>% 
        filter(Bd21 == 1) %>% 
        filter(Bd30 == 1)

# 1. agg
DescTools::LeveneTest(y = d.co.m$Ch4_aggressive_sum, 
                      group = d.co.m$Ft9)
t1 <- d.co.m %>% filter(Ft9 == 1) %>% .$Ch4_aggressive_sum
t2 <- d.co.m %>% filter(Ft9 == 3) %>% .$Ch4_aggressive_sum
t.test(t1, t2, alternative = "less", var.equal = FALSE)
# eff
effsize::cohen.d(as.numeric(Ch4_aggressive_sum) ~ Ft9, 
                 data = d.co.m,
                 pooled = TRUE, 
                 na.rm = TRUE, 
                 paired = FALSE)
# 2. soc
DescTools::LeveneTest(y = d.co.m$Ch4_soc_sum, 
                      group = d.co.m$Ft9)
t1 <- d.co.m %>% filter(Ft9 == 1) %>% .$Ch4_soc_sum
t2 <- d.co.m %>% filter(Ft9 == 3) %>% .$Ch4_soc_sum
t.test(t1, t2, alternative = "less", var.equal = TRUE)
# eff
effsize::cohen.d(as.numeric(Ch4_soc_sum) ~ Ft9, 
                 data = d.co.m,
                 pooled = TRUE, 
                 na.rm = TRUE, 
                 paired = FALSE)
# 3. attention
DescTools::LeveneTest(y = d.co.m$Ch4_attention_sum, 
                      group = d.co.m$Ft9)
t1 <- d.co.m %>% filter(Ft9 == 1) %>% .$Ch4_attention_sum
t2 <- d.co.m %>% filter(Ft9 == 3) %>% .$Ch4_attention_sum
t.test(t1, t2, alternative = "less", var.equal = TRUE)
# eff
effsize::cohen.d(as.numeric(Ch4_attention_sum) ~ Ft9, 
                 data = d.co.m,
                 pooled = TRUE, 
                 na.rm = TRUE, 
                 paired = FALSE)
# 4. withdrawal
DescTools::LeveneTest(y = d.co.m$Ch4_withdrawal_sum, 
                      group = d.co.m$Ft9)
t1 <- d.co.m %>% filter(Ft9 == 1) %>% .$Ch4_withdrawal_sum       
t2 <- d.co.m %>% filter(Ft9 == 3) %>% .$Ch4_withdrawal_sum
t.test(t1, t2, alternative = "less", var.equal = TRUE)
# eff
effsize::cohen.d(as.numeric(Ch4_withdrawal_sum) ~ Ft9, 
                 data = d.co.m,
                 pooled = TRUE, 
                 na.rm = TRUE, 
                 paired = FALSE)

