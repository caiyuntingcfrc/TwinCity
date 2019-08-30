
# prep --------------------------------------------------------------------

rm(list = ls())
cat("\014")
library(tidyverse)
library(magrittr)
source("C:/Users/user/Documents/Github_CFRC/TwinCity/AOV_auto.R")
source("C:/Users/user/Documents/Github_CFRC/TwinCity/t_auto.R")

# read data file ----------------------------------------------------------

load("D:/R_wd/twin cities/working/Taipei_all_tmp.RData")

# df <- haven::read_sav("Twin Cities/working/11. Taipei_all_temp.sav", 
               # encoding = "UTF-8")
# 
# save(df, file = "Taipei_all_tmp.RData")

# standardized scores -----------------------------------------------------

        # mutate(Ch4_aggressive_sum_s = scale(Ch4_aggressive_sum)[ , 1],
        #        Ch4_soc_sum_s = scale(Ch4_soc_sum)[ , 1],
        #        Ch4_attention_sum_s = scale(Ch4_attention_sum)[ , 1],
        #        Ch4_soc_sum_s = scale(Ch4_soc_sum)[ , 1])


# [1] (Ch4) compare between caregivers ------------------------------------

# data
d.care <- df %>% 
        # care givers are both, the father and the mother
        filter(Ft9 %in% c(1, 2, 3)) %>% 
        # respondants are the father and the mother
        filter(Bd21 %in% c(1, 2)) %>% 
        # marital status: married
        filter(Bd30 == 1) %>% 
        # as.factor
        mutate_at(vars(matches("Bd21")), as.factor)
# check class
class(d.care$Ft9); str(d.care$Ft9)
class(d.care$Bd21); str(d.care$Bd21)

# 1. aggressive -----------------------------------------------------------

aov_auto(data = d.care, var1 = "Ch4_aggressive_sum", group = "Ft9")

# ggbetweenstats(data = d1, 
#                x = Ft9, 
#                y = Ch4_aggressive_sum_s, 
#                type = "p", 
#                pairwise.comparisons = TRUE, 
#                pairwise.display = TRUE,
#                p.adjust.method = "none")


# 2. soc ------------------------------------------------------------------

aov_auto(data = d.care, var1 = "Ch4_soc_sum", group = "Ft9")

# 3. attention ------------------------------------------------------------

aov_auto(data = d.care, var1 = "Ch4_attention_sum", group = "Ft9")

# 4. withdrawal -----------------------------------------------------------

aov_auto(data = d.care, var1 = "Ch4_withdrawal_sum", group = "Ft9")

# 5. Ft8 ------------------------------------------------------------------

# achievement
aov_auto(data = d.care, var1 = "Ft8_achievement_sum", group = "Ft9")
# warmth
aov_auto(data = d.care, var1 = "Ft8_warmth_sum", group = "Ft9")
# monitoring
aov_auto(data = d.care, var1 = "Ft8_monitoring_sum", group = "Ft9")
# harsh
aov_auto(data = d.care, var1 = "Ft8_harsh_sum", group = "Ft9")
# indulgence
aov_auto(data = d.care, var1 = "Ft8_indulgence_sum", group = "Ft9")
# autonomy
aov_auto(data = d.care, var1 = "Ft8_autonomy_sum", group = "Ft9")

# [2] (Ch4) compare within co-parenting -----------------------------------

# data
d.co <- df %>% 
        # caregivers are both
        filter(Ft9 == 1) %>% 
        # respondants are mother(1) and father(2)
        filter(Bd21 %in% c(1, 2)) %>% 
        # marital status: married
        filter(Bd30 == 1)

# group size
d.co %>% group_by(Bd21) %>% group_size()

# 1. agg ------------------------------------------------------------------

t_auto(data = d.co, 
       var = "Ch4_aggressive_sum", 
       group = "Bd21", 
       alternative = "less")

# effect size
effsize::cohen.d(as.numeric(Ch4_aggressive_sum) ~ as.factor(Bd21), 
                 data = d.co,
                 pooled = TRUE, 
                 na.rm = TRUE, 
                 paired = FALSE, 
                 hedges.correction = F)

# 2. soc ------------------------------------------------------------------

t_auto(data = d.co,
       var = "Ch4_soc_sum", 
       group = "Bd21", 
       alternative = "less")

# effect size
effsize::cohen.d(as.numeric(Ch4_soc_sum) ~ Bd21, 
                 data = d.co,
                 pooled = TRUE, 
                 na.rm = TRUE, 
                 paired = FALSE)

# 3. attention ------------------------------------------------------------

t_auto(data = d.co,
       var = "Ch4_attention_sum", 
       group = "Bd21", 
       alternative = "less")

# effect size
effsize::cohen.d(as.integer(Ch4_attention_sum) ~ Bd21, 
                 data = d.co,
                 na.rm = TRUE, 
                 paired = FALSE)

# 4. withdrawal -----------------------------------------------------------

t_auto(data = d.co,
       var = "Ch4_withdrawal_sum", 
       group = "Bd21", 
       alternative = "less")

# effect size
effsize::cohen.d(as.integer(Ch4_withdrawal_sum) ~ Bd21, 
                 data = d.co,
                 na.rm = TRUE, 
                 paired = FALSE)


# [3] (Ch4) compare both with the mother (within the mother respondant) --------------

# data
d.co.m <- df %>% 
        # caregivers are both and the mother
        filter(Ft9 %in% c(1, 3)) %>% 
        # respondant is the mother
        filter(Bd21 == 1) %>% 
        # marital status: married
        filter(Bd30 == 1)

# 1. agg ------------------------------------------------------------------
t_auto(data = d.co.m, 
       var = "Ch4_aggressive_sum", 
       group = "Ft9", 
       alternative = "less")

# effect size
effsize::cohen.d(as.numeric(Ch4_aggressive_sum) ~ Ft9, 
                 data = d.co.m,
                 pooled = TRUE, 
                 na.rm = TRUE, 
                 paired = FALSE)

# 2. soc ------------------------------------------------------------------
t_auto(data = d.co.m, 
       var = "Ch4_soc_sum", 
       group = "Ft9", 
       alternative = "less")
# effect size
effsize::cohen.d(as.numeric(Ch4_soc_sum) ~ Ft9, 
                 data = d.co.m,
                 pooled = TRUE, 
                 na.rm = TRUE, 
                 paired = FALSE)

# 3. attention ------------------------------------------------------------
t_auto(data = d.co.m, 
       var = "Ch4_attention_sum", 
       group = "Ft9", 
       alternative = "less")

# effect size
effsize::cohen.d(as.numeric(Ch4_attention_sum) ~ Ft9, 
                 data = d.co.m,
                 pooled = TRUE, 
                 na.rm = TRUE, 
                 paired = FALSE)

# 4. withdrawal -----------------------------------------------------------
t_auto(data = d.co.m, 
       var = "Ch4_withdrawal_sum", 
       group = "Ft9", 
       alternative = "less")
# effect size
effsize::cohen.d(as.numeric(Ch4_withdrawal_sum) ~ Ft9, 
                 data = d.co.m,
                 pooled = TRUE, 
                 na.rm = TRUE, 
                 paired = FALSE)




# [*] two-way ANOVA (Ft9, Bd21) -------------------------------------------

# 1. agg ------------------------------------------------------------------

d.care <- df %>% 
        mutate_at(vars(matches("Ft9|Bd21")), as.factor)

aov(Ch4_aggressive_sum ~ Ft9 * Bd21, data = d.care.soc) %>% summary()
aov(Ch4_aggressive_sum ~ Bd21 * Ft9, data = d.care.soc) %>% summary()
car::Anova(aov(Ch4_aggressive_sum ~ Ft9 * Bd21, data = d.care.soc), 
           type = "II")
car::Anova(aov(Ch4_aggressive_sum ~ Bd21 * Ft9, data = d.care.soc), 
           type = "II")

d <- d.care.soc %>% 
        filter(!is.na(Ch4_aggressive_sum) & 
                       !is.na(Ft9) &
                       !is.na(Bd21) & 
                       !is.na(Bd43_G))
DescTools::PostHocTest(aov(Ch4_aggressive_sum ~ Ft9 * Bd43_G, data = d), 
                       method = "scheffe")

# 2. soc ------------------------------------------------------------------

aov(Ch4_soc_sum ~ Ft9 * Bd21, data = d.care.soc) %>% summary()
car::Anova(aov(Ch4_soc_sum ~ Ft9 + Bd21, data = d.care.soc), 
           type = "II")

d <- d.care.soc %>% 
        filter(!is.na(Ch4_soc_sum) & 
                       !is.na(Ft9) &
                       !is.na(Bd21)) %>% 
        mutate_at(vars(matches("Ft9|Bd21")), as.factor)
DescTools::PostHocTest(aov(Ch4_soc_sum ~ Ft9 + Bd21, data = d), 
                       method = "scheffe")

# 3. attention ------------------------------------------------------------

aov(Ch4_attention_sum ~ Ft9 * Bd21, data = d.care.soc) %>% summary()
car::Anova(aov(Ch4_attention_sum ~ Ft9 + Bd21, data = d.care.soc), 
           type = "II")

d <- d.care.soc %>% 
        filter(!is.na(Ch4_attention_sum) & 
                       !is.na(Ft9) &
                       !is.na(Bd21)) %>% 
        mutate_at(vars(matches("Ft9|Bd21")), as.factor)
DescTools::PostHocTest(aov(Ch4_attention_sum ~ Ft9 + Bd21, data = d), 
                       method = "scheffe")

# 4. withdrawal -----------------------------------------------------------

aov(Ch4_withdrawal_sum ~ Ft9 * Bd21, data = d.care.soc) %>% summary()
car::Anova(aov(Ch4_withdrawal_sum ~ Ft9 + Bd21, data = d.care.soc), 
           type = "II")

d <- d.care.soc %>% 
        filter(!is.na(Ch4_withdrawal_sum) & 
                       !is.na(Ft9) &
                       !is.na(Bd21)) %>% 
        mutate_at(vars(matches("Ft9|Bd21")), as.factor)
DescTools::PostHocTest(aov(Ch4_withdrawal_sum ~ Ft9 + Bd21, data = d), 
                       method = "scheffe")

# [4] (Ft15) reading books ------------------------------------------------

# 1. compare between caregivers -------------------------------------------

# data = d.care
aov_auto(data = d.care, 
         var1 = "Ft15", 
         group = "Ft9")

# 2. compare within co-parenting ------------------------------------------

# 
t_auto(data = d.co, 
       var = "Ft15", 
       group = "Bd21", 
       alternative = "greater")


# [5] Bd43 ----------------------------------------------------------------


# 1. compare between caregivers -------------------------------------------

# 1 > 3 > 2 (both > the mother > the father)
aov_auto(data = d.care, 
         var1 = "Bd43_sum", 
         group = "Ft9")

# 2. correlation between Bd43 and Ch4 -------------------------------------

# data
d.corr <- df %>% 
        # care givers are both, the father and the mother
        filter(Ft9 %in% c(1, 2, 3)) %>% 
        # respondants are the father and the mother
        filter(Bd21 %in% c(1, 2)) %>% 
        # marital status: married
        filter(Bd30 == 1) %>% 
        # select
        select(matches("Ch4_.*_sum|Bd43_sum"))
# no strong correlation
PerformanceAnalytics::chart.Correlation(d.corr, 
                                        histogram = FALSE, 
                                        method = "spearman")

# [6] test interaction (Ft9 * Bd43) ---------------------------------------

d.care <- d.care
# group social support
d.care.soc <- d.care %>% 
        filter(!(Ft9 == 2 )) %>% 
        mutate(Bd43_G = case_when(Bd43_sum >= 6 & Bd43_sum < 10 ~ 1, 
                                  Bd43_sum >= 10 & Bd43_sum < 14 ~ 2, 
                                  Bd43_sum >= 14 & Bd43_sum < 18 ~ 3, 
                                  TRUE ~ NA_real_)) %>% 
        # as.factor
        mutate_at(vars(matches("Bd43_G|Ft9")), as.factor)
table(d.care.soc$Bd43_G)
d.care.soc.2 <- d.care %>% 
        filter(!(Ft9 == 2)) %>% 
        mutate(Bd43_G = case_when(Bd43_sum >= 6 & Bd43_sum < 15 ~ 1, 
                                  Bd43_sum >= 15 & Bd43_sum < 18 ~ 2, 
                                  TRUE ~ NA_real_)) %>% 
        # as.factor
        mutate_at(vars(matches("Bd43_G")), as.factor)
table(d.care.soc.2$Bd43_G)
# 1. agg ------------------------------------------------------------------

# shows no interaction
aov(Ch4_aggressive_sum ~ Ft9 * Bd43_G, data = d.care.soc.2) %>% summary()
# type 1 (seqrencial)
aov(Ch4_aggressive_sum ~ Ft9 + Bd43_G, data = d.care.soc.2) %>% summary()
# type 2
car::Anova(aov(Ch4_aggressive_sum ~ Ft9 * Bd43_G, data = d.care.soc), 
           type = "II")
a <- aov(Ch4_aggressive_sum ~ Ft9 * Bd43_G, data = d.care.soc); summary(a)
DescTools::PostHocTest(a, ordered = TRUE, method = "scheffe")

# 2. soc ------------------------------------------------------------------

aov(Ch4_soc_sum ~ Ft9 * Bd43_G, data = d.care.soc.2) %>% summary()
# type 1 (seqrencial)
aov(Ch4_soc_sum ~ Ft9 + Bd43_G, data = d.care.soc.2) %>% summary()
# type 2
car::Anova(aov(Ch4_soc_sum ~ Ft9 * Bd43_G, data = d.care.soc), 
           type = "II")
a <- aov(Ch4_soc_sum ~ Ft9 * Bd43_G, data = d.care.soc); summary(a)
DescTools::PostHocTest(a, ordered = TRUE, method = "scheffe")

# 3. attentioni -----------------------------------------------------------

aov(Ch4_attention_sum ~ Ft9 * Bd43_G, data = d.care.soc) %>% summary()
# type 1 (seqrencial)
aov(Ch4_attention_sum ~ Ft9 + Bd43_G, data = d.care.soc) %>% summary()
# type 2
car::Anova(aov(Ch4_attention_sum ~ Ft9 + Bd43_G, data = d.care.soc), 
           type = "II")


# 4. withdrawal -----------------------------------------------------------

aov(Ch4_withdrawal_sum ~ Ft9 * Bd43_G, data = d.care.soc) %>% summary()
# type 1 (seqrencial)
aov(Ch4_withdrawal_sum ~ Ft9 + Bd43_G, data = d.care.soc) %>% summary()
# type 2
car::Anova(aov(Ch4_withdrawal_sum ~ Ft9 * Bd43_G, data = d.care.soc), 
           type = "II")


# [*] control variable Bd43 -----------------------------------------------

# 1. high -----------------------------------------------------------------
d.care.43 <- d.care.soc %>% 
        filter(Bd43_G == 3)
aov_auto(data = d.care.43, var1 = "Ch4_aggressive_sum", group = "Ft9")
aov_auto(data = d.care.43, var1 = "Ch4_soc_sum", group = "Ft9")
aov_auto(data = d.care.43, var1 = "Ch4_attention_sum", group = "Ft9")
aov_auto(data = d.care.43, var1 = "Ch4_withdrawal_sum", group = "Ft9")

d.care.43 <- d.care.soc.2 %>% 
        filter(Bd43_G == 2)
aov_auto(data = d.care.43, var1 = "Ch4_aggressive_sum", group = "Ft9")
aov_auto(data = d.care.43, var1 = "Ch4_soc_sum", group = "Ft9")
aov_auto(data = d.care.43, var1 = "Ch4_attention_sum", group = "Ft9")
aov_auto(data = d.care.43, var1 = "Ch4_withdrawal_sum", group = "Ft9")


# 2. medium ---------------------------------------------------------------
d.care.43 <- d.care.soc %>% 
        filter(Bd43_G == 2)
# sig
aov_auto(var1 = "Ch4_aggressive_sum", group = "Ft9", data = d.care.43)
aov_auto(data = d.care.43, var1 = "Ch4_soc_sum", group = "Ft9")
aov_auto(var1 = "Ch4_attention_sum", group = "Ft9", data = d.care.43)
aov_auto(var1 = "Ch4_withdrawal_sum", group = "Ft9", data = d.care.43)

d.care.43 <- d.care.soc.2 %>% 
        filter(Bd43_G == 1)
# sig
aov_auto(data = d.care.43, var1 = "Ch4_aggressive_sum", group = "Ft9")
aov_auto(data = d.care.43, var1 = "Ch4_soc_sum", group = "Ft9")
aov_auto(var1 = "Ch4_attention_sum", group = "Ft9", data = d.care.43)
aov_auto(var1 = "Ch4_withdrawal_sum", group = "Ft9", data = d.care.43)

# 3. low ------------------------------------------------------------------
d.care.43 <- d.care.soc %>% 
        filter(Bd43_G == 1)
aov_auto(data = d.care.43, var1 = "Ch4_aggressive_sum", group = "Ft9")
aov_auto(data = d.care.43, var1 = "Ch4_soc_sum", group = "Ft9")
aov_auto(var1 = "Ch4_attention_sum", group = "Ft9", data = d.care.43)
aov_auto(var1 = "Ch4_withdrawal_sum", group = "Ft9", data = d.care.43)
