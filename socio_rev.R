
# prep and options --------------------------------------------------------
# rm
rm(list = ls()); cat("\14")
source("~/Github/misc/func_ins.pack.R")
# setwd
setwd("i:/R_wd/")
# option
options(scipen = 999)
# ins.pak
ins.pack("tidyverse", "feather", "ggpubr", "ggExtra", 
         "sjstats", "pastecs", "stargazer", 
         "expss", "PerformanceAnalytics", "car", 
         "lm.beta")

# read data file ----------------------------------------------------------

df <- read_feather("Twin Cities Data/tp_all.feather")

# # desc-vanilla ------------------------------------------------------------
# 
# # Ch4_
# d <- df %>% 
#         select(matches("^Ch4_.*_sum")) %>% 
#         as.data.frame()
# # desc
# desc <- stat.desc(d, desc = TRUE) %>% 
#         round(2) %>% 
#         .[-7, ]
# 
# print(desc)
# 
# # plots-vanilla -----------------------------------------------------------
# 
# # boxplot
# boxplot(d)
# 
# # bar plot
# p1 <- ggplot(d, aes(x = Ch4_aggressive_sum)) + 
#         geom_bar() + 
#         labs(x = NULL, 
#              title = "aggressive_sum") +
#         theme_pubclean()
# p1
# 
# p2 <- ggplot(d, aes(x = Ch4_attention_sum)) + 
#         geom_bar() + 
#         labs(x = NULL, 
#              title = "attention_sum") +
#         theme_pubclean()
# 
# p3 <- ggplot(d, aes(x = Ch4_soc_sum)) + 
#         geom_bar() + 
#         labs(x = NULL, 
#              title = "social_sum") +
#         theme_pubclean()
# 
# p4 <- ggplot(d, aes(x = Ch4_withdrawal_sum)) + 
#         geom_bar() + 
#         labs(x = NULL, 
#              title = "withdrawal_sum") +
#         theme_pubclean()
# 
# gridExtra::grid.arrange(p1, p2, p3, p4, nrow = 2)
# 


# desc_filter -------------------------------------------------------------

# filter Ft9, Bd21, Bd30
# n: 5129
d <- df %>% 
        # caregiver: parents(1), the father(2) and the mother(3)
        filter(Ft9 %in% c(1, 2, 3)) %>% 
        # respondent: the mother(1) and the father(2)
        filter(Bd21 %in% c(1, 2)) %>% 
        # married
        filter(Bd30 == 1)
        # Bd34_sum < 12
        # filter(Bd43_sum < 8)

# desc_Ch4(CBCL) ----------------------------------------------------------

var_lab(d$Ch4_aggressive_sum) <- "攻擊行為"
var_lab(d$Ch4_soc_sum) <- "社交問題"
var_lab(d$Ch4_attention_sum) <- "注意力問題"
var_lab(d$Ch4_withdrawal_sum) <- "退縮"

# select Ch4_
d_ch4 <- d %>% 
        select(matches("^Ch4_.*_sum")) %>% 
        as.data.frame()


# desc
desc_ch4 <- stat.desc(d_ch4, desc = TRUE) %>% 
        round(2) %>% 
        .[-7, ]
print(desc_ch4)

# boxplot
boxplot(d_ch4)

# bar plot
p1 <- ggplot(d, aes(x = Ch4_aggressive_sum)) + 
        geom_bar() + 
        labs(title = "aggressive") +
        theme_pubclean()

p2 <- ggplot(d, aes(x = Ch4_attention_sum)) + 
        geom_bar() + 
        labs(title = "attention") +
        theme_pubclean()

p3 <- ggplot(d, aes(x = Ch4_soc_sum)) + 
        geom_bar() + 
        labs(title = "social") +
        theme_pubclean()

p4 <- ggplot(d, aes(x = Ch4_withdrawal_sum)) + 
        geom_bar() + 
        labs(title = "withdrawal") +
        theme_pubclean()

gridExtra::grid.arrange(p1, p2, p3, p4, nrow = 2)


# desc_Ft9 (caregiver) ----------------------------------------------------
# label
val_lab(d$Ft9) <- num_lab("
                        1 父母
                        2 父親
                        3 母親
                        ")
# factor
d$Ft9 <- as.factor(d$Ft9)

# label
var_lab(d$Ft9) <- "主要照顧者"

# freq
userfriendlyscience::freq(d$Ft9, nsmall = 2, 
                          plot = TRUE, 
                          plotTheme = theme_pubclean())

# desc_Bd21 (respondant) --------------------------------------------------

# label
var_lab(d$Bd21) <- "填答者"
val_lab(d$Bd21) <- num_lab("
                         1 母親
                         2 父親
                         ")

# factor
d$Bd21 <- as.factor(d$Bd21)
# freq
userfriendlyscience::freq(d$Bd21, nsmall = 2, 
                          plot = TRUE, 
                          plotTheme = theme_pubclean())

# desc_Bd23 (gender) ------------------------------------------------------

# label
val_lab(d$Bd23) <- num_lab("
                         1 女
                         2 男
                         ")
# factor
d$Bd23 <- as.factor(d$Bd23)

# label
var_lab(d$Bd23) <- "性別"

# freq
userfriendlyscience::freq(d$Bd23, nsmall = 2, 
                          plot = TRUE, 
                          plotTheme = theme_pubclean())

# desc_Bd26 (edu) ---------------------------------------------------------

# label
var_lab(d$Bd26) <- "教育程度"
val_lab(d$Bd26) <- num_lab("
                         1 國中及以下
                         2 高中（職）
                         3 專科
                         4 大學
                         5 碩士
                         6 博士
                         ")
# factor
# d$Bd26 <- as.factor(d$Bd26)

# freq
userfriendlyscience::freq(d$Bd26, nsmall = 2, 
                          plot = TRUE, 
                          plotTheme = theme_pubclean())


# desc_Bd31 (spouse edu) --------------------------------------------------

# label
var_lab(d$Bd31) <- "配偶教育程度"
val_lab(d$Bd31) <- num_lab("
                         1 國中及以下
                         2 高中（職）
                         3 專科
                         4 大學
                         5 碩士
                         6 博士
                         ")
# factor
d$Bd31 <- as.factor(d$Bd31)

# freq
userfriendlyscience::freq(d$Bd31, nsmall = 2, 
                          plot = TRUE, 
                          plotTheme = theme_pubclean())

# desc_Bd27 (employment) --------------------------------------------------

# label
val_lab(d$Bd27) <- num_lab("
                         1 在職
                         2 家庭主婦 / 夫
                         3 待業
                         4 被裁員 / 被資遣
                         5 在學中
                         6 退休
                         7 暫時或永久性殘疾
                         8 其他，請說明
                         9 同時在職及家庭主婦 / 夫
                         ")

# factor
d$Bd27 <- as.factor(d$Bd27)

# freq
userfriendlyscience::freq(d$Bd27, nsmall = 2, 
                          plot = TRUE, 
                          plotTheme = theme_pubclean())


# desc_Bd32 (spouse employment) -------------------------------------------

# label
val_lab(d$Bd32) <- num_lab("
                         1 在職
                         2 家庭主婦 / 夫
                         3 待業
                         4 被裁員 / 被資遣
                         5 在學中
                         6 退休
                         7 暫時或永久性殘疾
                         8 其他，請說明
                         9 同時在職及家庭主婦 / 夫
                         ")

# factor
d$Bd32 <- as.factor(d$Bd32)

# freq
userfriendlyscience::freq(d$Bd32, nsmall = 2, 
                          plot = TRUE, 
                          plotTheme = theme_pubclean())


# desc_Bd35 (family income) -----------------------------------------------

# label
val_lab(d$Bd35) <- num_lab("
                         1 少於30萬元
                         2 30~49萬元
                         3 50~69萬元
                         4 70~89萬元
                         5 90~109萬元
                         6 110~129萬元
                         7 130~149萬元
                         8 150~169萬元
                         9 170~199萬元
                         10 200~249萬元
                         11 250萬元以上
                         ")
# label
var_lab(d$Bd35) <- "家庭收入"

# freq
userfriendlyscience::freq(d$Bd35, nsmall = 2, 
                          plot = TRUE, 
                          plotTheme = theme_pubclean())

# desc_grade (grade) ------------------------------------------------------
# label
val_lab(d$grade) <- num_lab("
                            1 一年級
                            3 三年級
                            5 五年級
                            ")
# factor
d$grade <- as.factor(d$grade)

# label
var_lab(d$grade) <- "年級"

# freq
userfriendlyscience::freq(d$grade, nsmall = 2, 
                          plot = TRUE, 
                          plotTheme = theme_pubclean())


# desc_Bd43 (ISEL) --------------------------------------------------------
# Interpersonal Support Evaluation scale

# label
var_lab(d$Bd43_sum) <- "社會支持總分"
# freq
userfriendlyscience::freq(d$Bd43_sum, nsmall = 2, 
                          plot = TRUE, 
                          plotTheme = theme_pubclean())


# desc_SF -----------------------------------------------------------------

val_lab(d$Sf) <- num_lab("  1 單親
                            2 核心
                            3 三代
                            4 祖孫
                            5 其他
                            6 單親且同居
                            ")
# factor
d$Sf <- as.factor(d$Sf) 

var_lab(d$Sf) <- "家庭結構"

# freq
userfriendlyscience::freq(d$Sf, nsmall = 2, 
                          plot = TRUE, 
                          plotTheme = theme_pubclean())


# desc_realage ------------------------------------------------------------

# label
var_lab(d$realage) <- "實際年齡"

# recode live with others -------------------------------------------------

d <- d %>% 
        mutate(with_others = case_when(Sf == 1 ~ 0, 
                                       !(Sf == 1) ~ 1, 
                                       TRUE ~ NA_real_))
val_lab(d$with_others) <- num_lab("
                                        1 是
                                        0 否
                                  ")
# factor
d$with_others <- as.factor(d$with_others) 
# label
var_lab(d$with_others) <- "與他人同住"
# freq
userfriendlyscience::freq(d$with_others, nsmall = 2, 
                          plot = TRUE, 
                          plotTheme = theme_pubclean())

# recode_socio (3 groups) --------------------------------------------------

# 3 groups (18, 12~18, <12)
d <- d %>% 
        # mutate(socio_sd = socio / sd(socio, na.rm = TRUE)) %>%
        mutate(socio = case_when(Bd43_sum < 12 ~ 1,
                                 Bd43_sum >= 12 & Bd43_sum < 18 ~ 2,
                                 Bd43_sum == 18 ~ 3,
                                 TRUE ~ NA_real_))


# desc_socio --------------------------------------------------------------
# label
val_lab(d$socio) <- num_lab("
                            1 低
                            2 中
                            3 高
                            ")
# factor
d$socio <- as.factor(d$socio)
# label
var_lab(d$socio) <- "社會支持(低中高)"

# freq
userfriendlyscience::freq(d$socio, nsmall = 2, 
                          plot = TRUE, 
                          plotTheme = theme_pubclean())

# chart.Correlation(d)


# recode income -----------------------------------------------------------

d <- d %>% 
        mutate(income = case_when(Bd35 <= 5 ~ 0, 
                                  Bd35 > 5 ~ 1, 
                                  TRUE ~ NA_real_))
val_lab(d$income) <- num_lab("
                            0 低
                            1 高
                            ")
d$income <- as.factor(d$income)
var_lab(d$income) <- "家庭收入"
userfriendlyscience::freq(d$income, plot = TRUE)


# model -------------------------------------------------------------------
# aggressive
d_model1 <- d %>% 
        # filter(socio == "低") %>%
        select(Ch4_aggressive_sum, Ft9, Bd23, Bd35, realage, Bd43_sum, Bd26) %>% 
        na.omit() %>% 
        as.data.frame()
# lm.null
lm.null <- use_labels(d_model1, lm(Ch4_aggressive_sum ~ 1))
# lm.full
lm.full <- use_labels(d_model1, lm(Ch4_aggressive_sum ~ Ft9 + Bd23 + 
                                           Bd35 + realage + 
                                           Bd43_sum + Bd26))
# stepwise forward
forward.lm <- step(lm.null,  
                   scope = list(lower = lm.null, upper = lm.full), 
                   direction = "forward")
# summary
summary(forward.lm)
lm.beta(forward.lm)

# model -------------------------------------------------------------------
# soc
d_model1 <- d %>% 
        # filter(socio == "低") %>%
        select(Ch4_soc_sum, Ft9, Bd23, Bd35, realage, Bd43_sum, Bd26) %>% 
        na.omit() %>% 
        as.data.frame()
# lm.null
lm.null <- use_labels(d_model1, lm(Ch4_soc_sum ~ 1))
# lm.full
lm.full <- use_labels(d_model1, lm(Ch4_soc_sum ~ Ft9 + Bd23 + 
                                           Bd35 + realage + 
                                           Bd43_sum + Bd26))
# stepwise forward
forward.lm <- step(lm.null,  
                   scope = list(lower = lm.null, upper = lm.full), 
                   direction = "forward")
# summary
summary(forward.lm)
lm.beta(forward.lm)




# model -------------------------------------------------------------------
# attetion
d_model1 <- d %>% 
        # filter(socio == "低") %>%
        select(Ch4_attention_sum, Ft9, Bd23, Bd35, realage, Bd43_sum, Bd26) %>% 
        na.omit() %>% 
        as.data.frame()
# lm.null
lm.null <- use_labels(d_model1, lm(Ch4_attention_sum ~ 1))
# lm.full
lm.full <- use_labels(d_model1, lm(Ch4_attention_sum ~ Ft9 + Bd23 + 
                                           Bd35 + realage + 
                                           Bd43_sum + Bd26))
# stepwise forward
forward.lm <- step(lm.null,  
                   scope = list(lower = lm.null, upper = lm.full), 
                   direction = "forward")
# summary
summary(forward.lm)
lm.beta(forward.lm)

# model -------------------------------------------------------------------
# withdrawal
d_model1 <- d %>% 
        # filter(socio == "低") %>%
        select(Ch4_withdrawal_sum, Ft9, Bd23, Bd35, realage, Bd43_sum, Bd26) %>% 
        na.omit() %>% 
        as.data.frame()
# lm.null
lm.null <- use_labels(d_model1, lm(Ch4_withdrawal_sum ~ 1))
# lm.full
lm.full <- use_labels(d_model1, lm(Ch4_withdrawal_sum ~ Ft9 + Bd23 + 
                                           Bd35 + realage + 
                                           Bd43_sum + Bd26))
# stepwise forward
forward.lm <- step(lm.null,  
                   scope = list(lower = lm.null, upper = lm.full), 
                   direction = "forward")
# summary
summary(forward.lm)
lm.beta(forward.lm)