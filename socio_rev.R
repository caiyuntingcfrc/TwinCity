
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
         "expss", "PerformanceAnalytics")

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

# filter Ft9, Bd21
# n: 5666
d <- df %>% 
        # caregiver: parents(1), the father(2) and the mother(3)
        filter(Ft9 %in% c(1, 2, 3)) %>% 
        # respondent: the mother(1) and the father(2)
        filter(Bd21 %in% c(1, 2))

# desc_Ch4(CBCL) ----------------------------------------------------------

# select Ch4_
d_ch4 <- d %>% 
        select(matches("^Ch4_.*_sum")) %>% 
        as.data.frame()
# desc
desc_ch4 <- stat.desc(d_ch4, desc = TRUE) %>% 
        round(2) %>% 
        .[-7, ]
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
# freq
userfriendlyscience::freq(d$Ft9, nsmall = 2, 
                          plot = TRUE, 
                          plotTheme = theme_pubclean())

# desc_Bd21 (respondant) --------------------------------------------------

# label
val_lab(d$Bd21) <- num_lab("
                         1 母親
                         2 父親
                         ")
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
# freq
userfriendlyscience::freq(d$Bd23, nsmall = 2, 
                          plot = TRUE, 
                          plotTheme = theme_pubclean())

# desc_Bd26 (edu) ---------------------------------------------------------

# label
val_lab(d$Bd26) <- num_lab("
                         1 國中及以下
                         2 高中（職）
                         3 專科
                         4 大學
                         5 碩士
                         6 博士
                         ")
# freq
userfriendlyscience::freq(d$Bd26, nsmall = 2, 
                          plot = TRUE, 
                          plotTheme = theme_pubclean())


# desc_Bd31 (spouse edu) --------------------------------------------------

# label
val_lab(d$Bd31) <- num_lab("
                         1 國中及以下
                         2 高中（職）
                         3 專科
                         4 大學
                         5 碩士
                         6 博士
                         ")
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
# freq
userfriendlyscience::freq(d$Bd35, nsmall = 2, 
                          plot = TRUE, 
                          plotTheme = theme_pubclean())

# desc_grade (grade) ------------------------------------------------------

val_lab(d$grade) <- num_lab("
                            1 一年級
                            3 三年級
                            5 五年級
                            ")
# freq
userfriendlyscience::freq(d$grade, nsmall = 2, 
                          plot = TRUE, 
                          plotTheme = theme_pubclean())


# desc_Bd43 (ISEL) --------------------------------------------------------
# Interpersonal Support Evaluation scale
# freq
userfriendlyscience::freq(d$Bd43_sum, nsmall = 2, 
                          plot = TRUE, 
                          plotTheme = theme_pubclean())


# recode_socio (3 groups) --------------------------------------------------

# 3 groups (18, 12~18, <12)
d <- d %>% 
        # mutate(socio_sd = socio / sd(socio, na.rm = TRUE)) %>%
        mutate(socio = case_when(Bd43_sum < 12 ~ 1L,
                                 Bd43_sum >= 12 & Bd43_sum < 18 ~ 2L,
                                 Bd43_sum == 18 ~ 3L,
                                 TRUE ~ NA_integer_))

# desc_socio --------------------------------------------------------------

# freq
userfriendlyscience::freq(d$socio, nsmall = 2, 
                          plot = TRUE, 
                          plotTheme = theme_pubclean())


# regression-1 ------------------------------------------------------------

var_lab(d$Ft9) <- "主要照顧者"
str(d$Ft9)

d1 <- d %>% 
        select(Bd26, Bd31, 
               Bd32, Bd35, Bd43_sum) %>% 
        mutate_if(is.factor, as.numeric)
chart.Correlation(d1)

