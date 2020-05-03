
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
         "expss")

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

# desc-filter -------------------------------------------------------------

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
                        1 ����
                        2 ����
                        3 ����
                        ")
# freq
userfriendlyscience::freq(d$Ft9, nsmall = 2, 
                          plot = TRUE, 
                          plotTheme = theme_pubclean())

# desc_Bd21 (respondant) --------------------------------------------------

# label
val_lab(d$Bd21) <- num_lab("
                         1 ����
                         2 ����
                         ")
# freq
userfriendlyscience::freq(d$Bd21, nsmall = 2, 
                          plot = TRUE, 
                          plotTheme = theme_pubclean())

# desc_Bd23 (gender) ------------------------------------------------------

# label
val_lab(d$Bd23) <- num_lab("
                         1 �k
                         2 �k
                         ")
# freq
userfriendlyscience::freq(d$Bd23, nsmall = 2, 
                          plot = TRUE, 
                          plotTheme = theme_pubclean())

# desc_Bd26 (edu) ---------------------------------------------------------

# label
val_lab(d$Bd26) <- num_lab("
                         1 �ꤤ�ΥH�U
                         2 �����]¾�^
                         3 �M��
                         4 �j��
                         5 �Ӥh
                         6 �դh
                         ")
# freq
userfriendlyscience::freq(d$Bd26, nsmall = 2, 
                          plot = TRUE, 
                          plotTheme = theme_pubclean())


# desc_Bd31 (spouse edu) --------------------------------------------------

# label
val_lab(d$Bd31) <- num_lab("
                         1 �ꤤ�ΥH�U
                         2 �����]¾�^
                         3 �M��
                         4 �j��
                         5 �Ӥh
                         6 �դh
                         ")
# freq
userfriendlyscience::freq(d$Bd31, nsmall = 2, 
                          plot = TRUE, 
                          plotTheme = theme_pubclean())

# desc_Bd27 (employment) --------------------------------------------------

# label
val_lab(d$Bd27) <- num_lab("
                         1 �b¾
                         2 �a�x�D�� / ��
                         3 �ݷ~
                         4 �Q���� / �Q�껺
                         5 �b�Ǥ�
                         6 �h��
                         7 �ȮɩΥä[�ʴݯe
                         8 ��L�A�л���
                         9 �P�ɦb¾�ήa�x�D�� / ��
                         ")
# freq
userfriendlyscience::freq(d$Bd27, nsmall = 2, 
                          plot = TRUE, 
                          plotTheme = theme_pubclean())


# desc_Bd32 (spouse employment) -------------------------------------------

# label
val_lab(d$Bd32) <- num_lab("
                         1 �b¾
                         2 �a�x�D�� / ��
                         3 �ݷ~
                         4 �Q���� / �Q�껺
                         5 �b�Ǥ�
                         6 �h��
                         7 �ȮɩΥä[�ʴݯe
                         8 ��L�A�л���
                         9 �P�ɦb¾�ήa�x�D�� / ��
                         ")
# freq
userfriendlyscience::freq(d$Bd32, nsmall = 2, 
                          plot = TRUE, 
                          plotTheme = theme_pubclean())


# desc_Bd35 (family income) -----------------------------------------------

# label
val_lab(d$Bd35) <- num_lab("
                         1 �֩�30�U��
                         2 30~49�U��
                         3 50~69�U��
                         4 70~89�U��
                         5 90~109�U��
                         6 110~129�U��
                         7 130~149�U��
                         8 150~169�U��
                         9 170~199�U��
                         10 200~249�U��
                         11 250�U���H�W
                         ")
# freq
userfriendlyscience::freq(d$Bd35, nsmall = 2, 
                          plot = TRUE, 
                          plotTheme = theme_pubclean())

# desc_grade (grade) ------------------------------------------------------

