
# Prep and Options --------------------------------------------------------

# rm
rm(list = ls())
# set working directory
setwd("D:/R_wd/Twin Cities/data/")

# list of packages
list.packages <- c("tidyverse", "magrittr", "haven", 
                   "gridExtra", "summarytools", "tabulizer", 
                   "labelled", "DescTools", "userfriendlyscience", "stargazer", 
                   "sjlabelled", "sjmisc", "sjstats", "ggeffects", "sjPlot", "ggplot2")
# check if the packages are installed
new.packages <- list.packages[!(list.packages %in% installed.packages()[ , "Package"])]
# install new packages
if(length(new.packages)) install.packages(new.packages)
# load the packages
lapply(list.packages, require, character.only = TRUE)
# remove lists
rm(list.packages, new.packages)
# options
options(scipen = 999)

# Read the Data File ------------------------------------------------------
filepath <- "11. Taipei_all.sav"
df <- read_sav(filepath)

# Desc (BMI) --------------------------------------------------------------

# remove NAs and add levels
d <- df
d$bmicat <- d$bmicat_tw
d <- d[!is.na(df$grade), ]
d$grade <- as.factor(d$grade)
d <- d[!is.na(d$bmicat), ]
d$bmicat <- as.factor(d$bmicat)
d <- d[!is.na(d$Bd23), ]
d$Bd23 <- as.factor(d$Bd23)

levels(d$grade) <- c("一年級", "三年級", "五年級")
levels(d$bmicat) <- c("過輕", "正常範圍", "過重", "肥胖")
levels(d$Bd23) <- c("女性", "男性")


# bmi
tiff("plots/BMI.tiff", 1024, 768, compression = "none", res = 110)
ggplot(d, aes(bmicat, y = (..count..) / sum(..count..), fill = bmicat)) + 
        # bar
        geom_bar(stat = "count") + 
        # text prc
        geom_text(aes(label = paste(round((..count..) / sum(..count..) * 100, 2), " %", sep = "" ),
                      y = (..count..) / sum(..count..)), stat= "count", vjust = -2) +
        # text count
        geom_text(aes(label = paste("n = ", ..count.., sep = ""),
                      y = (..count..) / sum(..count..)), stat= "count", vjust = -.5) +
        # labels
        labs(x = "BMI分類", y = "百分比", title = "BMI分類\n") +
        # adjust scales
        scale_y_continuous(labels = scales::percent, expand = c(0, 0), limits = c(0, .8)) +
        # legend
        scale_fill_brewer(name = NULL, palette = "Paired", labels = levels(d$bmicat)) +
        # theme
        theme_sjplot()
dev.off()

# # bmi x grade stacked
# 
# a <- d %>% select("bmicat", "grade") %>% 
#         count(bmicat, grade) %>% 
#         mutate(prc = n / sum(n), ypos = 1 - 0.5 * prc)
# 
# ggplot(a, aes(x = grade, y = prc, fill = bmicat, 
#               label = paste0(sprintf("%1.1f", prc * 100)), "%")) + 
#         # stacked bar
#         geom_col(position = position_stack()) +
#         # adjust scales
#         scale_y_continuous(labels = scales::percent) +
#         # text prc
#         geom_text(position = position_stack(vjust = .5), size = 3)
#         # labels
#         labs(x = "BMI分類", y = "百分比", title = "BMI分類\n") +
# 
#         # legend
#         scale_fill_brewer(name = NULL, palette = "Paired", labels = levels(d$bmicat)) +
#         # theme
#         theme_sjplot()
# dev.off()

# bmi x grade
tiff("plots/BMI_Grade.tiff", 1280, 720, compression = "none", res = 110)
ggplot(d, aes(bmicat, y = ..prop.., fill = factor(..x..), group = grade)) + 
        geom_bar(stat = "count") + 
        # text prc
        geom_text(aes(label = scales::percent(..prop..),
                      y = ..prop.. ), stat= "count", vjust = -2) + 
        # text count
        geom_text(aes(label = paste("n = ", ..count.., sep = ""),
                      y = ..prop.. ), stat= "count", vjust = -.5) + 
        # labels
        labs(x = "BMI分類", y = "百分比", title = "BMI分類 X 年級\n") +
        # adjust scales
        scale_y_continuous(labels = scales::percent, expand = c(0, 0), limits = c(0, .8)) +
        # color brewer
        scale_fill_brewer(name = NULL, palette = "Paired", labels = levels(d$bmicat)) +
        # grid
        facet_grid(~ grade) + 
        # theme
        theme_sjplot()
dev.off()

# grade x bmi
tiff("plots/Grade_Bmi.tiff", 1280, 720, compression = "none", res = 110)
ggplot(d, aes(grade, group = bmicat, y = ..prop.., fill = factor(..x..))) + 
        geom_bar(stat = "count") + 
        # text prc
        geom_text(aes(label = scales::percent(..prop..),
                      y = ..prop.. ), stat= "count", vjust = -2) + 
        # text count
        geom_text(aes(label = paste("n = ", ..count.., sep = ""),
                      y = ..prop.. ), stat= "count", vjust = -.5) + 
        # labels
        labs(x = "年級", y = "百分比", title = "年級 X BMI分類\n") +
        # adjust scales
        scale_y_continuous(labels = scales::percent, expand = c(0, 0), limits = c(0, .8)) +
        # color brewer
        scale_fill_brewer(name = NULL, palette = "Paired", labels = levels(d$grade)) +
        # grid
        facet_grid(~bmicat) + 
        # theme
        theme_sjplot()
dev.off()

# bmi x sex
tiff("plots/BMI_Sex.tiff", 1280, 720, compression = "none", res = 110)
ggplot(d, aes(bmicat, y = ..prop.., fill = factor(..x..), group = Bd23)) + 
        geom_bar(stat = "count") + 
        # text prc
        geom_text(aes(label = scales::percent(..prop..),
                      y = ..prop.. ), stat= "count", vjust = -2) + 
        # text count
        geom_text(aes(label = paste("n = ", ..count.., sep = ""),
                      y = ..prop.. ), stat= "count", vjust = -.5) + 
        # labels
        labs(x = "BMI分類", y = "百分比", title = "BMI分類 X 性別\n") +
        # adjust scales
        scale_y_continuous(labels = scales::percent, expand = c(0, 0), limits = c(0, .8)) +
        # color brewer
        scale_fill_brewer(name = NULL, palette = "Paired", labels = levels(d$bmicat)) +
        # grid
        facet_grid(~Bd23) + 
        # theme
        theme_sjplot()
dev.off()

# sex x bmi
tiff("plots/Sex_bmi.tiff", 1280, 720, compression = "none", res = 110)
ggplot(d, aes(Bd23, y = ..prop.., fill = factor(..x..), group = bmicat)) + 
        geom_bar(stat = "count") + 
        # text prc
        geom_text(aes(label = scales::percent(..prop..),
                      y = ..prop.. ), stat= "count", vjust = -2) + 
        # text count
        geom_text(aes(label = paste("n = ", ..count.., sep = ""),
                      y = ..prop.. ), stat= "count", vjust = -.5) + 
        # labels
        labs(x = "性別", y = "百分比", title = "性別 X BMI分類\n") +
        # adjust scales
        scale_y_continuous(labels = scales::percent, expand = c(0, 0), limits = c(0, .8)) +
        # color brewer
        scale_fill_brewer(name = NULL, palette = "Paired", labels = levels(d$Bd23)) +
        # grid
        facet_grid(~bmicat) + 
        # theme
        theme_sjplot()
dev.off()

# 
dd <- d[!is.na(d$Sf), ]
dd$Sf <- as.factor(dd$Sf)
levels(dd$Sf) <- c("單親家庭", "核心家庭", "三代家庭" , 
                   "祖孫家庭", "其他", "單親且同居")

# Sf
tiff("plots/Sf.tiff", 1024, 768, compression = "none", res = 110)
ggplot(dd, aes(Sf, y = (..count..) / sum(..count..), fill = Sf)) + 
        # bar
        geom_bar(stat = "count") + 
        # text prc
        geom_text(aes(label = paste(round((..count..) / sum(..count..) * 100, 2), " %", sep = "" ),
                      y = (..count..) / sum(..count..)), stat= "count", vjust = -2) +
        # text count
        geom_text(aes(label = paste("n = ", ..count.., sep = ""),
                      y = (..count..) / sum(..count..)), stat= "count", vjust = -.5) +
        # labels
        labs(x = "家庭組織型態", y = "百分比", title = "家庭組織型態\n") +
        # adjust scales
        scale_y_continuous(labels = scales::percent, expand = c(0, 0), limits = c(0, .8)) +
        # legend
        scale_fill_brewer(name = NULL, palette = "Paired", labels = levels(dd$Sf)) +
        # theme
        theme_sjplot()
dev.off()

# bmi x Sf
tiff("plots/BMI_Sf.tiff", 1560, 720, compression = "none", res = 110)
ggplot(dd, aes(bmicat, y = ..prop.., fill = factor(..x..), group = Sf)) + 
        geom_bar(stat = "count") + 
        # text prc
        geom_text(aes(label = scales::percent(..prop..),
                      y = ..prop.. ), stat= "count", vjust = -2) + 
        # text count
        geom_text(aes(label = paste("n = ", ..count.., sep = ""),
                      y = ..prop.. ), stat= "count", vjust = -.5) + 
        # labels
        labs(x = "BMI分類", y = "百分比", title = "BMI分類 X 家庭組織型態\n") +
        # adjust y scales
        scale_y_continuous(labels = scales::percent, expand = c(0, 0), limits = c(0, 1)) +
        # color brewer
        scale_fill_brewer(name = NULL, palette = "Paired", labels = levels(dd$bmicat)) +
        # grid
        facet_grid(~ Sf) + 
        # theme
        theme_sjplot() +
        #customization
        theme(legend.position = "top")
dev.off()

# Sf x bmi
tiff("plots/Sf_Bmi.tiff", 1560, 720, compression = "none", res = 110)
ggplot(dd, aes(Sf, group = bmicat, y = ..prop.., fill = factor(..x..))) + 
        geom_bar(stat = "count") + 
        # text prc
        geom_text(aes(label = scales::percent(..prop..),
                      y = ..prop.. ), stat= "count", vjust = -2) + 
        # text count
        geom_text(aes(label = paste("n = ", ..count.., sep = ""),
                      y = ..prop.. ), stat= "count", vjust = -.5) + 
        # labels
        labs(x = "家庭組織型態", y = "百分比", title = "家庭組織型態 X BMI分類\n") +
        # adjust scales
        scale_y_continuous(labels = scales::percent, expand = c(0, 0), limits = c(0, .8)) +
        # color brewer
        scale_fill_brewer(name = NULL, palette = "Paired", labels = levels(dd$Sf)) +
        # grid
        facet_grid(~bmicat) + 
        # theme
        theme_sjplot() + 
        #customization
        theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1), 
              legend.position = "top")
dev.off()
# legacy ------------------------------------------------------------------
# ctable BMI x grade
sjt.xtab(var.row = df$bmicat, var.col = df$grade,
         var.labels = c("BMI分類", "年級"),
         title = "BMI分類 X 年級\n",
         string.total = "總和",
         show.cell.prc = FALSE, show.row.prc = FALSE, show.col.prc = TRUE,
         show.summary = FALSE, show.legend = TRUE,
         emph.total = TRUE,
         digits = 2,
         encoding = "UTF-8", 
         file = "plots/bmi_grad.doc")
# ctable BMI X Sex
sjt.xtab(var.row = df$bmicat, var.col = df$Bd23,
         var.labels = c("BMI分類", "性別"),
         title = "BMI分類 X 性別\n",
         string.total = "總和",
         show.cell.prc = FALSE, show.row.prc = FALSE, show.col.prc = TRUE,
         show.summary = FALSE, show.legend = TRUE,
         emph.total = TRUE,
         digits = 2,
         encoding = "UTF-8",
         file = "plots/BMIxSex.doc")
# ctable BMI X SF
sjt.xtab(var.row = df$bmicat, var.col = df$Sf,
         var.labels = c("BMI分類", "家庭組織型態"),
         title = "BMI分類 X 家庭組織型態\n",
         string.total = "總和",
         show.cell.prc = FALSE, show.row.prc = FALSE, show.col.prc = TRUE,
         show.summary = FALSE, show.legend = TRUE,
         emph.total = TRUE,
         digits = 2,
         encoding = "UTF-8",
         file = "plots/BMIxSf.doc")


# # Desc (illness) ----------------------------------------------------------
# 
# # illness
# plot_frq(df$illness, 
#          title = "身心狀況分類") + theme_sjplot()
# 
# # ctable illness X grade
# sjt.xtab(var.row = df$illness, var.col = df$grade, 
#          var.labels = c("身心狀況分類", "年級"), 
#          title = "身心狀況分類 X 年級", 
#          string.total = "總和", 
#          show.cell.prc = TRUE, show.row.prc = FALSE, show.col.prc = FALSE,
#          show.summary = FALSE, show.legend = TRUE, 
#          emph.total = TRUE,
#          digits = 2,
#          encoding = "UTF-8", 
#          file = "illnessxGrade.doc")
# # plot illness X grade
# sjp.xtab(x = df$grade, grp = df$illness, 
#          title = "身心狀況分類 X 年級", 
#          show.total = TRUE, show.n = TRUE, show.prc = TRUE) + theme_sjplot()
# # plot grade X illness
# sjp.xtab(x = df$illness, grp = df$grade, 
#          title = "年級 X 身心狀況分類", 
#          show.total = TRUE, show.n = TRUE, show.prc = TRUE) + theme_sjplot()
# 
# # ctable illness X sex
# sjt.xtab(var.row = df$illness, var.col = df$Bd23, 
#          var.labels = c("身心狀況分類", "性別"), 
#          title = "身心狀況分類 X 性別", 
#          string.total = "總和", 
#          show.cell.prc = TRUE, show.row.prc = FALSE, show.col.prc = FALSE,
#          show.summary = FALSE, show.legend = TRUE, 
#          emph.total = TRUE,
#          digits = 2,
#          encoding = "UTF-8", 
#          file = "illnessxSex.doc")
# # plot illness X Sex
# sjp.xtab(x = df$Bd23, grp = df$illness, 
#          title = "身心狀況分類 X 性別", 
#          show.total = TRUE, show.n = TRUE, show.prc = TRUE) + theme_sjplot()
# # plot Sex X illness
# sjp.xtab(x = df$illness, grp = df$Bd23, 
#          title = "性別 X 身心狀況分類", 
#          show.total = TRUE, show.n = TRUE, show.prc = TRUE) + theme_sjplot()
# 
# # ctable illness X Sf
# sjt.xtab(var.row = df$illness, var.col = df$Sf, 
#          var.labels = c("身心狀況分類", "家庭組織型態"), 
#          title = "身心狀況分類 X 家庭組織型態", 
#          string.total = "總和", 
#          show.cell.prc = TRUE, show.row.prc = FALSE, show.col.prc = FALSE,
#          show.summary = FALSE, show.legend = TRUE, 
#          emph.total = TRUE,
#          digits = 2,
#          encoding = "UTF-8", 
#          file = "illnessxSf.doc")
# # plot illness X Sf
# sjp.xtab(x = df$Sf, grp = df$illness, 
#          title = "身心狀況分類 X 家庭組織型態", 
#          show.total = TRUE, show.n = TRUE, show.prc = TRUE) + theme_sjplot()
# # plot Sex X illness
# sjp.xtab(x = df$illness, grp = df$Sf, 
#          title = "家庭組織型態 X 身心狀況分類", 
#          show.total = TRUE, show.n = TRUE, show.prc = TRUE) + theme_sjplot()
# 
# 
# # Desc Sf -----------------------------------------------------------------