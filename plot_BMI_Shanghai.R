
# Prep and Options --------------------------------------------------------

# rm
rm(list = ls())
# set working directory
setwd("D:/R_wd/Twin Cities/data")
# list of packages
list.packages <- c("tidyverse", "magrittr", "haven", 
                   "gridExtra", "summarytools", "tabulizer", 
                   "labelled", "DescTools", "userfriendlyscience", 
                   "stargazer", "sjPlot", "arsenal")
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


# import data -------------------------------------------------------------

# 2014
filepath_2014 <- "12. Shanghai_2014.sav"
d_2014 <- read_sav(filepath_2014, encoding = "UTF-8")

# 2016
filepath_2016 <- "13. Shanghai_2016.sav"
d_2016 <- read_sav(filepath_2016, encoding = "UTF-8")


# tables ------------------------------------------------------------------

# 2014
d_2014$Gender <- labelled(d_2014$Gender, 
                          labels = c("女性" = 0, 
                                     "男性" = 1), 
                          label = "性別")
sjt.xtab(var.row = d_2014$bmicat_tw, 
         var.col = d_2014$Gender,
         # encoding and digits
         encoding = "UTF-8", digits = 2, 
         # title
         title = "2014上海 — BMI標準 X 性別", 
         # show percentage
         show.cell.prc = FALSE, show.row.prc = TRUE, show.col.prc = TRUE,
         # show 
         show.legend = TRUE, show.exp = FALSE, 
         string.total = "總和", emph.total = TRUE)

t <- freq(factor(d_2014$bmicat_tw), digits = 3, round = 2)
a <- t[[4]]

# 2016
d_2016$Gender <- labelled(d_2016$Gender, 
                          labels = c("女性" = 0, 
                                     "男性" = 1), 
                          label = "性別")
sjt.xtab(var.row = d_2016$bmicat_tw, 
         var.col = d_2016$Gender,
         # encoding and digits
         encoding = "UTF-8", digits = 2, 
         # title
         title = "2016上海 — BMI標準 X 性別", 
         # show percentage
         show.cell.prc = FALSE, show.row.prc = TRUE, show.col.prc = TRUE,
         # show 
         show.legend = TRUE, show.exp = FALSE, 
         string.total = "總和", emph.total = TRUE)

# plots -------------------------------------------------------------------

d_2014 <- d_2014[!is.na(d_2014$bmicat_tw), ]
d_2014$bmicat_tw <- as.factor(d_2014$bmicat_tw)
levels(d_2014$bmicat_tw) <- c("過輕", "正常範圍", "過重", "肥胖")
ggplot(d_2014, aes(bmicat_tw, y = (..count..) / sum(..count..), fill = bmicat_tw)) + 
        # bar
        geom_bar(stat = "count") + 
        # text prc
        geom_text(aes(label = paste(round((..count..) / sum(..count..) * 100, 2), " %", sep = "" ),
                      y = (..count..) / sum(..count..)), stat= "count", vjust = -2) +
        # text count
        geom_text(aes(label = paste("n = ", ..count.., sep = ""),
                      y = (..count..) / sum(..count..)), stat= "count", vjust = -.5) +
        # labels
        labs(x = "BMI分類", y = "百分比", title = "2014上海 BMI分類\n") +
        # adjust scales
        scale_y_continuous(labels = scales::percent, expand = c(0, 0), limits = c(0, .8)) +
        # legend
        scale_fill_brewer(name = NULL, palette = "Paired", labels = levels(d_2014$bmicat_tw)) +
        # theme
        theme_sjplot()
