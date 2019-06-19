
# Prep and Options --------------------------------------------------------

# rm
rm(list = ls())
# set working directory
setwd("D:/R_wd/Twin Cities/")

# list of packages
list.packages <- c("tidyverse", "magrittr", "haven", 
                   "gridExtra", "summarytools", "tabulizer", 
                   "labelled", "DescTools", "userfriendlyscience", "stargazer", 
                   "sjlabelled", "sjmisc", "sjstats", "ggeffects", "sjPlot")
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


# bmi z-score -------------------------------------------------------------




# Desc (BMI) --------------------------------------------------------------

# bmicat

plot_frq(df$bmicat, 
         title = "BMI分類") + theme_sjplot()

# ctable BMI x grade
sjt.xtab(var.row = df$bmicat, var.col = df$grade, 
         var.labels = c("BMI分類", "年級"), 
         title = "BMI分類 X 年級", 
         string.total = "總和", 
         show.cell.prc = TRUE, show.row.prc = FALSE, show.col.prc = FALSE,
         show.summary = FALSE, show.legend = TRUE, 
         emph.total = TRUE,
         digits = 2,
         encoding = "UTF-8", 
         file= "BMIxGrade.doc")
# plot BMI x grade
sjp.xtab(x = df$grade, grp = df$bmicat, 
         title = "BMI分類 X 年級", 
         show.total = TRUE, show.n = TRUE, show.prc = TRUE) + theme_sjplot()
# plot grade x BMI
sjp.xtab(x = df$bmicat, grp = df$grade, 
         title = "年級 X BMI分類", 
         show.total = TRUE, show.n = TRUE, show.prc = TRUE) + theme_sjplot()

# ctable BMI X Sex
sjt.xtab(var.row = df$bmicat, var.col = df$Bd23, 
         var.labels = c("BMI分類", "性別"), 
         title = "BMI分類 X 性別", 
         string.total = "總和", 
         show.cell.prc = TRUE, show.row.prc = FALSE, show.col.prc = FALSE,
         show.summary = FALSE, show.legend = TRUE, 
         emph.total = TRUE,
         digits = 2,
         encoding = "UTF-8",
         file = "BMIxSex.doc")
# plot BMI X Sex
sjp.xtab(x = df$Bd23, grp = df$bmicat, 
         title = "BMI分類 X 性別", 
         show.total = TRUE, show.n = TRUE, show.prc = TRUE) + theme_sjplot()
# plot Sex X BMI
sjp.xtab(x = df$bmicat, grp = df$Bd23, 
         title = "性別 X BMI分類", 
         show.total = TRUE, show.n = TRUE, show.prc = TRUE) + theme_sjplot()

# ctable BMI X SF
sjt.xtab(var.row = df$bmicat, var.col = df$Sf, 
         var.labels = c("BMI分類", "家庭組織型態"), 
         title = "BMI分類 X 家庭組織型態", 
         string.total = "總和", 
         show.cell.prc = TRUE, show.row.prc = FALSE, show.col.prc = FALSE,
         show.summary = FALSE, show.legend = TRUE, 
         emph.total = TRUE,
         digits = 2,
         encoding = "UTF-8", 
         file = "BMIxSf.doc")
# plot BMI X Sf
sjp.xtab(x = df$Sf, grp = df$bmicat, 
         title = "BMI分類 X 家庭組織型態", 
         show.total = TRUE, show.n = TRUE, show.prc = TRUE) + theme_sjplot()
# plot Sf X BMI
sjp.xtab(x = df$bmicat, grp = df$Sf, 
         title = "家庭組織型態 X BMI分類", 
         show.total = TRUE, show.n = TRUE, show.prc = TRUE) + theme_sjplot()


# Desc (illness) ----------------------------------------------------------

# illness
plot_frq(df$illness, 
         title = "身心狀況分類") + theme_sjplot()

# ctable illness X grade
sjt.xtab(var.row = df$illness, var.col = df$grade, 
         var.labels = c("身心狀況分類", "年級"), 
         title = "身心狀況分類 X 年級", 
         string.total = "總和", 
         show.cell.prc = TRUE, show.row.prc = FALSE, show.col.prc = FALSE,
         show.summary = FALSE, show.legend = TRUE, 
         emph.total = TRUE,
         digits = 2,
         encoding = "UTF-8", 
         file = "illnessxGrade.doc")
# plot illness X grade
sjp.xtab(x = df$grade, grp = df$illness, 
         title = "身心狀況分類 X 年級", 
         show.total = TRUE, show.n = TRUE, show.prc = TRUE) + theme_sjplot()
# plot grade X illness
sjp.xtab(x = df$illness, grp = df$grade, 
         title = "年級 X 身心狀況分類", 
         show.total = TRUE, show.n = TRUE, show.prc = TRUE) + theme_sjplot()

# ctable illness X sex
sjt.xtab(var.row = df$illness, var.col = df$Bd23, 
         var.labels = c("身心狀況分類", "性別"), 
         title = "身心狀況分類 X 性別", 
         string.total = "總和", 
         show.cell.prc = TRUE, show.row.prc = FALSE, show.col.prc = FALSE,
         show.summary = FALSE, show.legend = TRUE, 
         emph.total = TRUE,
         digits = 2,
         encoding = "UTF-8", 
         file = "illnessxSex.doc")
# plot illness X Sex
sjp.xtab(x = df$Bd23, grp = df$illness, 
         title = "身心狀況分類 X 性別", 
         show.total = TRUE, show.n = TRUE, show.prc = TRUE) + theme_sjplot()
# plot Sex X illness
sjp.xtab(x = df$illness, grp = df$Bd23, 
         title = "性別 X 身心狀況分類", 
         show.total = TRUE, show.n = TRUE, show.prc = TRUE) + theme_sjplot()

# ctable illness X Sf
sjt.xtab(var.row = df$illness, var.col = df$Sf, 
         var.labels = c("身心狀況分類", "家庭組織型態"), 
         title = "身心狀況分類 X 家庭組織型態", 
         string.total = "總和", 
         show.cell.prc = TRUE, show.row.prc = FALSE, show.col.prc = FALSE,
         show.summary = FALSE, show.legend = TRUE, 
         emph.total = TRUE,
         digits = 2,
         encoding = "UTF-8", 
         file = "illnessxSf.doc")
# plot illness X Sf
sjp.xtab(x = df$Sf, grp = df$illness, 
         title = "身心狀況分類 X 家庭組織型態", 
         show.total = TRUE, show.n = TRUE, show.prc = TRUE) + theme_sjplot()
# plot Sex X illness
sjp.xtab(x = df$illness, grp = df$Sf, 
         title = "家庭組織型態 X 身心狀況分類", 
         show.total = TRUE, show.n = TRUE, show.prc = TRUE) + theme_sjplot()


# Desc Sf -----------------------------------------------------------------

# Sf
plot_frq(df$Sf, 
         title = "家庭組織型態") + theme_sjplot()
