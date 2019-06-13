
# Prep and Options --------------------------------------------------------

# rm
rm(list = ls())
# set working directory
setwd("D:/R_wd/Twin Cities/")
# list of packages
list.packages <- c("tidyverse", "magrittr", "haven", 
                   "gridExtra", "summarytools", "tabulizer", 
                   "labelled", "DescTools", "userfriendlyscience", 
                   "stargazer", "sjPlot")
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
# options for summarytools
st_options(style = "simple",
           round.digits = 4,
           ctable.prop = "c")


# Read the file -----------------------------------------------------------

df <- read_sav("Taipei_all.sav", encoding = "UTF-8")


# Desc --------------------------------------------------------------------

# BMI * sex
d <- df
attr(d$Bd23, "label") <- "性別"
sjt.xtab(d$ovrw, # row
         d$Bd23, # col
         # encoding and digits
         encoding = "UTF-8", digits = 2, 
         # title
         title = "雙城大台北地區 — BMI標準 X 性別", 
         # show
         show.cell.prc = TRUE, show.legend = TRUE, show.exp = FALSE, 
         string.total = "總和", emph.total = TRUE, file = "sex.doc")

# BMI & grade
sjt.xtab(d$ovrw, # row
         d$grade, # col
         # encoding and digits
         encoding = "UTF-8", digits = 2, 
         # title
         title = "雙城大台北地區 — BMI標準 X 年級", 
         # show
         show.cell.prc = TRUE, show.legend = TRUE, show.exp = FALSE, 
         string.total = "總和", emph.total = TRUE, file = "grade.doc")

# BMI & disabilities
attr(d$Ch3, "label") <- "疾病狀況"
names(attr(d$Ch3, "labels")) <- c("沒有", "有", "不知道", "不確定", "拒答")
sjt.xtab(d$ovrw, # row
         d$Ch3, # col
         # encoding and digits
         encoding = "UTF-8", digits = 2, 
         # title
         title = "雙城大台北地區 — BMI標準 X 身心狀況", 
         # show
         show.cell.prc = TRUE, show.legend = TRUE, show.exp = FALSE, 
         string.total = "總和", emph.total = TRUE, file = "disability.doc")

# corr age, sex and BMI
d <- df %>% select(c(realage, Bd23, ovrw))
attr(d$realage, "label") <- "年齡"
attr(d$Bd23, "label") <- "性別"
sjt.corr(d, corr.method = "pearson", encoding = "UTF-8", 
         p.numeric = TRUE, 
         fade.ns = TRUE, file = "corr.doc")
