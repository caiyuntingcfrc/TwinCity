rm(list = ls())
library(ggplot2)
library(tidyverse)
library(haven)
library(summarytools)
library(stargazer)
library(gridExtra)

# set global options of summarytools
st_options()
st_options("style", "grid")
st_options("round.digits", 4)
st_options("freq.report.nas", F)

# load .sav file with haven
twin.first.df <- read_spss("Twin Cities/3.雙城正式_全年級20180704final_han&Lung.sav")
# d <- twin.first.df$Ft9

# Ft9 care_after
df <- twin.first.df %>% filter(Ft9 %in% c(1, 2, 3)) %>% filter(Bd21 %in% c(1, 2)) %>% group_by(Ft9)

# ctable care_after
c <- ctable(df$Bd21, df$Ft9) %>% view()

# working hours
df %>% filter(!is.na(Bd28hr)) %>% filter(!(Bd28hr %in% c(96, 97, 98))) %>% 
        filter(!is.na(Bd33hr)) %>% filter(!(Bd33hr %in% c(96, 97, 98))) %>%
        summarise_at(vars(Bd28hr, Bd33hr), funs(mean)) %>% data.frame() %>% 
        round(digits = 2) %>% 
        stargazer(style = "ajs", type = "html", summary = F, out = "Twin Cities/workinghours.html")

#

# summary with stargazer
stargazer(twin.first.df, type = "html", out = "~/summary.html")

# with summarytools
# summary all
dfSummary(twin.first.df, style = "render", omit.headings = TRUE, 
          bootstrap.css = FALSE) %>% view()
descr(twin.first.df) %>% view()
# cross-table
with(twin.first.df, ctable(Location, Ch2)) %>% view() # location * 總體健康狀況
with(twin.first.df, stby(Ch2, Location, freq)) %>% view()
names(twin.first.df)

#variable view
shanghai2014.labels <- labelled::var_label(shanghai2014.df) #with labelled package
#with lapply
lapply(shanghai2014.df, function(x) attributes(x)$label) %>% unlist() %>% 
    as.data.frame() -> shanghai2014.label
lapply(shanghai2014.df, function(x) attributes(x)$labels) -> shanghai2014.labels

#missing value: birthdate
#vanilla subset
shanghai2014.birthdate.na <- shanghai2014.df[is.na(shanghai2014.df$Birthdate), ]
#with dplyr
shanghai2014.df %>% dplyr::filter(is.na(Birthdate)) -> shanghai2014.birthdate.na

# Better variables view
Varlist <- function(sia) {
    # Init varlist output
    varlist <- data.frame(row.names = names(sia))
    varlist[["comment"]] <- NA
    varlist[["type"]] <- NA
    varlist[["values"]] <- NA
    varlist[["NAs"]] <- NA
    # Fill with meta information
    for (var in names(sia)) {
        if (!is.null(comment(sia[[var]]))) {
            varlist[[var, "comment"]] = comment(sia[[var]])
        }
        varlist[[var, "NAs"]] = sum(is.na(sia[[var]]))
        if (is.factor(sia[[var]])) {
            varlist[[var, "type"]] = "factor"
            varlist[[var, "values"]] = paste(levels(sia[[var]]), collapse=", ")
        } else if (is.character(sia[[var]])) {
            varlist[[var, "type"]] = "character"
        } else if (is.logical(sia[[var]])) {
            varlist[[var, "type"]] = "logical"
            n = sum(!is.na(sia[[var]]))
            if (n > 0) {
                varlist[[var, "values"]] = paste(round(sum(sia[[var]], na.rm=T) / n * 100), "% TRUE", sep="")
            }
        } else if (is.numeric(sia[[var]])) {
            varlist[[var, "type"]] = typeof(sia[[var]])
            n = sum(!is.na(sia[[var]]))
            if (n > 0) {
                varlist[[var, "values"]] = paste(min(sia[[var]], na.rm=T), "...", max(sia[[var]], na.rm=T))
            }
        } else {
            varlist[[var, "type"]] = typeof(sia[[var]])
        }
    }
    View(varlist)
}
Varlist(shanghai2014.df)
