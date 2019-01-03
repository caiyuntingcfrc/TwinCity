rm(list = ls())
library(ggplot2)
library(dplyr)
library(summarytools)
library(stargazer)
library(Hmisc)


#with foreign
shanghai2014.df <- foreign::read.spss("C:/Users/user/Desktop/1.上海_2014r2.sav")

#with haven
shanghai2014.df <- haven::read_spss("c:/Users/user/Desktop/1.上海_2014r2.sav")

#summary with stargazer
df <- attitude
stargazer(df, type = "html", out = "~/summary.html")

#with summarytools
p <- dfSummary(df, style = "html", omit.headings = TRUE)
view(p)

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
