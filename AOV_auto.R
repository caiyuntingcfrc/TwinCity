devtools::source_url("https://raw.githubusercontent.com/caiyuntingcfrc/misc/function_poverty/func_ins.pack.R")
ins.pack("tidyverse", "DescTools", "userfriendlyscience")
# function ----------------------------------------------------------------

aov_auto <- function(data, var1, group, sig = 0.05) {
        # Test of Homogeneity of Variance
        var <- DescTools::LeveneTest(data[[var1]], 
                                     group = as.factor(data[[group]]))
        print(var)
        
        if(var[ , 3][1] < sig) {
                # Homogeneity of Variance = FALSE
                out_aov <- stats::oneway.test(data[[var1]] ~ data[[group]])
                print(out_aov)
                # if sig. then post-hoc (Games-Howell)
                if(out_aov[3] < sig) {
                        data <- data %>% 
                                filter(!is.na(data[[var1]]) & !is.na(data[[group]]))
                        userfriendlyscience::posthocTGH(y = data[[var1]], 
                                                        x = as.factor(data[[group]]),
                                                        digits = 3,
                                                        method = "games-howell")
                        }
                # Homogeneity of Variance = TRUE
                } else { 
                        out <- aov(data[[var1]] ~ as.factor(data[[group]]))
                        out_aov <- summary(out)
                        print(out_aov)
                        # if sig. then post-hoc
                        if(out_aov[[1]]$`Pr(>F)`[1] < sig) {
                                DescTools::PostHocTest(out, method = "scheffe") 
                                }
                        }
}