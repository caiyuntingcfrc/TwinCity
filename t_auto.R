
# prep --------------------------------------------------------------------

# list of packages
list.packages <- c("tidyverse", "userfriendlyscience")
# check if the packages are installed
new.packages <- list.packages[!(list.packages %in% installed.packages()[ , "Package"])]
# install new packages
if(length(new.packages)) install.packages(new.packages)
# remove lists
rm(list.packages, new.packages)
# options
options(scipen = 999)

# function ----------------------------------------------------------------

t_auto <- function(data, var, group, 
                   paired = FALSE, 
                   alternative = "two.sided", 
                   sig = .05)  {
         # group
        g0 <- data[[group]] %>% 
                as.factor() %>% 
                levels() %>% .[1] %>% 
                as.integer()
        g1 <- data[[group]] %>% 
                as.factor() %>% 
                levels() %>% .[2] %>% 
                as.integer()
        # filter v0 and v1
        v0 <- data %>% filter(.[[group]] == g0) %>% .[[var]]
        v1 <- data %>% filter(.[[group]] == g1) %>% .[[var]]
        
        # Test of Homogeneity of Variance
        var <- DescTools::LeveneTest(y = data[[var]], 
                                     group = as.factor(data[[group]]))
        print(var)
        cat("\n")
        print(paste("Levels of", group, ":", g0, "and", g1))
        if(var[ , 3][1] < sig) {
                t.test(x = v0, 
                       y = v1, 
                       alternative = alternative,
                       paired = paired,
                       var.equal = FALSE)} else {t.test(x = v0, 
                               y = v1,
                               alternative = alternative,
                               paired = paired, 
                               var.equal = TRUE)
                        }
        }