library(dplyr)
library(mice)

# fix random seed
set.seed(123)

# load previous work session
load("Workspaces/1. Data Generation.RData")

# amputation settings
p = 0.9
patterns <- expand.grid(replicate(3, 0:1, simplify = F)) %>% .[2:7,]

# imputation settings
methods = c("mean", "norm.predict", "norm.nob", "norm.nob", "norm", "norm", "pmm", "pmm")
names = c("mean", "reg", "stoch.m1", "stoch.m5", "bayes.m1", "bayes.m5", "pmm.m1", "pmm.m5")
maxit = c(1, 1, 7, 7, 7, 7, 7, 7)
m = c(1, 1, 1, 5, 1, 5, 1, 5)

simulate <- function(data) {
   # sample missing data patterns
   pattern <- patterns[sample(6, 3, replace = F),]
   
   # create missing data
   missing <- ampute(data, prop = p, patterns = pattern, mech = "MCAR")
   missing <- replicate(length(methods), missing$amp, simplify = F)
   
   # impute with given methods
   result <- mapply(mice, missing, m = m, method = methods, maxit = maxit, 
                    printFlag = F, SIMPLIFY = F)
   
   # give list imputation method names
   names(result) <- names
   
   return(c(result, data = list(data)))
}

# apply function to every simulation
imputed <- data %>% lapply(., simulate)

# store workspace data
rm(patterns)
save.image(file = "Workspaces/2. Simulate MCAR.RData")