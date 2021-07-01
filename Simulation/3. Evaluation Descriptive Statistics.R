library(mice)
library(tidyr)
library(purrr)

# load previous work sessions
#load("Workspaces/1. Data Generation.RData")
#load("Workspaces/2. Simulate.RData")

# function to calculate the imputed RMSE (credits to Stef v. Buuren)
rmse <- function(data, imp, i) {
   mx <- is.na(mice::complete(imp, 0))[, i]
   mse <- rep(NA, imp$m)
   
   for (k in seq_len(imp$m)) {
      filled <- complete(imp, k)[mx, i]
      true <- data[mx, i]
      mse[k] <- mean((filled - true)^2)
   }
   sqrt(mean(mse))
}

# function to evaluate the mean, variance, SEM coverage, RMSE
statistics.imp <- function(x) {
   data <- x %>% first %>% as.data.frame
   imp <- x %>% last
   mids <- complete(imp, action = "all") %>% do.call("rbind", .)
   com <- combn(3,2)
   
   means <- mids %>% sapply(mean)
   vars <- mids %>% sapply(var)
   truth <- data %>% sapply(mean)
   
   interval <- qt(.975, (n.rec-1)) * (sqrt(vars) / sqrt(n.rec))
   lower <- means - interval
   upper <- means + interval
   coverage <- lower < truth & truth < upper
   aw <- upper - lower
   
   cov <- lapply(seq(mids), function(i) cov(mids[,com[1,i]], mids[,com[2,i]])) %>% unlist
   cor <- lapply(seq(mids), function(i) cor(mids[,com[1,i]], mids[,com[2,i]])) %>% unlist
   
   rmse <- lapply(seq_along(data), function(i) rmse(data, imp, i)) %>% unlist   
   
   return(c(means, vars, coverage, cov, cor, rmse, aw))
}

format <- c("mean.x1"=1, "mean.x2"=1, "mean.y"=1, "var.x1"=1, "var.x2"=1, 
            "var.y"=1, "sem.coverage.x1" = T, "sem.coverage.x2" = T, 
            "sem.coverage.y" = T, "cov.x1.x2"=1, "cov.x1.x3"=1, "cov.x2.x3"=1, 
            "cor.x1.x2"=1, "cor.x1.x3"=1, "cor.x2.x3"=1, "rmse.x1"=1, "rmse.x2"=1, 
            "rmse.y"=1, "aw.x1"=1, "aw.x2"=1, "aw.y"=1)

# apply to every MAR simulation and to every method the statistics function
stat.mar.3 <- lapply(names, function(name) {
   map(imputed.mar.3, `[`, c("data", name))} %>% vapply(., statistics.imp, format))
stat.mar.5 <- lapply(names, function(name) {
   map(imputed.mar.5, `[`, c("data", name))} %>% vapply(., statistics.imp, format))
stat.mar.7 <- lapply(names, function(name) {
   map(imputed.mar.7, `[`, c("data", name))} %>% vapply(., statistics.imp, format))

# give list results statistic names
names(stat.mar.3) <- names
names(stat.mar.5) <- names
names(stat.mar.7) <- names

# function to evaluate the "true" data
statistics.true <- function(x) {
   data <- x %>% as.data.frame
   com <- combn(3,2)
   
   means <- data %>% sapply(mean)
   vars <- data %>% sapply(var)
   coverage <- rep(T,3)
   cov <- lapply(seq(data), function(i) cov(data[,com[1,i]], data[,com[2,i]])) %>% unlist
   cor <- lapply(seq(data), function(i) cor(data[,com[1,i]], data[,com[2,i]])) %>% unlist
   rmse <- rep(0,3)
   
   return(c(means, vars, coverage, cov, cor, rmse))
}

format <- c("mean.x1"=1, "mean.x2"=1, "mean.y"=1, "var.x1"=1, "var.x2"=1, "var.y"=1, 
            "sem.coverage.x1" = T, "sem.coverage.x2" = T, "sem.coverage.y" = T, 
            "cov.x1.x2"=1, "cov.x1.x3"=1, "cov.x2.x3"=1, "cor.x1.x2"=1, 
            "cor.x1.x3"=1, "cor.x2.x3"=1, "rmse.x1"=1, "rmse.x2"=1, "rmse.y"=1)

# apply function to every simulation
stat.true <- vapply(data, statistics.true, format) %>% 
   as.data.frame