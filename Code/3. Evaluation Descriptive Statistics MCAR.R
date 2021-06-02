library(mice)
library(tidyr)
library(purrr)

# load previous work sessions
load("Workspaces/1. Data Generation.RData")
load("Workspaces/2. Simulate MCAR.RData")

# function to evaluate the mean, variance, SEM coverage, RMSE
statistics.1 <- function(x) {
   data <- x[[1]] %>% as.data.frame
   imp <- x[[2]]
   mids <- complete(imp, action = "all")[[1]]
   
   means <- mids %>% sapply(mean)
   vars <- mids %>% sapply(var)
   
   truth <- data %>% sapply(mean)
   interval <- qt(.975, (n.rec-1)) * (vars / sqrt(n.rec))
   lower <- means - interval
   upper <- means + interval
   coverage <- lower < truth & truth < upper
   
   rmse <- sqrt((data - mids)^2) %>% sapply(mean)
   
   return(c(means, vars, coverage, rmse))
}

format <- c("mean.x1"=1, "mean.x2"=1, "mean.y"=1, "var.x1"=1, "var.x2"=1, 
            "var.y"=1, "sem.coverage.x1" = T, "sem.coverage.x2" = T, 
            "sem.coverage.y" = T, "rmse.x1"=1, "rmse.x2"=1, "rmse.y"=1)

# apply to every simulation and to every method the statistics function
statistics.1 <- lapply(names, function(name) {
   map(imputed, `[`, c("data", name))} %>% vapply(., statistics.1, format))

# give list results statistic names
names(statistics.1) <- names

# function to evaluate the covariance, correlation, and corr coverage
statistics.2 <- function(x) {
   data <- x[[1]] %>% as.data.frame
   imp <- x[[2]]
   mids <- complete(imp, action = "all")[[1]]
   com <- combn(3,2)
   
   ### QUESTION: "Is er een mooiere procedure?"
   cov <- lapply(seq(mids), function(i) cov(mids[,com[1,i]], mids[,com[2,i]])) %>% unlist
   cor <- lapply(seq(mids), function(i) cor(mids[,com[1,i]], mids[,com[2,i]])) %>% unlist
   truth <- lapply(seq(data), function(i) cor(data[,com[1,i]], data[,com[2,i]])) %>% unlist
   interval <- lapply(seq(mids), function(i) cor.test(mids[,com[1,i]], mids[,com[2,i]]) %>%
                         .$conf.int) 
   lower <- interval %>% lapply(`[[`, 1) %>% unlist
   upper <- interval %>% lapply(`[[`, 2) %>% unlist
   coverage <- lower < truth & truth < upper
   
   return(c(cov, cor, coverage))
}

format <- c("cov.x1.x2"=1, "cov.x1.x3"=1, "cov.x2.x3"=1, "cor.x1.x2"=1, "cor.x1.x3"=1, 
            "cor.x2.x3"=1, "cor.coverage.x1.x2"=T, "cor.coverage.x1.x3"=T, "cor.coverage.x2.x3"=T)

# apply to every simulation and to every method the statistics function
statistics.2 <- lapply(names, function(name) {
   map(imputed, `[`, c("data", name))} %>% vapply(., statistics.2, format))

# give list results statistic names
names(statistics.2) <- names

# function to evaluate the "true" data
statistics.ref <- function(x) {
   data <- x %>% as.data.frame
   com <- combn(3,2)
   
   means <- data %>% sapply(mean)
   vars <- data %>% sapply(var)
   coverage <- rep(T,3)
   rmse <- rep(0,3)
   cov <- lapply(seq(data), function(i) cov(data[,com[1,i]], data[,com[2,i]])) %>% unlist
   cor <- lapply(seq(data), function(i) cor(data[,com[1,i]], data[,com[2,i]])) %>% unlist

   return(c(means, vars, coverage, rmse, cov, cor, coverage))
}

format <- c("mean.x1"=1, "mean.x2"=1, "mean.y"=1, "var.x1"=1, "var.x2"=1, "var.y"=1, 
            "sem.coverage.x1" = T, "sem.coverage.x2" = T, "sem.coverage.y" = T, 
            "rmse.x1"=1, "rmse.x2"=1, "rmse.y"=1, "cov.x1.x2"=1, "cov.x1.x3"=1,
            "cov.x2.x3"=1, "cor.x1.x2"=1, "cor.x1.x3"=1, "cor.x2.x3"=1, 
            "cor.coverage.x1.x2"=T, "cor.coverage.x1.x3"=T, "cor.coverage.x2.x3"=T)

# apply function to every simulation
statistics.ref <- imputed %>% map("data") %>% vapply(.,statistics.ref, format)