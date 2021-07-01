library(mice)
library(tidyr)
library(purrr)
library(broom)

# load previous work session
#load("Workspaces/1. Data Generation.RData")
#load("Workspaces/2. Simulate.RData")

# function to evaluate the imputed data
analyis.imp <- function(x) {
   data <- x %>% first %>% as.data.frame
   imp <- x %>% last
   mids <- complete(imp, action = "all")[[1]]
   truth <- lm(V3 ~ V1 + V2, data = data)$coefficients[-1]
   
   if (imp[["m"]] == 1) {
      fit.y <- lm(V3 ~ V1 + V2, data = mids)
      fit.y.tidy <- tidy(fit.y, conf.int = T, conf.level = 0.95)
      fit.y.summary <- summary(fit.y)
      
      betas <- fit.y.tidy[-1,"estimate"] %>% unlist
      lower <- fit.y.tidy[-1,"conf.low"] %>% unlist
      upper <- fit.y.tidy[-1,"conf.high"] %>% unlist
      
      sigma <- fit.y.summary$sigma %>% .^2
      r.squared <- fit.y.summary$r.squared
   } else {
      fit.y <- with(imp, lm(V3 ~ V1 + V2))
      fit.y.pool <- pool(fit.y)
      fit.y.summary <- summary(fit.y.pool, conf.int = TRUE)
      
      betas <- fit.y.summary[-1,"estimate"]
      lower <- fit.y.summary[-1,"2.5 %"]
      upper <- fit.y.summary[-1,"97.5 %"]
      
      sigma <- fit.y.pool$glanced$sigma %>% mean %>% .^2
      r.squared <- pool.r.squared(fit.y)[1]
   }
   
   coverage <- lower < truth & truth < upper
   aw <- upper - lower
   
   return(c(betas, coverage, sigma, r.squared, aw))
}

format <- c("beta.x1"=0, "beta.x2"=0, "coverage.beta.x1"=0, "coverage.beta.x2"=0, 
            "sigma"=0, "r.squared"=0, "aw.x1"=0, "aw.x2"=0)

# apply to every simulation and to every method the statistics function
analysis.mar.3 <- lapply(names, function(name) {
   map(imputed.mar.3, `[`, c("data", name))} %>% vapply(., analyis.imp, format))
analysis.mar.5 <- lapply(names, function(name) {
   map(imputed.mar.5, `[`, c("data", name))} %>% vapply(., analyis.imp, format))
analysis.mar.7 <- lapply(names, function(name) {
   map(imputed.mar.7, `[`, c("data", name))} %>% vapply(., analyis.imp, format))

# give list results statistic names
names(analysis.mar.3) <- names
names(analysis.mar.5) <- names
names(analysis.mar.7) <- names

# function to evaluate the 'true' data
analyis.true <- function(x) {
   data <- x %>% as.data.frame
   fit.y <- lm(V3 ~ V1 + V2, data = data)
   fit.y.tidy <- tidy(fit.y, conf.int = T, conf.level = 0.95)
   fit.y.summary <- summary(fit.y)
   
   betas <- fit.y.tidy[-1,"estimate"] %>% unlist
   lower <- fit.y.tidy[-1,"conf.low"] %>% unlist
   upper <- fit.y.tidy[-1,"conf.high"] %>% unlist
   
   coverage <- rep(T,2)
   sigma <- fit.y.summary$sigma %>% .^2
   r.squared <- fit.y.summary$r.squared
   aw <- upper - lower
   
   return(c(betas, coverage, sigma, r.squared, aw))
}

format <- c("beta.x1"=0, "beta.x2"=0, "coverage.beta.x1"=0, "coverage.beta.x2"=0, 
            "sigma"=0, "r.squared"=0, "aw.x1"=0, "aw.x2"=0)

# apply function to every simulation
analysis.true <- vapply(data, analyis.true, format) %>% as.data.frame