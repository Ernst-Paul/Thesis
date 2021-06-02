library(mice)
library(tidyr)
library(purrr)
library(broom)

# load previous work session
load("Workspaces/1. Data Generation.RData")
load("Workspaces/2. Simulate MCAR.RData")

# function to evaluate the imputed data
analyis <- function(x) {
   data <- x[[1]] %>% as.data.frame
   imp <- x[[2]]
   mids <- complete(imp, action = "all")[[1]]
   truth <- lm(V3 ~ V1 + V2, data = data)$coefficients[-1]
   
   if (imp[["m"]] == 1) {
      fit.y <- lm(V3 ~ V1 + V2, data = mids)
      fit.y.tidy <- tidy(fit.y, conf.int = T, conf.level = 0.95)
      fit.y.summary <- summary(fit.y)
      
      betas <- fit.y.tidy[,2][-1,] %>% unlist()
      lower <- fit.y.tidy[,6][-1,] %>% unlist()
      upper <- fit.y.tidy[,7][-1,] %>% unlist()
      
      sigma <- fit.y.summary$sigma
      r.squared <- fit.y.summary$r.squared
   } else {
      fit.y <- with(imp, lm(V3 ~ V1 + V2))
      fit.y.pool <- pool(fit.y)
      fit.y.summary <- summary(fit.y.pool, conf.int = TRUE)
      
      betas <- fit.y.summary[-1,2]
      lower <- fit.y.summary[-1,"2.5 %"]
      upper <- fit.y.summary[-1,"97.5 %"]
      coverage <- lower < truth & truth < upper
      
      ### QUESTION: "Is dit de correcte procedure?"
      ### QUESTION: "Kun je ook coverage berekenen van de r.squared & sigma?"
      r.squared <- fit.y.pool$glanced$r.squared %>% mean
      sigma <- fit.y.pool$glanced$sigma %>% mean
   }
   
   coverage <- lower < truth & truth < upper
   
   return(c(betas, coverage, sigma, r.squared))
}

format <- c("beta.x1"=0, "beta.x2"=0, "coverage.beta.x1"=0, "coverage.beta.x2"=0, 
            "sigma"=0, "r.squared"=0)

# apply to every simulation and to every method the statistics function
analysis.1 <- lapply(names, function(name) {
   map(imputed, `[`, c("data", name))} %>% vapply(., analyis, format))

# give list results statistic names
names(analysis.1) <- names

# function to evaluate the 'true' data
analyis.ref <- function(x) {
   data <- x %>% as.data.frame
   
   fit.y <- lm(V3 ~ V1 + V2, data = data)
   fit.y.tidy <- tidy(fit.y, conf.int = T, conf.level = 0.95)
   fit.y.summary <- summary(fit.y)
   
   betas <- fit.y.tidy[,2][-1,] %>% unlist()
   lower <- fit.y.tidy[,6][-1,] %>% unlist()
   upper <- fit.y.tidy[,7][-1,] %>% unlist()
   
   sigma <- fit.y.summary$sigma
   r.squared <- fit.y.summary$r.squared
   
   coverage <- rep(T,2)
   
   return(c(betas, coverage, sigma, r.squared))
}

format <- c("beta.x1"=0, "beta.x2"=0, "coverage.beta.x1"=0, "coverage.beta.x2"=0, 
            "sigma"=0, "r.squared"=0)

# apply function to every simulation
analysis.ref <- imputed %>% map("data") %>% vapply(., analyis.ref, format)