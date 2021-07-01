library(MASS)
setwd("~/Desktop/Thesis/ErnstPaul")

# fix random seed
set.seed(123)

# data generation setting
n.sim = 1000
n.rec = 1000
mu = c(70, 5, 35)
cov.x1.x1 = 155
cov.x2.x2 = 0.3
cov.x3.x3 = 50
cov.x1.x2 = 1
cov.x1.x3 = 25
cov.x2.x3 = 1.5

generate <- function(n) {
   # co-variance matrix
   sigma <- matrix(c(cov.x1.x1, cov.x1.x2, cov.x1.x3,
                     cov.x1.x2, cov.x2.x2, cov.x2.x3,
                     cov.x1.x3, cov.x2.x3, cov.x3.x3),
                   nrow = 3, ncol = 3)
   
   # generate data
   data <- mvrnorm(n = n, mu = mu, Sigma=sigma)
   
   # transform Insulin to log-normal variable 
   data[,2] <- exp(data[,2])
   
   return(data)
}

data <- replicate(n.sim, generate(n = n.rec), simplify = F)

# store workspace data
save.image(file = "Workspaces/1. Data Generation.RData")