library(MASS)

# fix random seed
set.seed(123)

# data generation setting
n.sim = 100
n.rec = 10000
mu = rep(0,3)
cor.x1.x2 = .4
cor.x1.x3 = .6
cor.x2.x3 = .8

generate <- function(n) {
   # co-variance matrix
   sigma <- matrix(c(1, cor.x1.x2, cor.x1.x3,
                     cor.x1.x2, 1, cor.x2.x3,
                     cor.x1.x3, cor.x2.x3, 1),
                   nrow = 3, ncol = 3)

   # generate data
   data <- mvrnorm(n = n, mu = mu, Sigma=sigma)
   
   ### QUESTION: "Is schreef genoeg?"
   # transform to log-normal variable 
   data[,1] <- exp(data[,1])
   
   return(data)
}

data <- replicate(n.sim, generate(n = n.rec), simplify = F)

# store workspace data
save.image(file = "Workspaces/1. Data Generation.RData")