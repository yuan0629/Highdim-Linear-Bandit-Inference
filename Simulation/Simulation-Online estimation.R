library(ggplot2)
library(DebiasInfer)
library(glmnet)
library(truncnorm)
#library(hdi)

options(warn=-1)
setwd("")

set.seed(629122)

### three scenarios to produce Figure 1 Left, Center and Right ###

# parameters (scenario 1)
Time <- 300 # time horizon T
d <- 600  # dimension of covariate X_t
sigma1 <- sigma0 <- 0.1 # noise level
s0 <- 3 # true sparsity
s <- 10 # sparsity used in hard-thresholding
beta1 <- matrix(c(runif(s0, 0.5, 1), rep(0, d-s0)), ncol = 1) # true \beta_1
beta0 <- matrix(c(runif(s0, 0.5, 1), rep(0, d-s0)), ncol = 1) # true \beta_0
Iteration <- 100

# parameters (scenario 2)
# Time <- 600 # time horizon T
# d <- 1000  # dimension of covariate X_t
# sigma1 <- sigma0 <- 0.1 # noise level
# s0 <- 8 # true sparsity
# s <- 15 # sparsity used in hard-thresholding
# beta1 <- matrix(c(runif(s0, 0.5, 1), rep(0, d-s0)), ncol = 1) # true \beta_1
# beta0 <- matrix(c(runif(s0, 0.5, 1), rep(0, d-s0)), ncol = 1) # true \beta_0
# Iteration <- 100

# parameters (scenario 3)
# Time <- 1000 # time horizon T
# d <- 2000  # dimension of covariate X_t
# sigma1 <- sigma0 <- 0.1 # noise level
# s0 <- 10 # true sparsity
# s <- 20 # sparsity used in hard-thresholding
# beta1 <- matrix(c(runif(s0, 0.5, 1), rep(0, d-s0)), ncol = 1) # true \beta_1
# beta0 <- matrix(c(runif(s0, 0.5, 1), rep(0, d-s0)), ncol = 1) # true \beta_0
# Iteration <- 100


### Setting 1: IPW estimator (\gamma=1/3 and 1/2)
gamma <- 1/3 
c2 <- 4 # define the exploration probability \varepsilon_t=c_2t^{-gamma}; change between "gamma <- 1/3" and "gamma <- 1/2" to 
             #modify \gamma to produce the results in Figure 1
eta <- 0.05  # stepsize
  


# IPW estimator
result1 <- matrix(0, nrow = Iteration*Time, ncol = 3) # store the online estimation error
result1[,3] <- rep(c(1:Time), Iteration)


for (iter in 1:Iteration) {
  
  print(iter)
  set.seed(iter)
  
  #initialization
  beta1hat <- beta0hat <- matrix(0, ncol = 1, nrow = d)
  Sigma1 <- Sigma0 <- matrix(0, ncol = d, nrow = d)
  g0 <- g1 <- matrix(0, ncol = 1, nrow = d)
  
  
  for (t in 1:Time) {
    
    # observe context X_t
    Xt <- rtruncnorm(d, a=-1, b=1)
    
    
    # decision (line 4-5 of Algorithm 1)
    epsilon <- min(c2*t^(-gamma), 0.5)
    pit <- (1-epsilon/2) * (t(beta1hat) %*% Xt > t(beta0hat) %*% Xt)[[1]] + epsilon/2 * (t(beta1hat) %*% Xt <= t(beta0hat) %*% Xt)[[1]]
    ac <- sample(c(0,1), prob = c(1-pit, pit), 1) 
    
    if (ac == 0) {
      y <- t(beta0) %*% Xt + rnorm(1, 0, sigma0)
    }else {
      y <- t(beta1) %*% Xt + rnorm(1, 0, sigma1)
    }
    
    
    # estimation
    
    Sigma0 <- (t-1)/t*Sigma0 + (ac==0) * 1/t * 1/(1-pit) * Xt %*% t(Xt)  # (line 10 of Algorithm 1)
    Sigma1 <- (t-1)/t*Sigma1 + (ac==1) * 1/t * 1/pit * Xt %*% t(Xt)
    g0 <- (t-1)/t*g0 + (ac==0) * 1/t * 1/(1-pit) * Xt %*% y # (line 11)
    g1 <- (t-1)/t*g1 + (ac==1) * 1/t * 1/pit * Xt %*% y 
    
    
    # update beta0 (line 12)
    g <- 2 * Sigma0 %*% beta0hat - 2 * g0
    beta0hat <- beta0hat - eta * g
    beta0hat[-order(abs(beta0hat), decreasing = TRUE)[1:s]] <- 0
    
    
    # update beta1
    g <- 2 * Sigma1 %*% beta1hat - 2 * g1
    beta1hat <- beta1hat - eta * g
    beta1hat[-order(abs(beta1hat), decreasing = TRUE)[1:s]] <- 0
    
    # store the online l2 estimation error
    result1[(iter-1)*Time+t, 1] <- sum((beta1hat-beta1)^2)
    result1[(iter-1)*Time+t, 2] <- sum((beta0hat-beta0)^2)
    
  }
  
  
}



# save result
save(result1, file = "scenario1-sgd-result1.RData")
# save(result2, file = "scenario2-sgd-result2.RData")





### Setting 2: Exploration-free algorithm (\varepsilon_t=0)

eta <- 0.12 # stepsize

result3 <- matrix(0, nrow = Iteration*Time, ncol = 3) # store the online estimation error
result3[,3] <- rep(c(1:Time), Iteration)


#initialization
for (iter in 1:Iteration) {

  print(iter)
  set.seed(iter)
  
  # initialization
  beta1hat <- beta0hat <- matrix(0, ncol = 1, nrow = d)
  sigma1hat <- sigma0hat <- 0
  Sigma1 <- Sigma0 <- matrix(0, ncol = d, nrow = d)
  g0 <- g1 <- matrix(0, ncol = 1, nrow = d)
  
  
  for (t in 1:Time) {
    
    # observe context X_t
    Xt <- rtruncnorm(d, a=-1, b=1)
    
    # decision 
    ac <- (t(beta1hat) %*% Xt > t(beta0hat) %*% Xt)[[1]]
    
    
    if (ac == 0) {
      y <- t(beta0) %*% Xt + rnorm(1, 0, sigma0)
    }else {
      y <- t(beta1) %*% Xt + rnorm(1, 0, sigma1)
    }
    
    
    
    # estimation
    
    Sigma0 <- (t-1)/t*Sigma0 + (ac==0) * 1/t * Xt %*% t(Xt) # (line 10 of Algorithm 1)
    Sigma1 <- (t-1)/t*Sigma1 + (ac==1) * 1/t * Xt %*% t(Xt)
    g0 <- (t-1)/t*g0 + (ac==0) * 1/t *  Xt %*% y # (line 11)
    g1 <- (t-1)/t*g1 + (ac==1) * 1/t * Xt %*% y
    
    # update beta0 (line 12)
    g <- 2 * Sigma0 %*% beta0hat - 2 * g0
    beta0hat <- beta0hat - eta * g
    beta0hat[-order(abs(beta0hat), decreasing = TRUE)[1:s]] <- 0
    
    # update beta1
    g <- 2 * Sigma1 %*% beta1hat - 2 * g1
    beta1hat <- beta1hat - eta * g
    beta1hat[-order(abs(beta1hat), decreasing = TRUE)[1:s]] <- 0
    
    result3[(iter-1)*Time+t, 1] <- sum((beta1hat-beta1)^2)
    result3[(iter-1)*Time+t, 2] <- sum((beta0hat-beta0)^2)
    
  }
  

  
}


# save result
save(result3, file = "scenario3-sgd-result3.RData")







