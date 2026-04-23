library(ggplot2)
library(DebiasInfer)
library(glmnet)
library(truncnorm)
#library(hdi)

options(warn=-1)
setwd("")

set.seed(629122)

### three scenarios to produce Figure 3 Left, Center and Right ###

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




### optimal policy value inference

eta <- 0.05 # stepsize

result3 <- matrix(0, nrow = Iteration*Time, ncol = 4) # store point and interval estimators

for (iter in 1:Iteration) {
  
  print(iter)
  
  #initialization
  beta1hat <- beta0hat <- matrix(0, ncol = 1, nrow = d)
  ipw1sum1 <- ipw1sum2 <- ipw0sum1 <- ipw0sum2 <- 0
  sigma1hat <- sigma0hat <- 0
  Sigma1 <- Sigma0 <- matrix(0, ncol = d, nrow = d)
  g0 <- g1 <- matrix(0, ncol = 1, nrow = d)
  GT1 <- GT0 <- WT1 <- WT2 <- Vhat <- Vreal <- Vhatt <- Vrealt <- 0
  S1 <- S0 <- rep(0, cut)
  
  set.seed(iter)
  for (t in 1:Time) {
    
    # observe context X_t
    Xt <- matrix(rnorm(d), ncol = 1)
    Xt[(abs(Xt)>1)] <- sign(Xt[(abs(Xt)>1)])
    
    # decision
    ac <- (t(beta1hat) %*% Xt > t(beta0hat) %*% Xt)[[1]]
    acreal <- (t(beta1) %*% Xt > t(beta0) %*% Xt)[[1]]
    
    
    if (ac == 0) {
      y <- t(beta0) %*% Xt + rnorm(1, 0, sigma0)
    }else {
      y <- t(beta1) %*% Xt + rnorm(1, 0, sigma1)
    }
    
    # oracle optimal value: reward for true optimal arm
    if (acreal == 0) {
      yreal <- t(beta0) %*% Xt 
    }else {
      yreal <- t(beta1) %*% Xt 
    }
    
    
    # part of variance estimator \hat{S}_V^2
    if (ac == 1) { 
      GT1 <- GT1 + 1
      WT1 <- WT1 + (t(beta1hat) %*% Xt)^2
      WT2 <- WT2 + t(beta1hat) %*% Xt
    } else {
      GT0 <- GT0 + 1
      WT1 <- WT1 + (t(beta0hat) %*% Xt)^2
      WT2 <- WT2 + t(beta0hat) %*% Xt
    }
    
    
    # noise level estimator
    sigma1hat <- sigma1hat + (ac == 1)  * (y - t(Xt) %*% beta1hat)^2
    sigma0hat <- sigma0hat + (ac == 0)  * (y - t(Xt) %*% beta0hat)^2
    
    # CI and point estimator 
    Vhat <- Vhat + y # optimal policy value estiamtor \hat{V}_T 
    Vhatt <- Vhat / t 
    Vreal <- Vreal + yreal # oracle optimal value
    Vrealt <- Vreal / t
    SV <- (sigma1hat + sigma0hat + WT1) / t - (WT2 /t)^2 # variance estimator \hat{S}_V^2
    result3[((iter-1)*Time + t),1] <- Vhatt - qnorm(0.975) * sqrt(SV/t)
    result3[((iter-1)*Time + t),2] <- Vhatt 
    result3[((iter-1)*Time + t),3] <- Vhatt + qnorm(0.975) * sqrt(SV/t)
    result3[((iter-1)*Time + t),4] <- Vrealt 
    
    # online estimation
    
    Sigma0 <- (t-1)/t*Sigma0 + (ac==0) * 1/t * Xt %*% t(Xt)
    Sigma1 <- (t-1)/t*Sigma1 + (ac==1) * 1/t * Xt %*% t(Xt)
    g0 <- (t-1)/t*g0 + (ac==0) * 1/t *  Xt %*% y 
    g1 <- (t-1)/t*g1 + (ac==1) * 1/t * Xt %*% y
    
    # update beta0
    g <- 2 * Sigma0 %*% beta0hat - 2 * g0
    beta0hat <- beta0hat - eta * g
    beta0hat[-order(abs(beta0hat), decreasing = TRUE)[1:s]] <- 0
    
    # update beta1
    g <- 2 * Sigma1 %*% beta1hat - 2 * g1
    beta1hat <- beta1hat - eta * g
    beta1hat[-order(abs(beta1hat), decreasing = TRUE)[1:s]] <- 0
    
  }
  
  
  
  
}

# save result
valueresult3 <- result3
save(valueresult3, file = "valueresult3.RData")



