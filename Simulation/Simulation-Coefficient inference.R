library(ggplot2)
library(DebiasInfer)
library(glmnet)
library(truncnorm)
#library(hdi)

options(warn=-1)
setwd("")

set.seed(629122)

### three scenarios to produce Figure 2 Top, Middle and Bottom ###

# parameters (scenario 1)
Time <- 300 # time horizon T
d <- 600  # dimension of covariate X_t
sigma1 <- sigma0 <- 0.1 # noise level
s0 <- 3 # true sparsity
s <- 10 # sparsity used in hard-thresholding
cut <- 10 # number of coefficients to be presented in Figure 2
beta1 <- matrix(c(runif(s0, 0.5, 1), rep(0, d-s0)), ncol = 1) # true \beta_1
beta0 <- matrix(c(runif(s0, 0.5, 1), rep(0, d-s0)), ncol = 1) # true \beta_0
K <- 150 # number of iterations to solve (9) to obtain the de-correlation matrix M_T
Iteration <- 100


# parameters (scenario 2)
# Time <- 600 # time horizon T
# d <- 1000  # dimension of covariate X_t
# sigma1 <- sigma0 <- 0.1 # noise level
# s0 <- 8 # true sparsity
# s <- 15 # sparsity used in hard-thresholding
# cut <- 10 # number of coefficients to be presented in Figure 2
# beta1 <- matrix(c(runif(s0, 0.5, 1), rep(0, d-s0)), ncol = 1) # true \beta_1
# beta0 <- matrix(c(runif(s0, 0.5, 1), rep(0, d-s0)), ncol = 1) # true \beta_0
# K <- 150 # number of iterations to solve (9) to obtain the de-correlation matrix M_T
# Iteration <- 100


# parameters (scenario 3)
# Time <- 1000 # time horizon T
# d <- 2000  # dimension of covariate X_t
# sigma1 <- sigma0 <- 0.1 # noise level
# s0 <- 10 # true sparsity
# s <- 20 # sparsity used in hard-thresholding
# cut <- 10 # number of coefficients to be presented in Figure 2
# beta1 <- matrix(c(runif(s0, 0.5, 1), rep(0, d-s0)), ncol = 1) # true \beta_1
# beta0 <- matrix(c(runif(s0, 0.5, 1), rep(0, d-s0)), ncol = 1) # true \beta_0
# K <- 150 # number of iterations to solve (9) to obtain the de-correlation matrix M_T
# Iteration <- 100

### Setting 1: IPW estimator (\gamma=1/3 and 1/2)
gamma <- 1/3 
c2 <- 4 # define the exploration probability \varepsilon_t=c_2t^{-gamma}; change between "gamma <- 1/3" and "gamma <- 1/2" to 
            #modify \gamma to produce the results in Figure 2
eta <- 0.09 # stepsize
mu <- 0.0005 # parameter to construct the de-correlation matrix; \mu_{T1} in (9) of paper


# IPW estimator
result1 <- matrix(0, nrow = Iteration*cut, ncol = 9) # store the point and interval estimators
result1[,9] <- rep(c(1:cut), Iteration)


for (iter in 1:Iteration) {
  
  print(iter)
  set.seed(iter)
  
  #initialization
  beta1hat <- beta0hat <- matrix(0, ncol = 1, nrow = d)
  ipw1sum1 <- ipw1sum2 <- ipw0sum1 <- ipw0sum2 <- 0
  sigma1hat <- sigma0hat <- 0
  Sigma <- Lambda1 <- Lambda0 <- Sigma1 <- Sigma0 <- matrix(0, ncol = d, nrow = d)
  g0 <- g1 <- matrix(0, ncol = 1, nrow = d)
  S1 <- S0 <- rep(0, cut)
  
  MT <- matrix(0, ncol = d, nrow = cut) 
  
  set.seed(iter)
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
    
    # parameter inference (part of summation in (7))
      
    ipw1sum1 <- ipw1sum1 + (ac==1) * 1/pit * Xt %*% y
    ipw1sum2 <- ipw1sum2 + (ac==1) * 1/pit * Xt %*% t(Xt)
    ipw0sum1 <- ipw0sum1 + (ac==0) * 1/(1-pit) * Xt %*% y
    ipw0sum2 <- ipw0sum2 + (ac==0) * 1/(1-pit) * Xt %*% t(Xt)
    
    
    
    # variance estimation
    if (t >= 1/2*Time) {
      sigma1hat <- sigma1hat + (ac == 1) / pit * (y - t(Xt) %*% beta1hat)^2
      sigma0hat <- sigma0hat + (ac == 0) / (1-pit) * (y - t(Xt) %*% beta0hat)^2
    }

    
    # update online estimators
    
    Sigma0 <- (t-1)/t*Sigma0 + (ac==0) * 1/t * 1/(1-pit) * Xt %*% t(Xt)  # (line 10 of Algorithm 1)
    Sigma1 <- (t-1)/t*Sigma1 + (ac==1) * 1/t * 1/pit * Xt %*% t(Xt)
    g0 <- (t-1)/t*g0 + (ac==0) * 1/t * 1/(1-pit) * Xt %*% y  # (line 11)
    g1 <- (t-1)/t*g1 + (ac==1) * 1/t * 1/pit * Xt %*% y 
    
    
    # update beta0 (line 12)
    g <- 2 * Sigma0 %*% beta0hat - 2 * g0
    beta0hat <- beta0hat - eta * g
    beta0hat[-order(abs(beta0hat), decreasing = TRUE)[1:s]] <- 0
    
    
    # update beta1
    g <- 2 * Sigma1 %*% beta1hat - 2 * g1
    beta1hat <- beta1hat - eta * g
    beta1hat[-order(abs(beta1hat), decreasing = TRUE)[1:s]] <- 0
    
    Sigma <- (t-1)/t * Sigma + 1/t * Xt %*% t(Xt)
    Lambda0 <- (t-1)/t*Lambda0 + (ac==0) * 1/t * Xt %*% t(Xt)
    Lambda1 <- (t-1)/t*Lambda1 + (ac==1) * 1/t * Xt %*% t(Xt)
  }
  

  # IPW parameter debiasing
  for (l in 1:cut) {  # debiasing for \beta_{i(l)}
    mt <- matrix(0, ncol = d, nrow = 1)
    for (k in 1:K) { # solve (9) to construct de-correlation matrix M_{T}
      for (j in 1:d) {
        mt[j] <- 1/Sigma[j, j] * SoftThres(- Sigma[j, -j] %*% t(mt)[-j] + (l == j), mu)
      }
    }
    MT[l,] <- mt
    # variance estimator S_{i(l)}
    S1[l] <- 2 / c2 / (1+gamma) * mt %*% Lambda0 %*% t(mt) + Time^(-gamma) * mt %*% Lambda1 %*% t(mt) 
    S0[l] <- 2 / c2/  (1+gamma) * mt %*% Lambda1 %*% t(mt) + Time^(-gamma) * mt %*% Lambda0 %*% t(mt)
    
  }
  
  # IPW coefficient debiased estimator in (7)
  beta1ipw <- beta1hat[1:cut] + 1/Time * MT %*% (ipw1sum1- ipw1sum2 %*% beta1hat)  
  beta0ipw <- beta0hat[1:cut] + 1/Time * MT %*% (ipw0sum1- ipw0sum2 %*% beta0hat)
  # noise level estimators
  sigma1hat <- sigma1hat / (1/2*Time) 
  sigma0hat <- sigma0hat / (1/2*Time)
  
  
  # store the result
  
  # \beta_1
  result1[(((iter-1)*cut + 1):(iter*cut)), 1] <- (beta1ipw - beta1[1:cut]) / sqrt(sigma1hat*S1/Time^(1-gamma)) # normalized value
  result1[(((iter-1)*cut + 1):(iter*cut)), 2] <- beta1ipw - qnorm(0.975) * sqrt(sigma1hat * S1 / Time^(1-gamma)) # 95% CI
  result1[(((iter-1)*cut + 1):(iter*cut)), 3] <- beta1ipw + qnorm(0.975) * sqrt(sigma1hat * S1 / Time^(1-gamma))
  result1[(((iter-1)*cut + 1):(iter*cut)), 4] <- beta1ipw # point estimator
  # \beta_0                                              
  result1[(((iter-1)*cut + 1):(iter*cut)), 5] <- (beta0ipw - beta0[1:cut]) / sqrt(sigma0hat*S0/Time^(1-gamma)) # normalized value
  result1[(((iter-1)*cut + 1):(iter*cut)), 6] <- beta0ipw - qnorm(0.975) * sqrt(sigma0hat * S0 / Time^(1-gamma)) # 95% CI
  result1[(((iter-1)*cut + 1):(iter*cut)), 7] <- beta0ipw + qnorm(0.975) * sqrt(sigma0hat * S0 / Time^(1-gamma))
  result1[(((iter-1)*cut + 1):(iter*cut)), 8] <- beta0ipw # point estimator
  
}


# save result
save(result1, file = "scenario3result1.RData")

### Setting 2: offline debiased Lasso (Montanari 2014)

mu <- 0.1 # parameter to construct the de-correlation matrix; \mu_{T2} in (10)
K <- 50 # number of iterations to solve the de-correlation matrix

result3 <- matrix(0, nrow = Iteration*cut, ncol = 7) # store the point and interval estimators
result3[,7] <- rep(c(1:cut), Iteration)


for (iter in 1:Iteration) {

  print(iter)
  set.seed(iter)
  
  # generate T/2 i.i.d. samples for both arms
  Xall1 <- matrix(rtruncnorm(Time*d/2, a=-1, b=1), nrow = Time/2, ncol = d)
  Yall1 <- Xall1 %*% beta1 + rnorm(Time/2, 0, sigma1)
  
  Xall0 <- matrix(rtruncnorm(Time*d/2, a=-1, b=1), nrow = Time/2, ncol = d)
  Yall0 <- Xall0 %*% beta0 + rnorm(Time/2, 0, sigma0)
  
  # arm 1
  
  fitlasso <- glmnet(Xall1, Yall1, alpha = 1, intercept = FALSE, lambda = 0.05) # initial Lasso estimator
  initlasso <- as.matrix(coef(fitlasso)[-1], ncol = 1)
  # debiased Lasso
  SigmaX <- 1 / nrow(Xall1) * t(Xall1) %*% Xall1
  M <- matrix(0, ncol = d, nrow = cut)
  Soff1 <- rep(0, cut)
  for (l in 1:cut) {
    mt <- matrix(0, ncol = d, nrow = 1)
    for (k in 1:K) {
      for (j in 1:d) {
        mt[j] <- 1/SigmaX[j, j] * SoftThres(- SigmaX[j, -j] %*% t(mt)[-j] + (l == j), mu)
      }
    }
    M[l,] <- mt
    Soff1[l] <-  mt %*% SigmaX %*% t(mt)
  }
  debiaslasso1 <- initlasso[1:cut] + 1 / nrow(Xall1) * M %*% t(Xall1) %*% (Yall1 - Xall1 %*% initlasso) # debiased Lasso estimator
  sigma1hat <- mean((Yall1 - Xall1 %*% initlasso)^2) # noise level estimator
  
  # arm0
  
  fitlasso <- glmnet(Xall0, Yall0, alpha = 1, intercept = FALSE, lambda = 0.05) # initial Lasso estimator
  initlasso <- as.matrix(coef(fitlasso)[-1], ncol = 1)
  # debiased Lasso
  SigmaX <- 1 / nrow(Xall0) * t(Xall0) %*% Xall0
  M <- matrix(0, ncol = d, nrow = cut)
  Soff0 <- rep(0, cut)
  for (l in 1:cut) {
    mt <- matrix(0, ncol = d, nrow = 1)
    for (k in 1:K) {
      for (j in 1:d) {
        mt[j] <- 1/SigmaX[j, j] * SoftThres(- SigmaX[j, -j] %*% t(mt)[-j] + (l == j), mu)
      }
    }
    M[l,] <- mt
    Soff0[l] <-  mt %*% SigmaX %*% t(mt)
  }
  debiaslasso0 <- initlasso[1:cut] + 1 / nrow(Xall0) * M %*% t(Xall0) %*% (Yall0 - Xall0 %*% initlasso) # debiased Lasso estimator
  sigma0hat <- mean((Yall0 - Xall0 %*% initlasso)^2) # noise level estimator
  
  result3[(((iter-1)*cut + 1):(iter*cut)), 1] <- debiaslasso1 - qnorm(0.975) * sqrt(sigma1hat * Soff1 / (Time/2)) # 95% CI
  result3[(((iter-1)*cut + 1):(iter*cut)), 2] <- debiaslasso1 + qnorm(0.975) * sqrt(sigma1hat * Soff1 / (Time/2)) 
  result3[(((iter-1)*cut + 1):(iter*cut)), 3] <- debiaslasso1 # point estimator
  
  result3[(((iter-1)*cut + 1):(iter*cut)), 4] <- debiaslasso0 - qnorm(0.975) * sqrt(sigma0hat * Soff0 / (Time/2)) # 95% CI
  result3[(((iter-1)*cut + 1):(iter*cut)), 5] <- debiaslasso0 + qnorm(0.975) * sqrt(sigma0hat * Soff0 / (Time/2))
  result3[(((iter-1)*cut + 1):(iter*cut)), 6] <- debiaslasso0 # point estimator
  
}

# save result
save(result3, file = "scenario3result3.RData")






### Setting 3: AW estimator

K <- 150 # number of iterations to solve (10) to obtain the de-correlation matrix M_T^{(i)}
mu <- 0.0005 # parameter to construct the de-correlation matrix; \mu_{T2} in (10)
Iteration <- 100

result4 <- matrix(0, nrow = Iteration*cut, ncol = 9)  # store the point and interval estimators
result4[,9] <- rep(c(1:cut), Iteration)

for (iter in 1:Iteration) {

  print(iter)
  set.seed(iter)
  
  #initialization  
  beta1hat <- beta0hat <- matrix(0, ncol = 1, nrow = d)
  aw1sum1 <- aw1sum2 <- aw0sum1 <- aw0sum2 <- 0
  sigma1hat <- sigma0hat <- 0
  Sigma1 <- Sigma0 <- matrix(0, ncol = d, nrow = d)
  g0 <- g1 <- matrix(0, ncol = 1, nrow = d)
  GT1 <- GT0 <- WT1 <- WT2 <- Vhat <- 0
  S1 <- S0 <- rep(0, cut)
  GT11 <- GT01 <- 0
  
  M1T <- M0T <- matrix(0, ncol = d, nrow = cut)
  
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
    
    
    # parameter inference
    
    if (ac == 1) {
        
      GT1 <- GT1 + 1 # number of samples assgined to arm 1
        
      aw1sum1 <- aw1sum1 + Xt %*% y  # part of summation in AW debiased estimator
      aw1sum2 <- aw1sum2 + Xt %*% t(Xt)
        
      } else {
        
      GT0 <- GT0 + 1  # number of samples assgined to arm 0
        
      aw0sum1 <- aw0sum1 + Xt %*% y  # part of summation in AW debiased estimator
      aw0sum2 <- aw0sum2 + Xt %*% t(Xt)
        
      }
      
    
    
    # update online estimators
    
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
    
    # variance estimation (in practice, can use data after T/4 for variance estimation since (y_t - X_t\hat{\beta}_{i,t})^2 are more precise)
    if (t >= Time/4) {
      sigma1hat <- sigma1hat + (ac == 1)  * (y - t(Xt) %*% beta1hat)^2
      sigma0hat <- sigma0hat + (ac == 0)  * (y - t(Xt) %*% beta0hat)^2 
      GT11 <- GT11 + (ac == 1)
      GT01 <- GT01 + (ac == 0)
    }
    
  }
  
  # AW debiasing
  for (l in 1:cut) { # debiasing for \beta_{1(l)}
    m1t <- matrix(0, ncol = d, nrow = 1) # solve (10) to construct de-correlation matrix M_{T}^{(1)}
    for (k in 1:K) {
      for (j in 1:d) {
        m1t[j] <- 1/(Sigma1[j, j]) * SoftThres(- Sigma1[j, -j] %*% t(m1t)[-j] + (l == j), mu)
      }
    }
    M1T[l,] <- m1t
    S1[l] <- m1t %*% Sigma1 %*% t(m1t) # variance estimator \hat{\Lambda}_{1(ll)}^{-1} 
  }
  
  for (l in 1:cut) { # debiasing for \beta_{0(l)}
    m0t <- matrix(0, ncol = d, nrow = 1)  # solve (10) to construct de-correlation matrix M_{T}^{(0)}
    for (k in 1:K) {
      for (j in 1:d) {
        m0t[j] <- 1/(Sigma0[j, j]) * SoftThres(- Sigma0[j, -j] %*% t(m0t)[-j] + (l == j), mu)
      }
    }
    M0T[l,] <- m0t
    S0[l] <- m0t %*% Sigma0 %*% t(m0t)  # variance estimator \hat{\Lambda}_{0(ll)}^{-1} 
  }
  
  # noise level estimator
  sigma1hat <- sigma1hat / GT11
  sigma0hat <- sigma0hat / GT01
  
  # AW debiasing estimator in
  beta1aw <- beta1hat[1:cut] + 1/Time * M1T %*% (aw1sum1- aw1sum2 %*% beta1hat)
  beta0aw <- beta0hat[1:cut] + 1/Time * M0T %*% (aw0sum1- aw0sum2 %*% beta0hat)

  
  result4[(((iter-1)*cut + 1):(iter*cut)), 1] <- (beta1aw - beta1[1:cut]) / sqrt(sigma1hat*S1/Time) # normalized value
  result4[(((iter-1)*cut + 1):(iter*cut)), 2] <- beta1aw - qnorm(0.975) * sqrt(sigma1hat * S1 / Time) # 95% CI
  result4[(((iter-1)*cut + 1):(iter*cut)), 3] <- beta1aw + qnorm(0.975) * sqrt(sigma1hat * S1 / Time)
  result4[(((iter-1)*cut + 1):(iter*cut)), 4] <- beta1aw # point estimator
  
  result4[(((iter-1)*cut + 1):(iter*cut)), 5] <- (beta0aw - beta0[1:cut]) / sqrt(sigma0hat*S0/Time) # normalized value
  result4[(((iter-1)*cut + 1):(iter*cut)), 6] <- beta0aw - qnorm(0.975) * sqrt(sigma0hat * S0 / Time)
  result4[(((iter-1)*cut + 1):(iter*cut)), 7] <- beta0aw + qnorm(0.975) * sqrt(sigma0hat * S0 / Time)
  result4[(((iter-1)*cut + 1):(iter*cut)), 8] <- beta0aw # point estimator
  
  
}

save(result4, file = "scenario1result4.RData")




