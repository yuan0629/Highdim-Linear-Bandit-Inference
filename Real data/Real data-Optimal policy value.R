library(ggplot2)
library(DebiasInfer)
library(caret)
library(glmnet)
library(latex2exp)
options(warn=-1)
load(here::here("Real data", "dataall.RData"))

Time <- nrow(dataallnew)
d <- ncol(dataallnew) + 1


# sample 700 optimal samples belongs to three arms, respectively
set.seed(0122)
permutation1 <- sample(which(reward == 2), 700)
permutation2 <- sample(which(reward == 1), 700)
permutation0 <- sample(which(reward == 0), 700)
permutation <- sample(c(permutation1, permutation2, permutation0), 2100)


### Setting 3: value inference

# tuning parameters
d <- ncol(dataallnew) + 1
eta <- 0.1
s <- 25
Time <- 5528
Iteration <- 1
iter <- 1

Converge2 <- matrix(0, ncol = 4, nrow = Time*Iteration)

beta2hat <- beta1hat <- beta0hat <- matrix(0, ncol = 1, nrow = d)
sigma2hat <- sigma1hat <- sigma0hat <- 0
Sigma2 <- Sigma1 <- Sigma0 <- matrix(0, ncol = d, nrow = d)
g0 <- g1 <- g2 <- matrix(0, ncol = 1, nrow = d)
m2t <- m1t <- m0t <- matrix(0, ncol = d, nrow = 1)
GT2 <- GT1 <- GT0 <- WT1 <- WT2 <- Vhat <- 0
T0new <- 1
Timenew <- 0

set.seed(1)
permutation <- sample(Time, Time)
 
for (t in permutation) {
    
  Xt <- matrix(c(1, as.numeric(dataallnew[t,])), ncol = 1)
  Timenew <- Timenew + 1
    
    
  # decision
  pit2 <- (t(beta2hat) %*% Xt > t(beta0hat) %*% Xt) && (t(beta2hat) %*% Xt > t(beta1hat) %*% Xt)[[1]]
  pit1 <- (t(beta1hat) %*% Xt > t(beta0hat) %*% Xt) && (t(beta1hat) %*% Xt > t(beta2hat) %*% Xt)[[1]]
  pit0 <- (t(beta0hat) %*% Xt > t(beta2hat) %*% Xt) && (t(beta0hat) %*% Xt > t(beta1hat) %*% Xt)[[1]]
    
  if ((pit2 == 0) && (pit1 == 0) && (pit0 == 0)) {
    pit2 <- pit1 <- pit0 <- 1/3
  }
  ac <- sample(c(0,1,2), prob = c(pit0, pit1, pit2), 1)
    
  y <- (ac == reward[t])
    
   
  # value inference
  Vhat <- Vhat + y
  if (ac == 1) {
    GT1 <- GT1 + 1
    WT1 <- WT1 + (t(beta1hat) %*% Xt)^2
    WT2 <- WT2 + t(beta1hat) %*% Xt
  } else if (ac == 0){
    GT0 <- GT0 + 1
    WT1 <- WT1 + (t(beta0hat) %*% Xt)^2
    WT2 <- WT2 + t(beta0hat) %*% Xt
  } else {
    GT2 <- GT2 + 1
    WT1 <- WT1 + (t(beta2hat) %*% Xt)^2
    WT2 <- WT2 + t(beta2hat) %*% Xt
  }
      
  # variance estimation
  sigma2hat <- sigma2hat + (ac == 2) * (y - t(Xt) %*% beta2hat)^2
  sigma1hat <- sigma1hat + (ac == 1) * (y - t(Xt) %*% beta1hat)^2
  sigma0hat <- sigma0hat + (ac == 0) * (y - t(Xt) %*% beta0hat)^2
      
      
  # CI
  Vhatt <- Vhat / Timenew
  SV <- (sigma1hat + sigma0hat + WT1 + sigma2hat) / Timenew - (WT2 /Timenew)^2
  Converge2[((iter-1)*Time + Timenew),1] <- Vhatt - qnorm(0.975) * sqrt(SV/Timenew)
  Converge2[((iter-1)*Time + Timenew),2] <- Vhatt 
  Converge2[((iter-1)*Time + Timenew),3] <- Vhatt + qnorm(0.975) * sqrt(SV/Timenew)
  
  # update online estimation

  Sigma0 <- (Timenew-1)/Timenew*Sigma0 + (ac==0) * 1/Timenew * Xt %*% t(Xt)
  Sigma1 <- (Timenew-1)/Timenew*Sigma1 + (ac==1) * 1/Timenew * Xt %*% t(Xt)
  Sigma2 <- (Timenew-1)/Timenew*Sigma2 + (ac==2) * 1/Timenew * Xt %*% t(Xt)

  g0 <- (Timenew-1)/Timenew*g0 + (ac==0) * 1/Timenew  * Xt %*% y
  g1 <- (Timenew-1)/Timenew*g1 + (ac==1) * 1/Timenew  * Xt %*% y
  g2 <- (Timenew-1)/Timenew*g2 + (ac==2) * 1/Timenew  * Xt %*% y


  # update beta0
  g <- 2 * Sigma0 %*% beta0hat - 2 * g0
  beta0hat <- beta0hat - eta * g
  beta0hat[-order(abs(beta0hat), decreasing = TRUE)[1:s]] <- 0


  # update beta1
  g <- 2 * Sigma1 %*% beta1hat - 2 * g1
  beta1hat <- beta1hat - eta * g
  beta1hat[-order(abs(beta1hat), decreasing = TRUE)[1:s]] <- 0

  # update beta2
  g <- 2 * Sigma2 %*% beta2hat - 2 * g2
  beta2hat <- beta2hat - eta * g
  beta2hat[-order(abs(beta2hat), decreasing = TRUE)[1:s]] <- 0
    
}
  



## OLS estimation as oracle ##

dataX <- data.matrix(dataallnew[permutation,])
model2olsnew <- lm((reward == 2)[permutation] ~ dataX)
model1olsnew <- lm((reward == 1)[permutation] ~ dataX)
model0olsnew <- lm((reward == 0)[permutation] ~ dataX)
beta2hat <- model2olsnew$coefficients
beta1hat <- model1olsnew$coefficients
beta0hat <- model0olsnew$coefficients
beta2hat[is.na(beta2hat)] <- 0
beta1hat[is.na(beta1hat)] <- 0
beta0hat[is.na(beta0hat)] <- 0


Timenew <- 0
  
set.seed(1)
permutation <- sample(Time, Time)
  
Vhat <- 0
  
for (t in permutation) {
    
  Xt <- matrix(c(1, as.numeric(dataallnew[t,])), ncol = 1)
  Timenew <- Timenew + 1
    
  # decision
  pit2 <- (t(beta2hat) %*% Xt > t(beta0hat) %*% Xt) && (t(beta2hat) %*% Xt > t(beta1hat) %*% Xt)[[1]]
  pit1 <- (t(beta1hat) %*% Xt > t(beta0hat) %*% Xt) && (t(beta1hat) %*% Xt > t(beta2hat) %*% Xt)[[1]]
  pit0 <- (t(beta0hat) %*% Xt > t(beta2hat) %*% Xt) && (t(beta0hat) %*% Xt > t(beta1hat) %*% Xt)[[1]]
    
  if ((pit2 == 0) && (pit1 == 0) && (pit0 == 0)) {
    pit2 <- pit1 <- pit0 <- 1/3
  }
  ac <- sample(c(0,1,2), prob = c(pit0, pit1, pit2), 1)
    
  y <- (ac == reward[t])
    
    
  # value inference
  Vhat <- Vhat + y
  Vhatt <- Vhat / Timenew
  Converge2[((iter-1)*Time + Timenew),4] <- Vhatt 
}
  

# combine the results
Converge <- data.frame(Time = rep(c(1:Time), 2))
Converge$method <- c(rep("Online", Time), rep("OLS", Time))
Converge$value <- Converge$low <- Converge$high <- 0
Converge$value[1:Time] <- Converge2[1:Time,2]
Converge$low[1:Time] <- Converge2[1:Time, 1]
Converge$high[1:Time] <- Converge2[1:Time, 3]
Converge$value[(Time+1):(2*Time)] <- Converge2[1:Time, 4]


## Figure 6: Comparison of optimal value estimation under our online estimator and OLS, and 95% CI ###

ggplot(data = Converge) + geom_line(aes(x = Time, y = value, color = method)) +  scale_color_manual(values = c("#fdb462", "#386cb0")) + coord_cartesian(ylim = c(0,1)) +theme_classic() +
  geom_ribbon(data = Converge[Converge$method == "Online",], aes(x = Converge[Converge$method == "Online","Time"], ymin = Converge[Converge$method == "Online", "low"], ymax = Converge[Converge$method == "Online","high"]), fill = "#386cb0", alpha = 0.3)
p1 <- ggplot(data = Converge) + geom_line(aes(x = Time, y = value, color = method)) +  scale_color_manual(values = c("#fdb462", "#386cb0")) + coord_cartesian(ylim = c(0,1)) +theme_classic()+
  geom_ribbon(data = Converge[Converge$method == "Online",], aes(x = Converge[Converge$method == "Online","Time"], ymin = Converge[Converge$method == "Online", "low"], ymax = Converge[Converge$method == "Online","high"]), fill = "#386cb0", alpha = 0.3)
# ggsave(here::here("Real data", "value.jpg"), plot = p1)


