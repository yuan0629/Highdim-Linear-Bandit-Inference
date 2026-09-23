library(ggplot2)
library(latex2exp)
library(caret)
library(glmnet)
library(patchwork)
library(ggpubr)


### parameter inference Figure 2###

options(warn=-1)


## scenario 1 (Top)
set.seed(629122)
# parameters
Time <- 300
d <- 600
s0 <- 3
cut <- 10
beta1 <- matrix(c(runif(s0, 0.5, 1), rep(0, d-s0)), ncol = 1)
beta0 <- matrix(c(runif(s0, 0.5, 1), rep(0, d-s0)), ncol = 1)
Iteration <- 100

load(here::here("Simulation", "Simulation-result", "scenario1-inf-result1.RData"))
load(here::here("Simulation", "Simulation-result", "scenario1-inf-result2.RData"))
load(here::here("Simulation", "Simulation-result", "scenario1-inf-result3.RData"))
load(here::here("Simulation", "Simulation-result", "scenario1-inf-result4.RData"))

exist <- c(1:Iteration)

# beta1 (top left)
value1df <- data.frame(variable = c(1:cut))
value1df$low <- value1df$value <- value1df$high <- 0
for (i in 1:cut) {
  outindex <- which.max(result1[result1[,9] == i, ][exist,3] - result1[result1[,9] == i, ][exist,2])
  value1df$low[i] <- mean(result1[result1[,9] == i, ][exist,2][-outindex])
  value1df$value[i] <- mean(result1[result1[,9] == i, ][exist,4][-outindex])
  value1df$high[i] <- mean(result1[result1[,9] == i, ][exist,3][-outindex])
}
value1df$method <- 1

value2df <- data.frame(variable = c(1:cut))
value2df$low <- value2df$value <- value2df$high <- 0
for (i in 1:cut) {
  value2df$low[i] <- mean(result2[result2[,9] == i, ][exist,2])
  value2df$value[i] <- mean(result2[result2[,9] == i, ][exist,4])
  value2df$high[i] <- mean(result2[result2[,9] == i, ][exist,3])
}
value2df$method <- 2

value3df <- data.frame(variable = c(1:cut))
value3df$low <- value3df$value <- value3df$high <- 0
for (i in 1:cut) {
  value3df$low[i] <- mean(result3[result3[,7] == i, ][exist,1])
  value3df$value[i] <- mean(result3[result3[,7] == i, ][exist,3])
  value3df$high[i] <- mean(result3[result3[,7] == i, ][exist,2])
}
value3df$method <- 3


value4df <- data.frame(variable = c(1:cut))
value4df$low <- value4df$value <- value4df$high <- 0
for (i in 1:cut) {
  value4df$low[i] <- mean(result4[result4[,9] == i, ][exist,2])
  value4df$value[i] <- mean(result4[result4[,9] == i, ][exist,4])
  value4df$high[i] <- mean(result4[result4[,9] == i, ][exist,3])
}
value4df$method <- 4

valuefinal <- rbind(value1df, value2df, value3df, value4df)
valuefinal$variable <- as.factor(valuefinal$variable)
valuefinal$method <- as.factor(valuefinal$method)

benchmark <- data.frame(value = rep(beta1[1:cut], 4), variable = rep(as.factor(c(1:cut)), 4), group = rep(c(1:4), each = cut))

ggplot() + 
  geom_point(data = valuefinal, aes(x = variable, y = value, group = method, color = method), position=position_dodge(0.6)) +   
  scale_color_manual(labels = c(TeX(r"($\epsilon_t=6t^{-1/3}$)"), TeX(r"($\epsilon_t=6t^{-1/2}$)"), "debiased Lasso", TeX(r"($\epsilon_t=0$     )")), values = c("#386cb0", "#fdb462", "#7fc97f", "#CF5640")) +  
  geom_point(data = benchmark, aes(x = variable, y = value, group = group), color = "red", position=position_dodge(0.6), alpha = 0.5) + 
  geom_errorbar(data = valuefinal, aes(x = variable, ymin = low, ymax = high, group = method, color = method),  position=position_dodge(0.6)) + xlab("variable") + ylab("") + theme_classic() + theme(axis.text.x = element_text(angle = 15, vjust = 0.5))


# ggarrange(common.legend = TRUE, legend = "bottom")

figparameter11 <- ggplot() + 
  geom_point(data = valuefinal, aes(x = variable, y = value, group = method, color = method), position=position_dodge(0.6)) +   
  scale_color_manual(labels = c(TeX(r"($\gamma=1/3$)"), TeX(r"($\gamma=1/2$)"), "debiased Lasso", TeX(r"($\epsilon_t=0$     )")), values = c("#386cb0", "#fdb462", "#7fc97f", "#CF5640")) +  
  geom_point(data = benchmark, aes(x = variable, y = value, group = group), color = "red", position=position_dodge(0.6), alpha = 0.5) + 
  geom_errorbar(data = valuefinal, aes(x = variable, ymin = low, ymax = high, group = method, color = method),  position=position_dodge(0.6)) + xlab("variable") + ylab("") + theme_classic() + theme(axis.text.x = element_text(angle = 15, vjust = 0.5)) +
  ggtitle(TeX(r"(Scenario(1) \  $\beta_1$)"))

#ggsave(here::here("Simulation", "Simulation-result", "figparameter11.jpg"), figparameter11, units = "px", width = 1704, height = 750)


# beta0 (top right)
value1df <- data.frame(variable = c(1:cut))
value1df$low <- value1df$value <- value1df$high <- 0
for (i in 1:cut) {
  value1df$low[i] <- mean(result1[result1[,9] == i, ][exist,6])
  value1df$value[i] <- mean(result1[result1[,9] == i, ][exist,8])
  value1df$high[i] <- mean(result1[result1[,9] == i, ][exist,7])
}
value1df$method <- 1

value2df <- data.frame(variable = c(1:cut))
value2df$low <- value2df$value <- value2df$high <- 0
for (i in 1:cut) {
  value2df$low[i] <- mean(result2[result2[,9] == i, ][exist,6])
  value2df$value[i] <- mean(result2[result2[,9] == i, ][exist,8])
  value2df$high[i] <- mean(result2[result2[,9] == i, ][exist,7])
}
value2df$method <- 2

value3df <- data.frame(variable = c(1:cut))
value3df$low <- value3df$value <- value3df$high <- 0
for (i in 1:cut) {
  value3df$low[i] <- mean(result3[result3[,7] == i, ][exist,4])
  value3df$value[i] <- mean(result3[result3[,7] == i, ][exist,6])
  value3df$high[i] <- mean(result3[result3[,7] == i, ][exist,5])
}
value3df$method <- 3

value4df <- data.frame(variable = c(1:cut))
value4df$low <- value4df$value <- value4df$high <- 0
for (i in 1:cut) {
  value4df$low[i] <- mean(result4[result4[,9] == i, ][exist,6])
  value4df$value[i] <- mean(result4[result4[,9] == i, ][exist,8])
  value4df$high[i] <- mean(result4[result4[,9] == i, ][exist,7])
}
value4df$method <- 4

valuefinal <- rbind(value1df, value2df, value3df, value4df)
valuefinal$variable <- as.factor(valuefinal$variable)
valuefinal$method <- as.factor(valuefinal$method)

benchmark <- data.frame(value = rep(beta0[1:cut], 4), variable = rep(as.factor(c(1:cut)), 4), group = rep(c(1:4), each = cut))

ggplot() + 
  geom_point(data = valuefinal, aes(x = variable, y = value, group = method, color = method), position=position_dodge(0.6)) +   
  scale_color_manual(labels = c(TeX(r"($\epsilon_t=6t^{-1/3}$)"), TeX(r"($\epsilon_t=6t^{-1/2}$)"), "debiased Lasso", TeX(r"($\epsilon_t=0$     )")), values = c("#386cb0", "#fdb462", "#7fc97f", "#CF5640")) +  
  geom_point(data = benchmark, aes(x = variable, y = value, group = group), color = "red", position=position_dodge(0.6), alpha = 0.5) + 
  geom_errorbar(data = valuefinal, aes(x = variable, ymin = low, ymax = high, group = method, color = method),  position=position_dodge(0.6)) + xlab("variable") + ylab("") + theme_classic() + theme(axis.text.x = element_text(angle = 15, vjust = 0.5))

figparameter10 <- ggplot() + 
  geom_point(data = valuefinal, aes(x = variable, y = value, group = method, color = method), position=position_dodge(0.6)) +   
  scale_color_manual(labels = c(TeX(r"($\gamma=1/3$)"), TeX(r"($\gamma=1/2$)"), "debiased Lasso", TeX(r"($\epsilon_t=0$     )")), values = c("#386cb0", "#fdb462", "#7fc97f", "#CF5640")) +  
  geom_point(data = benchmark, aes(x = variable, y = value, group = group), color = "red", position=position_dodge(0.6), alpha = 0.5) + 
  geom_errorbar(data = valuefinal, aes(x = variable, ymin = low, ymax = high, group = method, color = method),  position=position_dodge(0.6)) + xlab("variable") + ylab("") + theme_classic() + theme(axis.text.x = element_text(angle = 15, vjust = 0.5)) +
  ggtitle(TeX(r"(Scenario(1) \  $\beta_0$)"))

#ggsave(here::here("Simulation", "Simulation-result", "figparameter10.jpg"), figparameter10, units = "px", width = 1704, height = 750)




## scenario 2 (Middle)
set.seed(629122)
# parameters
Time <- 600
d <- 1000
s0 <- 8
cut <- 10
beta1 <- matrix(c(runif(s0, 0.5, 1), rep(0, d-s0)), ncol = 1)
beta0 <- matrix(c(runif(s0, 0.5, 1), rep(0, d-s0)), ncol = 1)
Iteration <- 100

load(here::here("Simulation", "Simulation-result", "scenario2-inf-result1.RData"))
load(here::here("Simulation", "Simulation-result", "scenario2-inf-result2.RData"))
load(here::here("Simulation", "Simulation-result", "scenario2-inf-result3.RData"))
load(here::here("Simulation", "Simulation-result", "scenario2-inf-result4.RData"))

exist <- c(1:Iteration)

# beta1 (Middle left)
value1df <- data.frame(variable = c(1:cut))
value1df$low <- value1df$value <- value1df$high <- 0
for (i in 1:cut) {
  outindex <- which.max(result1[result1[,9] == i, ][exist,3] - result1[result1[,9] == i, ][exist,2])
  value1df$low[i] <- mean(result1[result1[,9] == i, ][exist,2])
  value1df$value[i] <- mean(result1[result1[,9] == i, ][exist,4])
  value1df$high[i] <- mean(result1[result1[,9] == i, ][exist,3])
}
value1df$method <- 1

value2df <- data.frame(variable = c(1:cut))
value2df$low <- value2df$value <- value2df$high <- 0
for (i in 1:cut) {
  value2df$low[i] <- mean(result2[result2[,9] == i, ][exist,2])
  value2df$value[i] <- mean(result2[result2[,9] == i, ][exist,4])
  value2df$high[i] <- mean(result2[result2[,9] == i, ][exist,3])
}
value2df$method <- 2

value3df <- data.frame(variable = c(1:cut))
value3df$low <- value3df$value <- value3df$high <- 0
for (i in 1:cut) {
  value3df$low[i] <- mean(result3[result3[,7] == i, ][exist,1])
  value3df$value[i] <- mean(result3[result3[,7] == i, ][exist,3])
  value3df$high[i] <- mean(result3[result3[,7] == i, ][exist,2])
}
value3df$method <- 3


value4df <- data.frame(variable = c(1:cut))
value4df$low <- value4df$value <- value4df$high <- 0
for (i in 1:cut) {
  value4df$low[i] <- mean(result4[result4[,9] == i, ][exist,2])
  value4df$value[i] <- mean(result4[result4[,9] == i, ][exist,4])
  value4df$high[i] <- mean(result4[result4[,9] == i, ][exist,3])
}
value4df$method <- 4

valuefinal <- rbind(value1df, value2df, value3df, value4df)
valuefinal$variable <- as.factor(valuefinal$variable)
valuefinal$method <- as.factor(valuefinal$method)

benchmark <- data.frame(value = rep(beta1[1:cut], 4), variable = rep(as.factor(c(1:cut)), 4), group = rep(c(1:4), each = cut))

ggplot() + 
  geom_point(data = valuefinal, aes(x = variable, y = value, group = method, color = method), position=position_dodge(0.6)) +   
  scale_color_manual(labels = c(TeX(r"($\epsilon_t=10t^{-1/3}$)"), TeX(r"($\epsilon_t=10t^{-1/2}$)"), "debiased Lasso", TeX(r"($\epsilon_t=0$     )")), values = c("#386cb0", "#fdb462", "#7fc97f", "#CF5640")) +  
  geom_point(data = benchmark, aes(x = variable, y = value, group = group), color = "red", position=position_dodge(0.6), alpha = 0.5) + 
  geom_errorbar(data = valuefinal, aes(x = variable, ymin = low, ymax = high, group = method, color = method),  position=position_dodge(0.6)) + xlab("variable") + ylab("") + theme_classic() + theme(axis.text.x = element_text(angle = 15, vjust = 0.5))


figparameter21 <- ggplot() + 
  geom_point(data = valuefinal, aes(x = variable, y = value, group = method, color = method), position=position_dodge(0.6)) +   
  scale_color_manual(labels = c(TeX(r"($\gamma=1/3$)"), TeX(r"($\gamma=1/2$)"), "debiased Lasso", TeX(r"($\epsilon_t=0$     )")), values = c("#386cb0", "#fdb462", "#7fc97f", "#CF5640")) +  
  geom_point(data = benchmark, aes(x = variable, y = value, group = group), color = "red", position=position_dodge(0.6), alpha = 0.5) + 
  geom_errorbar(data = valuefinal, aes(x = variable, ymin = low, ymax = high, group = method, color = method),  position=position_dodge(0.6)) + xlab("variable") + ylab("") + theme_classic() + theme(axis.text.x = element_text(angle = 15, vjust = 0.5)) +
  ggtitle(TeX(r"(Scenario(2) \  $\beta_1$)"))



# beta0 (Middle right)
value1df <- data.frame(variable = c(1:cut))
value1df$low <- value1df$value <- value1df$high <- 0
for (i in 1:cut) {
  value1df$low[i] <- mean(result1[result1[,9] == i, ][exist,6])
  value1df$value[i] <- mean(result1[result1[,9] == i, ][exist,8])
  value1df$high[i] <- mean(result1[result1[,9] == i, ][exist,7])
}
value1df$method <- 1

value2df <- data.frame(variable = c(1:cut))
value2df$low <- value2df$value <- value2df$high <- 0
for (i in 1:cut) {
  value2df$low[i] <- mean(result2[result2[,9] == i, ][exist,6])
  value2df$value[i] <- mean(result2[result2[,9] == i, ][exist,8])
  value2df$high[i] <- mean(result2[result2[,9] == i, ][exist,7])
}
value2df$method <- 2

value3df <- data.frame(variable = c(1:cut))
value3df$low <- value3df$value <- value3df$high <- 0
for (i in 1:cut) {
  value3df$low[i] <- mean(result3[result3[,7] == i, ][exist,4])
  value3df$value[i] <- mean(result3[result3[,7] == i, ][exist,6])
  value3df$high[i] <- mean(result3[result3[,7] == i, ][exist,5])
}
value3df$method <- 3

value4df <- data.frame(variable = c(1:cut))
value4df$low <- value4df$value <- value4df$high <- 0
for (i in 1:cut) {
  value4df$low[i] <- mean(result4[result4[,9] == i, ][exist,6])
  value4df$value[i] <- mean(result4[result4[,9] == i, ][exist,8])
  value4df$high[i] <- mean(result4[result4[,9] == i, ][exist,7])
}
value4df$method <- 4

valuefinal <- rbind(value1df, value2df, value3df, value4df)
valuefinal$variable <- as.factor(valuefinal$variable)
valuefinal$method <- as.factor(valuefinal$method)

benchmark <- data.frame(value = rep(beta0[1:cut], 4), variable = rep(as.factor(c(1:cut)), 4), group = rep(c(1:4), each = cut))

ggplot() + 
  geom_point(data = valuefinal, aes(x = variable, y = value, group = method, color = method), position=position_dodge(0.6)) +   
  scale_color_manual(labels = c(TeX(r"($\epsilon_t=10t^{-1/3}$)"), TeX(r"($\epsilon_t=10t^{-1/2}$)"), "debiased Lasso", TeX(r"($\epsilon_t=0$     )")), values = c("#386cb0", "#fdb462", "#7fc97f", "#CF5640")) +  
  geom_point(data = benchmark, aes(x = variable, y = value, group = group), color = "red", position=position_dodge(0.6), alpha = 0.5) + 
  geom_errorbar(data = valuefinal, aes(x = variable, ymin = low, ymax = high, group = method, color = method),  position=position_dodge(0.6)) + xlab("") + ylab("") + theme_classic() + theme(axis.text.x = element_text(angle = 15, vjust = 0.5))


figparameter20 <- ggplot() + 
  geom_point(data = valuefinal, aes(x = variable, y = value, group = method, color = method), position=position_dodge(0.6)) +   
  scale_color_manual(labels = c(TeX(r"($\gamma=1/3$)"), TeX(r"($\gamma=1/2$)"), "debiased Lasso", TeX(r"($\epsilon_t=0$     )")), values = c("#386cb0", "#fdb462", "#7fc97f", "#CF5640")) +  
  geom_point(data = benchmark, aes(x = variable, y = value, group = group), color = "red", position=position_dodge(0.6), alpha = 0.5) + 
  geom_errorbar(data = valuefinal, aes(x = variable, ymin = low, ymax = high, group = method, color = method),  position=position_dodge(0.6)) + xlab("variable") + ylab("") + theme_classic() + theme(axis.text.x = element_text(angle = 15, vjust = 0.5)) +
  ggtitle(TeX(r"(Scenario(2) \  $\beta_0$)"))


## scenario 3 (Bottom)
set.seed(629122)
# parameters
Time <- 1000
d <- 2000
s0 <- 10
cut <- 10
beta1 <- matrix(c(runif(s0, 0.5, 1), rep(0, d-s0)), ncol = 1)
beta0 <- matrix(c(runif(s0, 0.5, 1), rep(0, d-s0)), ncol = 1)
Iteration <- 100

load(here::here("Simulation", "Simulation-result", "scenario3-inf-result1.RData"))
load(here::here("Simulation", "Simulation-result", "scenario3-inf-result2.RData"))
load(here::here("Simulation", "Simulation-result", "scenario3-inf-result3.RData"))
load(here::here("Simulation", "Simulation-result", "scenario3-inf-result4.RData"))

exist <- c(1:Iteration)


# beta1
value1df <- data.frame(variable = c(1:cut))
value1df$low <- value1df$value <- value1df$high <- 0
for (i in 1:cut) {
  outindex <- which.max(result1[result1[,9] == i, ][exist,3] - result1[result1[,9] == i, ][exist,2])
  value1df$low[i] <- mean(result1[result1[,9] == i, ][exist,2][-outindex])
  value1df$value[i] <- mean(result1[result1[,9] == i, ][exist,4][-outindex])
  value1df$high[i] <- mean(result1[result1[,9] == i, ][exist,3][-outindex])
}
value1df$method <- 1

value2df <- data.frame(variable = c(1:cut))
value2df$low <- value2df$value <- value2df$high <- 0
for (i in 1:cut) {
  outindex <- which.max(result2[result2[,9] == i, ][exist,3] - result2[result2[,9] == i, ][exist,2])
  value2df$low[i] <- mean(result2[result2[,9] == i, ][exist,2][-outindex])
  value2df$value[i] <- mean(result2[result2[,9] == i, ][exist,4][-outindex])
  value2df$high[i] <- mean(result2[result2[,9] == i, ][exist,3][-outindex])
}
value2df$method <- 2

value3df <- data.frame(variable = c(1:cut))
value3df$low <- value3df$value <- value3df$high <- 0
for (i in 1:cut) {
  value3df$low[i] <- mean(result3[result3[,7] == i, ][exist,1])
  value3df$value[i] <- mean(result3[result3[,7] == i, ][exist,3])
  value3df$high[i] <- mean(result3[result3[,7] == i, ][exist,2])
}
value3df$method <- 3


value4df <- data.frame(variable = c(1:cut))
value4df$low <- value4df$value <- value4df$high <- 0
for (i in 1:cut) {
  value4df$low[i] <- mean(result4[result4[,9] == i, ][exist,2])
  value4df$value[i] <- mean(result4[result4[,9] == i, ][exist,4])
  value4df$high[i] <- mean(result4[result4[,9] == i, ][exist,3])
}
value4df$method <- 4

valuefinal <- rbind(value1df, value2df, value3df, value4df)
valuefinal$variable <- as.factor(valuefinal$variable)
valuefinal$method <- as.factor(valuefinal$method)

benchmark <- data.frame(value = rep(beta1[1:cut], 4), variable = rep(as.factor(c(1:cut)), 4), group = rep(c(1:4), each = cut))

ggplot() + 
  geom_point(data = valuefinal, aes(x = variable, y = value, group = method, color = method), position=position_dodge(0.6)) +   
  scale_color_manual(labels = c(TeX(r"($\gamma=1/3$)"), TeX(r"($\gamma=1/2$)"), "debiased Lasso", TeX(r"($\epsilon_t=0$     )")), values = c("#386cb0", "#fdb462", "#7fc97f", "#CF5640")) +  
  geom_point(data = benchmark, aes(x = variable, y = value, group = group), color = "red", position=position_dodge(0.6), alpha = 0.5) + 
  geom_errorbar(data = valuefinal, aes(x = variable, ymin = low, ymax = high, group = method, color = method),  position=position_dodge(0.6)) + xlab("") + ylab("") + theme_classic() + theme(axis.text.x = element_text(angle = 15, vjust = 0.5))

figparameter31 <- ggplot() + 
  geom_point(data = valuefinal, aes(x = variable, y = value, group = method, color = method), position=position_dodge(0.6)) +   
  scale_color_manual(labels = c(TeX(r"($\gamma=1/3$)"), TeX(r"($\gamma=1/2$)"), "debiased Lasso", TeX(r"($\epsilon_t=0$     )")), values = c("#386cb0", "#fdb462", "#7fc97f", "#CF5640")) +  
  geom_point(data = benchmark, aes(x = variable, y = value, group = group), color = "red", position=position_dodge(0.6), alpha = 0.5) + 
  geom_errorbar(data = valuefinal, aes(x = variable, ymin = low, ymax = high, group = method, color = method),  position=position_dodge(0.6)) + xlab("variable") + ylab("") + theme_classic() + theme(axis.text.x = element_text(angle = 15, vjust = 0.5)) +
  ggtitle(TeX(r"(Scenario(3) \  $\beta_1$)"))


# beta0
value1df <- data.frame(variable = c(1:cut))
value1df$low <- value1df$value <- value1df$high <- 0
for (i in 1:cut) {
  value1df$low[i] <- mean(result1[result1[,9] == i, ][exist,6])
  value1df$value[i] <- mean(result1[result1[,9] == i, ][exist,8])
  value1df$high[i] <- mean(result1[result1[,9] == i, ][exist,7])
}
value1df$method <- 1

value2df <- data.frame(variable = c(1:cut))
value2df$low <- value2df$value <- value2df$high <- 0
for (i in 1:cut) {
  value2df$low[i] <- mean(result2[result2[,9] == i, ][exist,6])
  value2df$value[i] <- mean(result2[result2[,9] == i, ][exist,8])
  value2df$high[i] <- mean(result2[result2[,9] == i, ][exist,7])
}
value2df$method <- 2

value3df <- data.frame(variable = c(1:cut))
value3df$low <- value3df$value <- value3df$high <- 0
for (i in 1:cut) {
  value3df$low[i] <- mean(result3[result3[,7] == i, ][exist,4])
  value3df$value[i] <- mean(result3[result3[,7] == i, ][exist,6])
  value3df$high[i] <- mean(result3[result3[,7] == i, ][exist,5])
}
value3df$method <- 3

value4df <- data.frame(variable = c(1:cut))
value4df$low <- value4df$value <- value4df$high <- 0
for (i in 1:cut) {
  value4df$low[i] <- mean(result4[result4[,9] == i, ][exist,6])
  value4df$value[i] <- mean(result4[result4[,9] == i, ][exist,8])
  value4df$high[i] <- mean(result4[result4[,9] == i, ][exist,7])
}
value4df$method <- 4

valuefinal <- rbind(value1df, value2df, value3df, value4df)
valuefinal$variable <- as.factor(valuefinal$variable)
valuefinal$method <- as.factor(valuefinal$method)

benchmark <- data.frame(value = rep(beta0[1:cut], 4), variable = rep(as.factor(c(1:cut)), 4), group = rep(c(1:4), each = cut))

ggplot() + 
  geom_point(data = valuefinal, aes(x = variable, y = value, group = method, color = method), position=position_dodge(0.6)) +   
  scale_color_manual(labels = c(TeX(r"($\gamma=1/3$)"), TeX(r"($\gamma=1/2$)"), "debiased Lasso", TeX(r"($\epsilon_t=0$     )")), values = c("#386cb0", "#fdb462", "#7fc97f", "#CF5640")) +  
  geom_point(data = benchmark, aes(x = variable, y = value, group = group), color = "red", position=position_dodge(0.6), alpha = 0.5) + 
  geom_errorbar(data = valuefinal, aes(x = variable, ymin = low, ymax = high, group = method, color = method),  position=position_dodge(0.6)) + xlab("") + ylab("") + theme_classic() + theme(axis.text.x = element_text(angle = 15, vjust = 0.5)) +
  ggtitle(TeX(r"(Scenario(3) \ $\beta_0$)"))

figparameter30 <- ggplot() + 
  geom_point(data = valuefinal, aes(x = variable, y = value, group = method, color = method), position=position_dodge(0.6)) +   
  scale_color_manual(labels = c(TeX(r"($\gamma=1/3$)"), TeX(r"($\gamma=1/2$)"), "debiased Lasso", TeX(r"($\epsilon_t=0$     )")), values = c("#386cb0", "#fdb462", "#7fc97f", "#CF5640")) +  
  geom_point(data = benchmark, aes(x = variable, y = value, group = group), color = "red", position=position_dodge(0.6), alpha = 0.5) + 
  geom_errorbar(data = valuefinal, aes(x = variable, ymin = low, ymax = high, group = method, color = method),  position=position_dodge(0.6)) + xlab("variable") + ylab("") + theme_classic() + theme(axis.text.x = element_text(angle = 15, vjust = 0.5)) +
  ggtitle(TeX(r"(Scenario(3) \  $\beta_0$)"))



ggarrange(figparameter11, figparameter10, figparameter21, figparameter20, figparameter31, figparameter30,
          ncol = 2, nrow = 3,  align = "hv", 
          common.legend = TRUE)

totalparameter <- ggarrange(figparameter11, figparameter10, figparameter21, figparameter20, figparameter31, figparameter30,
                            ncol = 2, nrow = 3,  align = "hv", 
                            common.legend = TRUE)

ggsave(here::here("Simulation", "Simulation-result", "totalparameter.pdf"),
       plot = totalparameter, device = grDevices::cairo_pdf,
       units = "px", width = 3700, height = 3000)



