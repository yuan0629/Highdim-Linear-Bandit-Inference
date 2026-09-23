library(ggplot2)
library(latex2exp)
library(caret)
library(glmnet)
library(patchwork)
library(ggpubr)



### online estimation Figure 1 ###


options(warn=-1)

## scenario 1 (Left panel)
Time <- 300
Iteration <- 100
load(here::here("Simulation", "Simulation-result", "scenario1-sgd-result1.RData"))
load(here::here("Simulation", "Simulation-result", "scenario1-sgd-result2.RData"))
load(here::here("Simulation", "Simulation-result", "scenario1-sgd-result3.RData"))

df1 <- data.frame(result1)
colnames(df1)[1:3] <- c("beta1", "beta0", "time")
df1$method <- "df1"
df1$iter <- rep(1:Iteration, each = Time)

df2 <- data.frame(result2)
colnames(df2)[1:3] <- c("beta1", "beta0", "time")
df2$method <- "df2"
df2$iter <- rep(1:Iteration, each = Time)

df3 <- data.frame(result3)
colnames(df3)[1:3] <- c("beta1", "beta0", "time")
df3$method <- "df3"
df3$iter <- rep(1:Iteration, each = Time)


# Combine all data frames
df_all <- rbind(df1, df2, df3)
# start from later time...
df_all <- df_all[df_all$time >= 50, ]

# beta1
df_summary <- aggregate(beta1 ~ method + time, data = df_all, FUN = mean)
# Plot
figsgd11 <- ggplot(df_summary, aes(x = time, y = beta1, color = method, linetype = method)) +
  geom_line(linewidth = 1) +
  labs(
    x = "Time",
    y = TeX(r"($\|\hat{\beta}_{1,t}-\beta_1\|^2$)")
  ) +
  scale_color_manual(
    name = "method", 
    labels = c(TeX(r"($\gamma=1/3$)"), TeX(r"($\gamma=1/2$)"), TeX(r"($\epsilon_t=0$     )")),
    values = c("#386cb0", "#fdb462", "#CF5640")
  ) +
  scale_linetype_manual(
    name = "method", 
    labels = c(TeX(r"($\gamma=1/3$)"), TeX(r"($\gamma=1/2$)"), TeX(r"($\epsilon_t=0$     )")),
    values = c("solid", "dashed", "dotted") # Adjust linetypes 
  ) +
  theme_minimal()
figsgd11 
#ggsave(here::here("Simulation", "Simulation-result", "figsgd11.pdf"),
#       plot = figsgd11, units = "in", width = 5.82, height = 3.61)


# beta0
df_summary <- aggregate(beta0 ~ method + time, data = df_all, FUN = mean)
# Plot
figsgd10 <- ggplot(df_summary, aes(x = time, y = beta0, color = method, linetype = method)) +
  geom_line(linewidth = 1) +
  labs(
    x = "Time",
    y = TeX(r"($\|\hat{\beta}_{0,t}-\beta_0\|^2$)")
  ) +
  scale_color_manual(
    name = "method", 
    labels = c(TeX(r"($\gamma=1/3$)"), TeX(r"($\gamma=1/2$)"), TeX(r"($\epsilon_t=0$     )")),
    values = c("#386cb0", "#fdb462", "#CF5640")
  ) +
  scale_linetype_manual(
    name = "method", 
    labels = c(TeX(r"($\gamma=1/3$)"), TeX(r"($\gamma=1/2$)"), TeX(r"($\epsilon_t=0$     )")),
    values = c("solid", "dashed", "dotted") # Adjust linetypes
  ) +
  theme_minimal()
figsgd10 
#ggsave(here::here("Simulation", "Simulation-result", "figsgd10.pdf"),
#       plot = figsgd10, units = "in", width = 5.82, height = 3.61)

## scenario 2 (Center panel)
Time <- 600
Iteration <- 100
load(here::here("Simulation", "Simulation-result", "scenario2-sgd-result1.RData"))
load(here::here("Simulation", "Simulation-result", "scenario2-sgd-result2.RData"))
load(here::here("Simulation", "Simulation-result", "scenario2-sgd-result3.RData"))

df1 <- data.frame(result1)
colnames(df1)[1:3] <- c("beta1", "beta0", "time")
df1$method <- "df1"
df1$iter <- rep(1:Iteration, each = Time)

df2 <- data.frame(result2)
colnames(df2)[1:3] <- c("beta1", "beta0", "time")
df2$method <- "df2"
df2$iter <- rep(1:Iteration, each = Time)

df3 <- data.frame(result3)
colnames(df3)[1:3] <- c("beta1", "beta0", "time")
df3$method <- "df3"
df3$iter <- rep(1:Iteration, each = Time)


# Combine all data frames
df_all <- rbind(df1, df2, df3)
# start from later time...
df_all <- df_all[df_all$time >= 200, ]

# beta1
df_summary <- aggregate(beta1 ~ method + time, data = df_all, FUN = mean)
# Plot
figsgd21 <- ggplot(df_summary, aes(x = time, y = beta1, color = method, linetype = method)) +
  geom_line(linewidth = 1) +
  labs(
    x = "Time",
    y = TeX(r"($\|\hat{\beta}_{1,t}-\beta_1\|^2$)")
  ) +
  scale_color_manual(
    name = "method", 
    labels = c(TeX(r"($\gamma=1/3$)"), TeX(r"($\gamma=1/2$)"), TeX(r"($\epsilon_t=0$     )")),
    values = c("#386cb0", "#fdb462", "#CF5640")
  ) +
  scale_linetype_manual(
    name = "method", 
    labels = c(TeX(r"($\gamma=1/3$)"), TeX(r"($\gamma=1/2$)"), TeX(r"($\epsilon_t=0$     )")),
    values = c("solid", "dashed", "dotted") # Adjust linetypes 
  ) +
  theme_minimal()
figsgd21 
#ggsave(here::here("Simulation", "Simulation-result", "figsgd21.pdf"), plot = figsgd21, units = "in", width = 5.82, height = 3.61)

# beta0
df_summary <- aggregate(beta0 ~ method + time, data = df_all, FUN = mean)
# Plot
figsgd20 <- ggplot(df_summary, aes(x = time, y = beta0, color = method, linetype = method)) +
  geom_line(linewidth = 1) +
  labs(
    x = "Time",
    y = TeX(r"($\|\hat{\beta}_{0,t}-\beta_0\|^2$)")
  ) +
  scale_color_manual(
    name = "method", 
    labels = c(TeX(r"($\gamma=1/3$)"), TeX(r"($\gamma=1/2$)"), TeX(r"($\epsilon_t=0$     )")),
    values = c("#386cb0", "#fdb462", "#CF5640")
  ) +
  scale_linetype_manual(
    name = "method", 
    labels = c(TeX(r"($\gamma=1/3$)"), TeX(r"($\gamma=1/2$)"), TeX(r"($\epsilon_t=0$     )")),
    values = c("solid", "dashed", "dotted") # Adjust linetypes 
  ) +
  theme_minimal()
figsgd20 
#ggsave(here::here("Simulation", "Simulation-result", "figsgd20.pdf"), plot = figsgd20, units = "in", width = 5.82, height = 3.61)


## scenario 3  (Right panel)
Time <- 1000
load(here::here("Simulation", "Simulation-result", "scenario3-sgd-result1.RData"))
load(here::here("Simulation", "Simulation-result", "scenario3-sgd-result2.RData"))
load(here::here("Simulation", "Simulation-result", "scenario3-sgd-result3.RData"))

df1 <- data.frame(result1)
colnames(df1)[1:3] <- c("beta1", "beta0", "time")
df1$method <- "df1"
df1$iter <- rep(1:Iteration, each = Time)

df2 <- data.frame(result2)
colnames(df2)[1:3] <- c("beta1", "beta0", "time")
df2$method <- "df2"
df2$iter <- rep(1:Iteration, each = Time)

df3 <- data.frame(result3)
colnames(df3)[1:3] <- c("beta1", "beta0", "time")
df3$method <- "df3"
df3$iter <- rep(1:Iteration, each = Time)


# Combine all data frames
df_all <- rbind(df1, df2, df3)
# start from later time...
df_all <- df_all[df_all$time >= 600, ]

# beta1
df_summary <- aggregate(beta1 ~ method + time, data = df_all, FUN = mean)
# Plot
figsgd31 <- ggplot(df_summary, aes(x = time, y = beta1, color = method, linetype = method)) +
  geom_line(linewidth = 1) +
  labs(
    x = "Time",
    y = TeX(r"($\|\hat{\beta}_{1,t}-\beta_1\|^2$)")
  ) +
  scale_color_manual(
    name = "method", 
    labels = c(TeX(r"($\gamma=1/3$)"), TeX(r"($\gamma=1/2$)"), TeX(r"($\epsilon_t=0$     )")),
    values = c("#386cb0", "#fdb462", "#CF5640")
  ) +
  scale_linetype_manual(
    name = "method", 
    labels = c(TeX(r"($\gamma=1/3$)"), TeX(r"($\gamma=1/2$)"), TeX(r"($\epsilon_t=0$     )")),
    values = c("solid", "dashed", "dotted") # Adjust linetypes 
  ) +
  theme_minimal()
figsgd31 
#ggsave(here::here("Simulation", "Simulation-result", "figsgd31.pdf"), plot = figsgd31, units = "in", width = 5.82, height = 3.61)

# beta0
df_summary <- aggregate(beta0 ~ method + time, data = df_all, FUN = mean)
# Plot
figsgd30 <- ggplot(df_summary, aes(x = time, y = beta0, color = method, linetype = method)) +
  geom_line(linewidth = 1) +
  labs(
    x = "Time",
    y = TeX(r"($\|\hat{\beta}_{0,t}-\beta_0\|^2$)")
  ) +
  scale_color_manual(
    name = "method", 
    labels = c(TeX(r"($\gamma=1/3$)"), TeX(r"($\gamma=1/2$)"), TeX(r"($\epsilon_t=0$     )")),
    values = c("#386cb0", "#fdb462", "#CF5640")
  ) +
  scale_linetype_manual(
    name = "method", 
    labels = c(TeX(r"($\gamma=1/3$)"), TeX(r"($\gamma=1/2$)"), TeX(r"($\epsilon_t=0$     )")),
    values = c("solid", "dashed", "dotted") # Adjust linetypes
  ) +
  theme_minimal()
figsgd30 
#ggsave(here::here("Simulation", "Simulation-result", "figsgd30.pdf"), plot = figsgd30, units = "in", width = 5.82, height = 3.61)


totalsgd <- ggarrange(figsgd11, figsgd21, figsgd31, figsgd10, figsgd20, figsgd30,
          ncol = 3, nrow = 2,  align = "hv", 
          common.legend = TRUE)
totalsgd
ggsave(here::here("Simulation", "Simulation-result", "totalsgd.pdf"),
       plot = totalsgd, device = grDevices::cairo_pdf,
       units = "in", width = 9.51, height = 5.28)


