library(ggplot2)
library(latex2exp)
library(caret)
library(glmnet)
library(patchwork)
library(ggpubr)


### optimal policy value Figure 3 ###

options(warn=-1)


load(here::here("Simulation", "Simulation-result", "valueresult1.RData"))
load(here::here("Simulation", "Simulation-result", "valueresult2.RData"))
load(here::here("Simulation", "Simulation-result", "valueresult3.RData"))
Time <- 10000
Iteration <- 100


make_plot <- function(valueresult, Time, Iteration) {
  
  Converge <- data.frame(Time = rep(1:Time, 2))
  Converge$method <- c(rep("Online", Time), rep("Oracle", Time))
  Converge$value <- Converge$low <- Converge$high <- 0
  
  for (i in 1:Time) {
    idx <- c(0:(Iteration - 1)) * Time + i
    Converge$value[i] <- mean(valueresult[idx, 2])
    Converge$low[i]   <- mean(valueresult[idx, 1])
    Converge$high[i]  <- mean(valueresult[idx, 3])
    Converge$value[Time + i] <- mean(valueresult[idx, 4])
  }
  
  ggplot(Converge, aes(x = Time, y = value, color = method)) +
    geom_ribbon(
      data = subset(Converge, method == "Online"),
      aes(x = Time, ymin = low, ymax = high),
      fill = "#386cb0",
      alpha = 0.3,
      inherit.aes = FALSE
    ) +
    geom_line(linewidth = 0.6) +
    scale_color_manual(values = c("Online" = "#386cb0", "Oracle" = "#fdb462")) +
    theme_classic() +
    coord_cartesian(ylim = c(-0.5, 0.5)) +
    ylab("")
}

figvalue1 <- make_plot(valueresult1, Time, Iteration)
figvalue2 <- make_plot(valueresult2, Time, Iteration)
figvalue3 <- make_plot(valueresult3, Time, Iteration)


totalvalue <- ggarrange(
  figvalue1, figvalue2, figvalue3,
  ncol = 3, nrow = 1,
  align = "hv",
  common.legend = TRUE,
  legend = "top"
)


ggsave(here::here("Simulation", "Simulation-result", "totalvalue.pdf"),
       plot = totalvalue, device = grDevices::cairo_pdf,
       units = "in", width = 11, height = 3.81)


