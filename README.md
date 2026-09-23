# Highdim-Linear-Bandit-Inference

Code and data to reproduce simulation results, figures, and real data analysis results for the paper "Regret Minimization and Statistical Inference in Online Decision Making with High-dimensional Covariates"

## Running the code

Open `Highdim-Linear-Bandit-Inference.Rproj` before running the scripts. All input and output paths are constructed with `here::here()`, so no manual setting of the working directory is required. The R package `here` is therefore required in addition to the packages loaded by each script.

## Organization

### Simulation

Folder Simulation contains the code to reproduce simulation results and figures in Section 6.1.

Simulation-Online estimation.R examines the convergence performance of the online estimation procedure. Plot-Online estimation.R visualizes the results and produces Figure 1.

Simulation-Coefficient inference.R evaluates the performance of IPW and AW debiased estimator on parameter inference. Plot-Coefficient inference.R visualizes the results and produces Figure 2.

Simulation-Optimal policy value.R is the experiment on optimal policy value inference. Plot-Optimal policy value.R visualizes the results and produces Figure 3.

The subfolder Simulation-result contains all the simulation results. To be specific, 1) scenario1-sgd-result1.RData, scenario1-sgd-result2.RData, scenario1-sgd-result3.RData are the online estimation error under $\gamma=1/3$, $\gamma=1/2$, and $\varepsilon_t=0$, respectively, under scenario 1. Files with analogous names are defined similarly for the other scenarios. They are used to produce Figure 1. 2) scenario1-inf-result1.RData, scenario1-inf-result2.RData, scenario1-inf-result3.RData, scenario1-inf-result4.RData are the coefficient inference results under $\gamma=1/3$, $\gamma=1/2$, offline debiased Lasso, and $\varepsilon_t=0$, respectively, under scenario 1. Files with analogous names are defined similarly for the other scenarios. They are used to produce Figure 2. 3) valueresult1.RData, valueresult2.RData, valueresult3.RData are the optimal policy value inference results under scenario1, scenario2 and scenario3, respectively. They are used to produce Figure 3.

### Real data

Folder Real data contains the data and code to reproduce real data analysis results and figures in Section 6.2.

dataall.RData contains the Warfarin dosage data used for real data analysis.

Real data-Coefficient inference.R conducts linear coefficients inference using both IPW and AW debiased estimators, identifies the significant variables corresponding to three different medicine dosage. It produces the point estimators and 95% CI for $\beta_1$, $\beta_2$ and $\beta_0$ in Figure 4, and point estimators and 95% CI for $\beta_0-\beta_1$, $\beta_0-\beta_2$ and $\beta_1-\beta_2$ in Figure 5.

Real data-Optimal policy value.R performs optimal policy’s value inference, and compares the results with OLS oracle estimator. It produces the optimal value estimation and 95% CI in Figure 6.
