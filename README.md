# Highdim-Linear-Bandit-Inference
Code and data to reproduce simulation results, figures, and real data analysis results for the paper "Regret Minimization and Statistical Inference in Online Decision
Making with High-dimensional Covariates"

## Organization

### Simulation
Folder Simulation contains the code to reproduce simulation results and figures in Section 6.1.

Simulation-Online estimation.R examines the convergence performance of the online estimation procedure. Plot-Online estimation.R visualizes the results and produces Figure 1.  

Simulation-Coefficient inference.R evaluates the performance of IPW and AW debiased estimator on parameter inference. Plot-Coefficient inference.R visualizes the results and produces Figure 2.

Simulation-Optimal policy value.R is the experiment on optimal policy value inference. Plot-Optimal policy value.R visualizes the results and produces Figure 3.

The subfolder Simulation-result contains all the simulation results. To be specific,
1) scenario1-sgd-result1.RData, scenario1-sgd-result2.RData, scenario1-sgd-result3.RData are the online estimation error under $\gamma=1/3$, $\gamma=1/2$, and $\varepsilon_t=0$, respectively, under scenario 1. Files with analogous names are defined similarly for the other scenarios. They are used to produce Figure 1. 
2) scenario1-inf-result1.RData,  scenario1-inf-result2.RData, scenario1-inf-result3.RData, scenario1-inf-result4.RData are the coefficient inference results under $\gamma=1/3$, $\gamma=1/2$, offline debiased Lasso, and $\varepsilon_t=0$, respectively, under scenario 1. Files with analogous names are defined similarly for the other scenarios. They are used to produce Figure 2.
3) valueresult1.RData, valueresult2.RData, valueresult3.RData are the optimal policy value inference results under scenario1, scenario2 and scenario3, respectively. They are used to produce Figure 3.


