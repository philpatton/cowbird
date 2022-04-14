# cowbird
This package contains the data and functions to recreate the analysis in "Modeling and estimating co--occurrence between the invasive Shiny Cowbird and its Puerto Rican hosts," by Patton, Pacifici, and Collazo (2022), at *Biological Invasions*.

Here, I present a workflow that roughly approximates our workflow in analyzing these data, ultimately reproducing the results described in the paper, including the tables and figures. The workflow starts with training the models, checking that the MCMC algorithms converged, posterior predictitve checking, model selection, then finally inference and predicition from the chosen model. 

Please note that the package requires the installation of [JAGS](https://mcmc-jags.sourceforge.io/).

(Note: If you have [devtools](https://devtools.r-lib.org/) installed, you can install this package using `devtools::install_github('philpatton/cowbird')`)

# Model fitting.

Begin by loading the data from the data from the package then converting it to a list for JAGS.

```
library(cowbird)

# fetch the cowbird / host community data
data("cowbird_data")

# convert it to a list (jags likes lists) 
data_list <- make_data_list(cowbird_data)

```

Then proceed to training the models. Here, we fit all the models at once using `fit_all_models`. (It's possible to fit individual models with `fit_model`.) The 
`mcmc_params_paper` argument tells the function to iterate the MCMC algorithm long enough that we can reasonably check for convergence later. For quick model fitting, set this argument to `FALSE`. Alternatively, set each MCMC parameter, e.g., `n.burn` or `n.chains`, to your liking. 

```
# a vector of parameters to be monitored in the MCMC fitting
params <- get_all_parameters()

# fit models 1, 2, and 3 (this takes 15-25 min on my machine)
fit_list <- fit_all_models(data_list,  params, mcmc_params_paper = TRUE)
```

In my prefered workflow, I save the `fit_list` locally and proceed from here to prevent disaster if my machine crashes. 

(NOTE: There is a bug here whereby the fit_all_models function causes warnings about not being able to monitor parameters. Don't worry; the rest of the code works despite the warning.)

# MCMC diagnostics.

In this section, we check to see that the algorithm converged.

## Model 1 Diagnostics

```
# fetch the estimates from model 1
fit1 <- fit_list$fit1

# specify subset parameters for roughly diagnosing convergence 
params <- get_hyper_parameters()

# convert the output to coda class
samps <- extract_coda_samples(fit1, params)

# traceplots for high level parameters
plot(samps$a, density = F)
plot(samps$b, density = F)
plot(samps$mu_c, density = F)
plot(samps$sd_c, density = F)
plot(samps$mu_d, density = F)
plot(samps$sd_d, density = F)

# effective sample size and gelman-rubin statistic
ess1 <- effective_sample_size(samps)
gr1 <- gelman_rubin(samps)
```

## Model 2 Diagnostics

```
fit2 <- fit_list$fit2

# also want to check for convergence of host effect on cowbird
params <- c(get_hyper_parameters(), 'kappa')

samps <- extract_coda_samples(fit2, params)

plot(samps$a, density = F)
plot(samps$b, density = F)
plot(samps$mu_c, density = F)
plot(samps$sd_c, density = F)
plot(samps$mu_d, density = F)
plot(samps$sd_d, density = F)
plot(samps$kappa, density = T)

ess2 <- effective_sample_size(samps)
gr2 <- gelman_rubin(samps)
```

## Model 3 Diagnostics

```
fit3 <- fit_list$fit3

# also want to check for convergence of host effect on cowbird
params <- c(get_hyper_parameters(), 'e', 'rho')

samps <- extract_coda_samples(fit3, params)

plot(samps$a, density = F)
plot(samps$b, density = F)
plot(samps$mu_c, density = F)
plot(samps$sd_c, density = F)
plot(samps$mu_d, density = F)
plot(samps$sd_d, density = F)
plot(samps$rho, density = F)
plot(samps$e, density = F)

ess3 <- effective_sample_size(samps)
gr3 <- gelman_rubin(samps)
```

# Model checking and selection.

Here, we perform posterior predictive checks then choose a model with WAIC. Computing both the Bayesian P-values and WAIC takes a while.

```
# posterior predictive check of every model (slow)
ppc_res <- check_all_models(fit_list, dl, just_pvals = F)

# table of pvalues
ppc_table(ppc_res)

# plot the results (figure two) 
ppc <- make_figure_two(ppc_res)
plot(ppc)

# Estimate WAIC (slow)
waic_res <- evaluate_all_models(fit_list, dl)

# table four 
waic_res
```

# Inference and predictions.

Using the model of best fit, we generate the figures and tables from the paper. 

```
# table one
table_one <- make_table_one(fit_list$fit1)
table_one 

# table two
table_two <- make_table_two(fit_list$fit2)
table_two

# table three
table_three <- make_table_three(fit_list$fit3)
table_three

# Prediction Plots

fit <- fit_list$fit1

# figure three
figure_three <- make_figure_three(fit)
plot(figure_three)

# figure four
figure_four <- make_figure_four(fit)
plot(figure_four)

# figure five
figure_five <- make_figure_five(fit)
plot(figure_five)
```
