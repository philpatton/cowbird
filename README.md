# cowbird
This package contains the data and functions to recreate the analysis in Patton et al. (TBD).

Here, I present a workflow that roughly approximates our workflow in analyzing these data. The workflow ultimately reproduces the results described in the paper, including the tables and figures. The workflow starts with training the models, checking that the MCMC algorithms converged, posterior predictitve checking, model selection, then finally inference and predicition from the chosen model. 

# Model fitting.

Begin by loading the data from the data from the package then converting it to a list for JAGS.

```
library(cowbird)

# fetch the cowbird / host community data
data("cowbird_data")

# convert it to a list (jags likes lists) 
data_list <- make_data_list(cowbird_data)

```

Then proceed to training the models. Here, we fit all the models at once using `fit_all_models`. (It's possible to fit individual models with `fit_model`. The 
`mcmc_params_paper` argument tells the function to iterate the MCMC algorithm long enough that we can reasonably check for convergence later. 

```
# a vector of parameters to be monitored in the MCMC fitting
params <- get_all_parameters()

# fit models 1, 2, and 3 (this takes 15-25 min on my machine)
fit_list <- fit_all_models(data_list,  params, mcmc_params_paper = TRUE)
```

In my prefered workflow, I save the `fit_list` locally and proceed from here to prevent disaster if my machine crashes. 

# MCMC diagnostics.

In this section, we check to see that the algorithm converged.

## Model 1 Diagnostics

```
# fetch the estimates from model 1
fit1 <- fit_list$fit1

# convert the output to coda class
diag_samples <- extract_coda_samples(fit1)

# traceplots for high level parameters
plot(diag_samples$a, density = F)
plot(diag_samples$b, density = F)
plot(diag_samples$mu_c, density = F)
plot(diag_samples$sd_c, density = F)
plot(diag_samples$mu_d, density = F)
plot(diag_samples$sd_d, density = F)

# effective sample size and gelman-rubin statistic
ess1 <- effective_sample_size(diag_samples)
gr1 <- gelman_rubin(diag_samples)
```

## Model 2 Diagnostics

```
fit2 <- fit_list$fit2

# also want to check for convergence of host effect on cowbird
params <- c(get_hyper_parameters(), 'kappa')

diag_samples <- extract_coda_samples(fit2, params)

plot(diag_samples$a, density = F)
plot(diag_samples$b, density = F)
plot(diag_samples$mu_c, density = F)
plot(diag_samples$sd_c, density = F)
plot(diag_samples$mu_d, density = F)
plot(diag_samples$sd_d, density = F)
plot(diag_samples$kappa, density = T)

ess2 <- effective_sample_size(diag_samples)
gr2 <- gelman_rubin(diag_samples)
```

## Model 3 Diagnostics

```
fit3 <- fit_list$fit3

# also want to check for convergence of host effect on cowbird
params <- c(get_hyper_parameters(), 'e', 'rho')

diag_samples <- extract_coda_samples(fit3, params)

plot(diag_samples$a, density = F)
plot(diag_samples$b, density = F)
plot(diag_samples$mu_c, density = F)
plot(diag_samples$sd_c, density = F)
plot(diag_samples$mu_d, density = F)
plot(diag_samples$sd_d, density = F)
plot(diag_samples$rho, density = F)
plot(diag_samples$e, density = F)

ess3 <- effective_sample_size(diag_samples)
gr3 <- gelman_rubin(diag_samples)
```

# Model checking and selection.

Here, we perform posterior predictive checks then choose a model with WAIC. 

```
# get the data for posterior checks
dl <- make_data_list(cowbird_data)

# Posterior predictive checks (this takes a while)

# posterior predictive check of all the models 
ppc_res <- check_all_models(fit_list, dl, just_pvals = F)

# table of pvalues
ppc_table(ppc_res)

# plot the results (figure two) 
ppc <- make_figure_two(ppc_res)

# Estimate WAIC (this takes a while)

waic_res <- evaluate_all_models(fit_list, dl)
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
