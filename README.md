# cowbird
Functions to reproduce analysis in Patton et al (TBD)

This package contains the data and functions to recreate the analysis in Patton et al. (TBD).

# Model fitting.

```
# load the cowbird functions
library(cowbird)

# fetch the cowbird / host community data
data("cowbird_data")

# convert it to a list of jags
data_list <- make_data_list(cowbird_data)

# parameters to monitor
params <- get_all_parameters()

# fit models 1, 2, and 3 (this takes 15-25 min on my machine) 
# (TODO) warnings because parameters do not apply to all models 
fit_list <- fit_all_models(data_list,  params, mcmc_params_paper = TRUE)
```

# MCMC diagnostics.

```
# Model 1 Diagnostics

# fetch the estimates from model 1
fit1 <- fit_list$fit1

# convert the output to coda class
diag_samples <- convert_mcmc(fit1)

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

# Model 2 Diagnostics

fit2 <- fit_list$fit2

# also want to check for convergence of host effect on cowbird
params <- c(get_hyper_parameters(), 'kappa')

diag_samples <- convert_mcmc(fit2, params)

plot(diag_samples$a, density = F)
plot(diag_samples$b, density = F)
plot(diag_samples$mu_c, density = F)
plot(diag_samples$sd_c, density = F)
plot(diag_samples$mu_d, density = F)
plot(diag_samples$sd_d, density = F)
plot(diag_samples$kappa, density = T)

ess2 <- effective_sample_size(diag_samples)
gr2 <- gelman_rubin(diag_samples)

# Model 3 Diagnostics

fit3 <- fit_list$fit3

# also want to check for convergence of host effect on cowbird
params <- c(get_hyper_parameters(), 'e', 'rho')

diag_samples <- convert_mcmc(fit3, params)

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

# Model checking and predictive performance.

```

# get the data for posterior checks
dl <- make_data_list(cowbird_data)

# Posterior predictive checks (this takes a while)

# posterior predictive check of all the models 
ppc_res <- check_all_models(fit_list, dl, just_pvals = F)

# table of pvalues
ppc_table(ppc_res)

# plot the results
ppc <- plot_ppc(ppc_res)

# Estimate WAIC (this takes a while)

waic_res <- evaluate_all_models(fit_list, dl)
waic_res
```

# Parameter estimates and predictions.

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
figure_three <- plot_cooccur_probs(fit)
plot(figure_three)

# figure four
figure_four <- plot_sites_with_both(fit)
plot(figure_four)

# figure five
figure_five <- plot_just_host(fit)
plot(figure_five)

```
