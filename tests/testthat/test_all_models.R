library(cowbird)

# Load Data

data("cowbird_data")

data_list <- make_data_list(cowbird_data)

# parameters for model 1
params <- get_evaluation_parameters(checking_parameters = T)

# Fit Model

fit_list <- fit_all_models(data_list, params)

# Check and evaluate model

ppc_list <- check_all_models(fit_list, data_list, just_pvals = F)

eval <- evaluate_all_models(fit_list, data_list)

# Plot models

p1 <- plot_all_ppc(ppc_list)

# Make Table

