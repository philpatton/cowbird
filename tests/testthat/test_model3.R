library(cowbird)

# Load Data

data("cowbird_data")

data_list <- make_data_list(cowbird_data)

# parameters for model 1
params <- c(
    get_table3_parameters(),
    get_evaluation_parameters(checking_parameters = T),
    get_predicition_parameters()
)

# Fit Model

model_fit <- fit_model('model_3', data_list, params)

# Check and evaluate model

ppc <- check_model(model_fit, data_list)

eval <- evaluate_model(model_fit, data_list)

# Plot models

# p1 <- plot_cooccur_probs(model_fit)

p2 <- plot_sites_with_both(model_fit)

p3 <- plot_just_hosts(model_fit)

# Make Table

table_parameters <- get_table3_parameters()
tab3 <- make_table_three(model_fit)

