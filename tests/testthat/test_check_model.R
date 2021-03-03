library(cowbird)

data("cowbird_data")

data_list <- make_data_list(cowbird_data)

params <- get_checking_parameters()

model_fit <- fit_model('model_1', data_list, params)

actual_pvals <- check_model(model_fit, data_list)

expected_pvals <- c(0.1, 0.8, 0.8, 0.7)

# browser()

# test_that('model checks', {expect_equal(actual_pvals, expected_pvals)})
