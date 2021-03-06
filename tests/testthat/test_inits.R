library(cowbird)

data("cowbird_data")

dl <- make_data_list(cowbird_data)

inits <- initialize_values(dl, fix_seed = T)

expect_equal(length(inits), 4)

inits <- initialize_values(dl, fix_seed = F)

expect_equal(length(inits), 2)
