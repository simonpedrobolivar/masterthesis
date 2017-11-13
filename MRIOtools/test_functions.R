# test funs
# create timeseries of matrices
years_obs_test <- 2000:2010
years_est_test <- c(2015, 2020)
test_mat_list <- list()
for(i in 1:length(years_obs_test)){
  test_mat_list[[i]] <- matrix(rnorm(25, mean = i), nrow = 5, ncol = 5)
}

extrapolate_matrix(mat.list = test_mat_list, years.obs = years_obs_test, years.est = years_est_test)
