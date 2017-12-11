###################################################### #
#### test functions ####################################
###################################################### #

# create test dataset (timeseries of matrices) -------------
years_obs_test <- 2000:2009
years_est_test <- c(2015, 2020)
test_mat_list <- list()
for(i in 1:length(years_obs_test)){
  test_mat_list[[i]] <- as.data.table(matrix(rnorm(25, mean = i), nrow = 5, ncol = 5))
  test_mat_list[[i]][,1] <- 0
}


lapply(test_mat_list, FUN = function(x) setnames(x, names(A_list_AUS[[1]])[1:5]))
lapply(test_mat_list, FUN = function(x) setkey(x, names(A_list_AUS[[1]])[1:5]))

# create 2nd test dataset (subset of WIOD)


# 1. test extrapolate_matrix -----------------------


extrapolate_matrix(mat.list = test_mat_list, years.obs = years_obs_test, years.est = years_est_test, parallel = F)

# 2. test extrapolate_A_mat ------------------------------

test <- extrapolate_A_mat(mat.list = test_mat_list, years.obs = years_obs_test, years.est = years_est_test, parallel = T)



test
colSums(test[[1]]) - colSums(test_mat_list[[11]])


coltobind <- list()
for(i in 1:11) coltobind[[i]] <- (i:(i+5))

coltobind <- sapply(test_mat_list, "[",,1) 
lapply(test_mat_list, cbind, as.numeric(unlist(coltobind)))

mapply(cbind, test_mat_list, V6 = coltobind)
Map(cbind, test_mat_list, coltobind)



