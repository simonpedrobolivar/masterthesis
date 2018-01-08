###################################################### #
#### test functions ####################################
###################################################### #

# create test dataset (timeseries of matrices) -------------
years_obs_test <- 2000:2009
years_est_test <- c(2015, 2020)
test_mat_list <- list()
for(i in 1:length(years_obs_test)){
  test_mat_list[[i]] <- as.data.table(matrix(rnorm(25, mean = i, sd =1), nrow = 5, ncol = 5))
  #test_mat_list[[i]][,1] <- 0
}


lapply(test_mat_list, FUN = function(x) setnames(x, names(A_list_AUS[[1]])[1:5]))
lapply(test_mat_list, FUN = function(x) setkey(x, names(A_list_AUS[[1]])[1:5]))

# create 2nd test dataset (subset of WIOD)


# 1. test extrapolate_matrix -----------------------

extrapolate_matrix(mat.list = test_mat_list, years.obs = years_obs_test, years.est = years_est_test, parallel = F)

# 2. test extrapolate_A_mat ------------------------------

test <- extrapolate_A_mat(mat.list = test_mat_list, years.obs = years_obs_test, years.est = years_est_test, parallel = F)

# 3. test validate_forecast
system.time(
test <- validate_forecast(mat.list = test_mat_list, years.obs =  years_obs_test, year.split = 2005, type = "glm2", parallel = T)
)
plot(test[Year_forecast - Year_split == 1]$est,  test[Year_forecast - Year_split == 1]$obs, ylim = c(4,13), xlim = c(4,13), col = test[Year_forecast - Year_split == 1]$.id)
lines(0:12, 0:12)
cor(test[Year_forecast - Year_split == 1]$est,  test[Year_forecast - Year_split == 1]$obs)^2

## 3b. scale = T
test <- validate_forecast(mat.list = test_mat_list, years.obs =  years_obs_test, year.split = 2005, type = "glm", parallel = T, scale = T)
plot(test[Year_forecast - Year_split == 2]$est,  test[Year_forecast - Year_split == 2]$obs, ylim = c(0,2), xlim = c(0,2), col = test[Year_forecast - Year_split == 1]$.id)
lines(0:12, 0:12)
cor(test[Year_forecast - Year_split == 1]$est,  test[Year_forecast - Year_split == 1]$obs)^2




test <- create_timeseries(test_mat_list)
a <- as.data.table(t(apply(test, 1, scale)))
test[,1] <- NaN
test[,lapply(.SD, function(x) ifelse(is.nan(x), 0, x))]

a[1,]
testtrans <- transpose(transpose(test)[ , lapply(.SD, scale)]) 
scaled[,1] - scale(as.numeric(test[1,])) 

