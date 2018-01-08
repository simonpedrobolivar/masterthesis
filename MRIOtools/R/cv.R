#' makes a cross-validation of a time-series. The time-series is split at a given year into a training data set and a test data set. A model is fitted to the training data and forecasts are made to the remaining years. These forecasts are compared to the "real" values. 
#'
#' @param x a timeseries of length(years.obs) observations
#' @param years.obs years for which observations exist
#' @param years.split first year, where time-series should be splitted to training and test data set. year.split is the last year of the first training data set. 
#' @export

cv <- function(x, years.obs, year.split, type = "glm"){
  # x is a timeseries_obs
  preds_dt <- as.data.table(matrix(ncol = 4, nrow = 0)) # ncol = 4 still hard coded. Change if wanna apply to other matrices than Y
  setnames(preds_dt, c("Year_split", "Year_forecast", "est", "obs"))
  n_years <- length(years.obs)
  n_years2model <- years.obs[length(years.obs)] - year.split
  for(i in 1:n_years2model){
    ts_test <- x[1:(n_years - n_years2model - 1 + i)] # test data
    years_test <- years_obs[1:(n_years - n_years2model - 1 + i)] # test years
    # fit model
    if(type == "glm") fm <- glm(ts_test ~ years_test)
    if(type == "glm2") fm <- glm(ts_test ~ years_test + I(years_test^2))
    if(type == "glm3") fm <- glm(ts_test ~ years_test + I(years_test^2) + I(years_test^3))
    if(type == "gam1") fm <- gamm(ts_test ~ s(years_test, bs = "cr"),
                                  correlation = corARMA(form = ~ 1|years_test, p = 1, q = 1))
    preds <- predict(fm, newdata = data.frame("years_test" = years_obs[(n_years - n_years2model + i):n_years])) # predict to remaining years
    preds_dt <- rbindlist(list(preds_dt, list(rep(year.split + i - 1, n_years2model + 1 - i), (year.split + i):years.obs[length(years.obs)], preds, x[(n_years - n_years2model + i):n_years])))
    
  }
  
  return(preds_dt)
}
