#' makes a cross-validation of a time-series. The time-series is split at a given year into a training data set and a train data set. A model is fitted to the training data and forecasts are made to the remaining years. These forecasts are compared to the "real" values. 
#'
#' @param x a timeseries of length(years.obs) observations
#' @param years.obs years for which observations exist
#' @param years.split first year, where time-series should be splitted to training and train data set. year.split is the last year of the first training data set. 
#' @export

cv <- function(x, years.obs, year.split, type = "glm", ... ){
  # x is a timeseries.obs
  preds.dt <- as.data.table(matrix(ncol = 4, nrow = 0)) 
  setnames(preds.dt, c("Year_split", "Year_forecast", "est", "obs"))
  n.years <- length(years.obs)
  n.years2model <- years.obs[length(years.obs)] - year.split
  for(i in 1:n.years2model){
    ts.train <- x[1:(n.years - n.years2model - 1 + i)] # train data
    years.train <- years.obs[1:(n.years - n.years2model - 1 + i)] # train years
    # fit model
    if(type %in% c("glm", "glm2", "glm3")){
      if(type == "glm")  fm <- glm(ts.train ~ years.train)
      if(type == "glm2") fm <- glm(ts.train ~ years.train + I(years.train^2))
      if(type == "glm3") fm <- glm(ts.train ~ years.train + I(years.train^2) + I(years.train^3))
      preds <- predict(fm, newdata = data.frame("years.train" = years.obs[(n.years - n.years2model + i):n.years]))
    }
    if(type == "gam1"){ 
      # TODO. not running yet
      fm    <- gamm(ts.train ~ s(years.train, bs = "cr"),
                 correlation = corARMA(form = ~ 1|years.train, p = 1, q = 1))
      preds <- as.numeric(predict(fm$gam, 
                                  newdata = data.frame("years.train" = years.obs[(n.years - n.years2model + i):n.years])))
    }
    if(type == "ets"){
      fm <- ets(ts(ts.train, 
                   start = years.train[1], 
                   end = years.train[length(years.train)]), ...)
      preds <- as.numeric(forecast(fm, 
                                   h = years.obs[n.years] - years.train[length(years.train)])$mean) # last year - last year of train data  
      print(paste("h=",years.obs[n.years] - years.train[length(years.train)]))
    }
    print(preds)
    preds.dt <- rbindlist(list(preds.dt, list(rep(year.split + i - 1, n.years2model + 1 - i), (year.split + i):years.obs[length(years.obs)], preds, x[(n.years - n.years2model + i):n.years])))
    
  }
  
  return(preds.dt)
}


