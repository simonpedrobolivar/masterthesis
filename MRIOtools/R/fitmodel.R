#' fits a model to a timeseries and extrapolates it into the future
#'
#' @param x a timeseries of length(years.obs) observations
#' @param years.obs years for which observations exist
#' @param years.est years for which estimations/extrapolations should be done
#' @export

fitmodel <- function(x, years.obs, years.est, type = "glm"){
  # x is a timeseries_obs
  
  if(type == "glm"){
    fm <- glm(x ~ years.obs) # fit model
    preds_output <- predict(fm, newdata = data.frame("years.obs" = years.est), se.fit = F) # predictions
    preds_output <- apply(as.matrix(preds_output), c(1,2),
                          FUN = function(x) ifelse(x < 0, return(0), return(x))) # all values < 0 are set to 0
  }
  if(type == "glm2"){
    fm <- glm(x ~ years.obs + I(years.obs^2)) # fit model
    preds_output <- predict(fm, newdata = data.frame("years.obs" = years.est), se.fit = F) # predictions
    preds_output <- apply(as.matrix(preds_output), c(1,2),
                          FUN = function(x) ifelse(x < 0, return(0), return(x))) # all values < 0 are set to 0
  }
  if(type == "glm3"){
    fm <- glm(x ~ years.obs + I(years.obs^2) + I(years.obs^3)) # fit model
    preds_output <- predict(fm, newdata = data.frame("years.obs" = years.est), se.fit = F) # predictions
    preds_output <- apply(as.matrix(preds_output), c(1,2),
                          FUN = function(x) ifelse(x < 0, return(0), return(x))) # all values < 0 are set to 0
  }
  
  if(type == "gam"){
    fm <- gam(x ~ s(years.obs))
  }
  

  return(preds_output)
}




