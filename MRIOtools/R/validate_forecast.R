#' .
#'
#' @param mat_list a list containing a timeseries of matrices. Each element of the list represents on matrix of year t.
#' @param years.obs years for which observations exist. Needs to be equal to number of columns of ts.dt
#' @param years.split first year, where time-series should be splitted to training and test data set. year.split is the last year of the first training data set. 
#' @param scale should each timeseries be scaled??
#' @param parallel boolean. calculating parallelly on multiple cores?
#' @param n.cores number of cores. Default n.cores = 1.
#' @export

validate_forecast <- function(mat.list, years.obs, year.split, parallel = F, n.cores = 1,
                              type = "glm", scale = F, progress.bar = F){
  n.col <- ncol(mat.list[[1]])
  n.row <- nrow(mat.list[[1]])
  # create timeseries
  ts <- create_timeseries(mat.list)
  if(scale){ # scale each timeseries (center to 0, scaled by root-mean-squatered). It is necessary to transpose ts first, then scale and then transpose again, because data.tables can (as far as I know) only apply functions column-wise. Time series with only Zeros (0) are scaled to NaN and thus need to be set to 0 again. 
    ts <- (transpose(transpose(ts)[ , lapply(.SD, scale)]))[,lapply(.SD, function(x) ifelse(is.nan(x), 0, x))]
  }
  if(!progress.bar){
    if(parallel == T){ # parallele compution on multiple cores
      # Initiate cluster
      cl <- parallel::makeCluster(n.cores, type = "FORK")
      preds_dt <- rbindlist(parLapply(cl, 1:nrow(ts), function(x) cv(as.numeric(ts[x,]), years.obs = years.obs, year.split = year.split, type = type)), idcol = T)
      stopCluster(cl)
    }else{ # computation only on one single core
      preds_dt <-  rbindlist(lapply(1:nrow(ts), function(x) cv(as.numeric(ts[x,]), years.obs = years.obs, year.split = year.split, type = type)), idcol = T)
    }
  }else{ # show progress bar
    if(parallel == T){ # parallele compution on multiple cores
      # Initiate cluster
      cl <- parallel::makeCluster(n.cores, type = "FORK")
      preds_dt <- rbindlist(pblapply(1:nrow(ts), function(x) cv(as.numeric(ts[x,]), years.obs = years.obs, year.split = year.split, type = type), cl = cl), idcol = T)
      stopCluster(cl)
    }else{ # computation only on one single core
      preds_dt <-  rbindlist(pblapply(1:nrow(ts), function(x) cv(as.numeric(ts[x,]), years.obs = years.obs, year.split = year.split, type = type)), idcol = T)
    }
  }
  
  return(preds_dt)
}






