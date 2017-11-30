#' Returns a list containing the extrapolated matrices.
#'
#' @param col.list a list containing a timeseries of matrices. Each element of the list represents on matrix of year t.
#' @param years.obs years for which observations exist. Needs to be equal to number of columns of ts.dt
#' @param years.est years for which estimations/extrapolations should be done
#' @param parallel boolean. calculation parallel on multiple cores
#' @param n.cores number of cores. Default n.cores = 1.
#' @param save.output boolean. Shall the output (new matrices) be saved as .csv to directory dir (specified with param dir)?
#' @param dir directory to where the new matrices should be saved
#' @export

extrapolate_A_column <- function(col.list, years.obs, years.est, scaleTo1 = F,parallel = F, n.cores = 1,
                              save.output = F, dir){

  n.row <- nrow(col.list[[1]])
  if(scaleTo1) col.sum.orig <- 0
  # save sum of each column of the latest A-matrix (later used for rescaling the extrapolated matrices)
  else col.sum.orig <- as.numeric(sum(col.list[[length(col.list)]]))
  # create timeseries, each col represents one year
  ts <- create_timeseries(col.list)

  if(parallel == T){ # parallele compution on multiple cores
    # Initiate cluster
    cl <- parallel::makeCluster(n.cores, type = "FORK")
    ts.extrapolated <- as.data.table(t(parSapply(cl, 1:nrow(ts),
                                                 FUN = function(x) return(fitmodel(as.numeric(ts[x,]),
                                                                                   years.obs = years.obs,
                                                                                   years.est = years.est)))))
    stopCluster(cl)
  }else{ # computation only on one single core
    ts.extrapolated <- as.data.table(t(sapply(1:nrow(ts),
                                              FUN = function(x) return(fitmodel(as.numeric(ts[x,]),
                                                                                years.obs = years.obs,
                                                                                years.est = years.est)))))
  }
  # create new matrices
  vec2mat <- function(x){
    # converts vector into matrix (size: n.row X n.col)
    return(as.data.table(matrix(x, nrow = n.row,
                                ncol = 1,
                                byrow = T))) # row-wise
  }
  # apply vec2mat for all columns (=years) of ts.extrapolated
  mat.extrapolated.list <- apply(ts.extrapolated, 2, FUN = vec2mat)
  names(mat.extrapolated.list) <- years.est
  # scale matrices
  col.sums.new_list <- lapply(mat.extrapolated.list, sum)

  for(i in 1:length(mat.extrapolated.list)){
    #each element of column i is multiplicated with col.sum.orig(i)/col.sums.new(i)
    if(col.sums.new_list[[i]] > 0) mat.extrapolated.list[[i]] <- mat.extrapolated.list[[i]] * (col.sum.orig/col.sums.new_list[[i]]) # to avoid division by zero
  }
  if(save.output == T){
    # TODO
  }
  return(mat.extrapolated.list)
}


