#' Returns a list containing the extrapolated matrices.
#'
#' @param mat.list a list containing a timeseries of matrices. Each element of the list represents on matrix of year t.
#' @param years.obs years for which observations exist. Needs to be equal to number of columns of ts.dt
#' @param years.est years for which estimations/extrapolations should be done
#' @param parallel boolean. calculation parallel on multiple cores
#' @param n.cores number of cores. Default n.cores = 1.
#' @param save.output boolean. Shall the output (new matrices) be saved as .csv to directory dir (specified with param dir)?
#' @param dir directory to where the new matrices should be saved
#' @export

extrapolate_A_mat <- function(mat.list, years.obs, years.est, scaleTo1 = F,parallel = F, n.cores = 1,
                               save.output = F, dir){
  n.col <- ncol(mat.list[[1]])
  n.row <- nrow(mat.list[[1]])
  if(scaleTo1) col.sums.orig <- vector(mode = "numeric", length = n.col)
  # save sum of each column of the latest A-matrix (later used for rescaling the extrapolated matrices)
  else col.sums.orig <- as.numeric(colSums(mat.list[[length(mat.list)]]))#col.sums.orig <- lapply(mat.list, colSums)
  # create timeseries
  ts <- create_timeseries(mat.list)

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
                                ncol = n.col,
                                byrow = T))) # row-wise
  }
  # apply vec2mat for all columns (=years) of ts.extrapolated
  mat.extrapolated.list <- apply(ts.extrapolated, 2, FUN = vec2mat)
  names(mat.extrapolated.list) <- years.est
  # scale matrices
  col.sums.new_list <- lapply(mat.extrapolated.list, colSums)
  #rescale <- function(x, col.sums.new){
    # x is one matrix A
    #
 #   p <- as.numeric(col.sums.orig/col.sums.new)
  #  return(as.matrix(x) %*% diag(p))
 # }
  #p <- as.numeric(col.sums.orig/col.sums.new_list[[1]])
  #colSums(as.matrix(mat.extrapolated.list[[1]]) %*% diag(p)) - col.sums.orig
  for(i in 1:length(mat.extrapolated.list)){
    #each element of column i is multiplicated with col.sums.orig(i)/col.sums.new(i)
    mat.extrapolated.list[[i]] <- as.data.table(as.matrix(mat.extrapolated.list[[i]]) %*% diag(as.numeric(col.sums.orig/col.sums.new_list[[i]])))
  }
  if(save.output == T){
    # TODO
  }
  return(mat.extrapolated.list)
}



