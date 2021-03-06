#' colwise cumulate values of a matrix (required for stacked_area_plot)
#'
#' @param mat matrix
#' @param order boolean
#' @return a cumulated matrix
#' @export

cumulate_matrix <- function(mat, order = T){
  # colwise cumulate values of a matrix (required for stacked_area_plot)
  if(order == T){
    if(length(which(colnames(mat) == "Other")) == 1){ # sector "Other" as first col --> better for plotting
      new_mat <- mat[,-which(colnames(mat) == "Other")]
      new_mat <- new_mat[,order(colMeans(new_mat))]
      new_mat <- cbind("Other" = mat[,which(colnames(mat) == "Other")], new_mat)
    }else new_mat <- mat[,order(colMeans(mat))]
  }else{new_mat <- mat}
  for(i in 2:ncol(new_mat)){
    new_mat[,i] <- new_mat[,i-1] + new_mat[,i]
  }
  return(new_mat)
}


