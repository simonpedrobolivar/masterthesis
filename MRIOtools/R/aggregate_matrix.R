aggregate_matrix <- function(mat,
                             n, # number of sectors/countries/etc. that stay
                             fun = mean # determines how first n sectors/countries/etc are determined
){
  # aggregates a matrix
  # first n sectors/countries/etc. stay, all others are subsumed into one sector "Other"
  new_mat <- matrix(0, ncol = n+1, nrow = nrow(mat))
  names_agg <- names(sort(apply(mat, 2, fun), decreasing = T))[1:n]
  index_vec <- vector(mode = "numeric", length = n)
  for(i in 1:n){
    new_mat[,i] <- mat[,which(colnames(mat) == names_agg[i])]
    index_vec[i] <- which(colnames(mat) == names_agg[i])
  }
  new_mat[,n+1] <- rowSums(mat[,-index_vec] )
  rownames(new_mat) <- rownames(mat)
  colnames(new_mat) <- c(colnames(mat)[index_vec], "Other")
  return(new_mat)
}
