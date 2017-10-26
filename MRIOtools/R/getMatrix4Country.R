#' function to get detailed matrix for ONE country.
#'
#' @param mat matrix
#' @param country.name character, name of the country
#' @param time.vec vector
#' @return a matrix
#' @export

getMatrix4Country <- function(mat, country.name, time.vec){
  #function to get detailed matrix for ONE country
  mat_country <- as.data.frame(matrix(nrow = length(time.vec), ncol = nrow(mat[[1]])))
  colnames(mat_country) <- rownames(mat[[1]])
  rownames(mat_country) <- time.vec
  for(i in 1:length(time.vec)){
    mat_country[i,] <- mat[[i]][,which(colnames(mat[[1]]) == as.character(country.name))]
  }
  return(mat_country)
}
