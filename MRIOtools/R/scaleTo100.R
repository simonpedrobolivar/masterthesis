#' scales matrix to 100.
#'
#' @param mat matrix
#' @param baseyear numeric, year that should be 100
#' @return a matrix
#' @export

scaleTo100 <- function(mat, baseyear){
  mat100 <- t(apply(as.matrix(mat), 1, function(x){100 * (x/as.matrix(mat)[which(rownames(mat) == as.character(baseyear)),])}))
  return(mat100)
}
