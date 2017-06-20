scaleTo100 <- function(mat, baseyear){
  mat100 <- t(apply(as.matrix(mat), 1, function(x){100 * (x/as.matrix(mat)[which(rownames(mat) == as.character(baseyear)),])}))
  return(mat100)
}
