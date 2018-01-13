#' function for reproducing error message: "Fehler in .subset(x, j) : ungültiger Indextyp 'list'"
#'
#' @export

test_fun <- function(ts) {
  #return(ts[ , lapply(ts, scale)])
         #[,lapply(.SD, function(x) ifelse(is.nan(x), 0, x))])
  return(as.data.table(apply(ts, 2, scale)))
}








