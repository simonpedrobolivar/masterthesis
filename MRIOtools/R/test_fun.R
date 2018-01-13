#' function for reproducing error message: "Fehler in .subset(x, j) : ungültiger Indextyp 'list'"
#'
#' @export

test_fun <- function(ts) {
  return(transpose(transpose(ts)[ , lapply(.SD, scale)]))[,lapply(.SD, function(x) ifelse(is.nan(x), 0, x))]
}








