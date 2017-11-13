#' creates a data.table which contains the timeseries of each coefficient of a timeseries of matrices
#'
#' @param mat.list a list containing a timeseries of matrices. Each element of the list represents on matrix of year t. 
#' @export

create_timeseries <- function(mat.list){
  require(data.table)
  ts <- as.data.table(sapply(mat.list, 
              FUN = function(x) return(apply(x, c(2,1), # c(2,1) damit reihenfolge erst zeile, dann spalte, wie in for-schleifen oben
                                              FUN = function(x) return(x)))))
  return(ts)
}




