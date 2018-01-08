#' ?
#'
#' @param x a data.table with timeseries of all coefficients of a matrix. (Rowwise, i.e. 1st element if (1,1), 2nd is (1,2), etc. )
#' @param n.row number of rows of the output matrix
#' @param n.col number of cols of the output matrix
#' @export



vec2mat <- function(x, n.row, n.col){
    # converts vector into matrix (size: n.row X n.col)
    return(as.data.table(matrix(x, nrow = n.row,
                                ncol = n.col,
                                byrow = T))) # row-wise
}

