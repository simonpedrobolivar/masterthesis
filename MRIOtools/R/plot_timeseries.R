#' plots the time series of one cell/coefficient of a list that contains time series of matrices (such as SUT, Y, etc.).
#'
#' @param mat_list a list containing a timeseries of matrices/2D-df/dt. Each element of the list represents on matrix of year t.
#' @param row.index the row index of the cell
#' @param col.index the col.index of the cell
#' @param years A vector with the years for which data exists. Needs to be equal to the length of @param mat_list
#' @param ... further arguments for plot(). See also ?plot
#' @export

plot_timeseries <- function(mat.list, row.index, col.index, years.obs, years.est = F, ...){
  ts <- as.numeric(sapply(mat.list, FUN = "[", row.index, col.index))
  if(!years.est){
    years <- years.obs # if only for observed years
  }else{
    years <- c(years.obs, years.est)
  }
  plot(years, ts, xlab = "Years", type = "l", ...)
  #lines(years.est, ts[length(years.obs) + 1], col = "red")
  # TODO: auch für extrapolated values anwendbar machen: years.obs, years.est
}


