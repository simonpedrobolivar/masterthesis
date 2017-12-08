#' aggregates data.tables with two columns: 1st col Country/Industry whatever, 2nd col "V1" with values. 
#' First, the data.table is sorted based on the values of column "V1". Then, all but n rows are aggregated to one sector "Rest". 
#'
#' @param dt data.table with two columns: 1st col Country/Industry whatever, 2nd col "V1" with values
#' @param n number of sectors/countries/etc. that stay
#' @return a aggregated dtrix
#' @export


aggregate_datatable <- function(dt,
                                n, # number of sectors/countries/etc. that stay
                                col # name of column used for aggregating
){
  # aggregates a dtrix
  # first n sectors/countries/etc. stay, all others are subsumed into one sector "Other"
  setorderv(dt, cols = col, order = -1)
  dt_agg <- dt[1:n,]
  tempdt <- data.table("Rest", dt[(n+1):nrow(dt), sum(V1)])
  setnames(tempdt, names(dt))
  dt_agg <- rbind(dt_agg, tempdt)
  return(dt_agg)
}






