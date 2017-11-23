#' stacked area plot.
#'
#' @param mat matrix with Years as rownames and sectors/countries(/...) as colnames --> output of function cumulate_matrix with order = T
#' @param agg aggregated? calls aggregate_matrix function. Boolean
#' @param agg.level aggregation level. Required if agg = T.
#' @param colorvec vector with colorcodes (length(colorvec) = ncol(mat))
#' @param xaxt.steps  steps for drawing tick marks at x-axis
#' @param yaxt.steps  steps for drawing tick marks at x-axis
#' @param legend boolean,  draw a legend??
#' @param legend.pos coordinates (c(x,y) or position (e.g."bottomleft") of the legend
#' @return a plot
#' @export


stacked_area_plot <-function(dt, # matrix with Years as rownames and sectors/countries(/...) as colnames --> output of function cumulate_matrix with order = T
                             agg = F,
                             agg.level,
                             colorvec = 1:100, # vector with colorcodes (length(colorvec) = ncol(mat))
                             xaxt.steps, # steps for drawing tick marks at x-axis
                             yaxt.steps, # steps for drawing tick marks at x-axis
                             ylim.adjust = 0, # adjust the upper limit of y-axis
                             legend = T, # draw a legend??
                             legend.pos = "topleft", # coordinates (c(x,y) or position (e.g."bottomleft") of the legend, see ?legend)
                             legend.ncol = 1, 
                             ... # additional arguments for plot(), see ?plot
){
  names   <- names(dt)
  if(names[1] != "Year" & names[1] != "year") stop("First column of data table needs to contain the Years")
  Years   <- unique(dt[[names[1]]])
  sectors <- unique(dt[[names[2]]])
  mat <- matrix(dt[[names[3]]], ncol = length(sectors), nrow = length(Years), byrow = T)
  print(dim(mat))
  colnames(mat) <- sectors
  rownames(mat) <- Years
  if(agg){
    if(is.null(agg.level)) stop("agg.level must be specified!")
    mat <- aggregate_matrix(mat, agg.level)
  }
  mat <- cumulate_matrix(mat)
  xx <- c(Years, rev(Years)) # create x-values of the polygons (same for all)
  yy_mat <- matrix(0, nrow = ncol(mat), ncol = 2 * nrow(mat)) # matrix to store y-values of each polygon
  for(i in 1:ncol(mat)){
    # fill y-values
    yy_mat[i,] <- c(rep(0, nrow(mat)), rev(mat[,ncol(mat) + 1 - i]))
  }
  #if(plot.ylim == NULL) plot.ylim <- 1.1*c(0, max(mat[,ncol(mat)]))
  plot(x=Years, y=mat[,ncol(mat)], col=colorvec[1], type='l', ylim=1.1*c(0, max(mat[,ncol(mat)])+ ylim.adjust) ,
       bty= "n", xaxt = "n", yaxt = "n", ...)
  axis(1, at = seq(min(Years), max(Years), xaxt.steps), lwd.ticks = 0.5, lwd = 0.5)
  axis(2, at = seq(0, signif(max(mat[,ncol(mat)]), 2), yaxt.steps), las = 2, lwd.ticks = 0.5, lwd = 0.5)
  abline(v = seq(min(Years), max(Years), xaxt.steps), col = "grey30", lty = 2, lwd = 0.5)
  abline(h = seq(0, signif(max(mat[,ncol(mat)]), 2), yaxt.steps), col = "grey30", lty = 2, lwd = 0.5)
  for(i in 1:ncol(mat)){
    polygon(xx, yy_mat[i,], col=colorvec[i], border=colorvec[i])
  }
  # drawing a legend if required
  if(legend == T){
    if(is.character(legend.pos)) legend(legend.pos, legend = rev(colnames(mat)),
                                        fill = colorvec,
                                        #bty = "n", 
                                        box.col = "white", bg = "white",
                                        border = colorvec,
                                        ncol = legend.ncol 
                                        #x.intersp = 0.5, 
                                        #text.width = 4
                                        )
    else legend(legend.pos[1], legend.pos[2], legend = rev(colnames(mat)),
                fill = colorvec,
                bty = "n", #box.col = "white",bg = "white"
                border = colorvec
                )
  }
}
