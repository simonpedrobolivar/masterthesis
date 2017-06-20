stacked_area_plot <-function(mat, # matrix with years as rownames and sectors/countries(/...) as colnames --> output of function cumulate_matrix with order = T
                             colorvec, # vector with colorcodes (length(colorvec) = ncol(mat))
                             xaxt.steps, # steps for drawing tick marks at x-axis
                             yaxt.steps, # steps for drawing tick marks at x-axis
                             legend = T, # draw a legend??
                             legend.pos = "topleft", # coordinates (c(x,y) or position (e.g."bottomleft") of the legend, see ?legend)
                             ... # additional arguments for plot(), see ?plot
){
  years <- rownames(mat)
  xx <- c(years, rev(years)) # create x-values of the polygons (same for all)
  yy_mat <- matrix(0, nrow = ncol(mat), ncol = 2 * nrow(mat)) # matrix to store y-values of each polygon
  for(i in 1:ncol(mat)){
    # fill y-values
    yy_mat[i,] <- c(rep(0, nrow(mat)), rev(mat[,ncol(mat) + 1 - i]))
  }
  plot(x=years, y=mat[,ncol(mat)], col=colorvec[1], type='l', ylim=1.1*c(0, max(mat[,ncol(mat)])),
       bty= "n", xaxt = "n", yaxt = "n", ...)
  axis(1, at = seq(min(years), max(years), xaxt.steps), lwd.ticks = 0.5, lwd = 0.5)
  axis(2, at = seq(0, signif(max(mat[,ncol(mat)]), 2), yaxt.steps), las = 2, lwd.ticks = 0.5, lwd = 0.5)
  for(i in 1:ncol(mat)){
    polygon(xx, yy_mat[i,], col=colorvec[i], border=colorvec[i])
  }
  # drawing a legend if required
  if(legend == T){
    if(is.character(legend.pos)) legend(legend.pos, legend = rev(colnames(mat)),
                                        fill = colorvec,
                                        bty = "n", border = colorvec)
    else legend(legend.pos[1], legend.pos[2], legend = rev(colnames(mat)),
                fill = colorvec,
                bty = "n", border = colorvec)
  }
}
