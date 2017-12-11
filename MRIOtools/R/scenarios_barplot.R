#' Barplots to compare scenarios according to Region/INdusstry etc..
#'
#' @param dt.list list with data.tables. Each element of the list represents one scenario. The data.tables need to have 2 columns: one with Country/Industry etc, one with the values. If legend = T the names of the list elements need to the scenario names
#' @param colorvec vector with colorcodes 
#' @param yaxt.steps  steps for drawing tick marks at x-axis
#' @param sectors A character vector with names of the Regions/Industries etc. that want to be plotted. They need to be present in the first column of each data.table
#' @param legend boolean,  draw a legend??
#' @param legend.pos coordinates (c(x,y) or position (e.g."bottomleft") of the legend
#' @param ylim.adjust adjusts the upper limit of the y-axis. Adjust if you want more space for the legend
#' @return a plot
#' @export



scenarios_barplot <- function(dt.list, 
                              sectors,
                              scale = F,
                              colorvec = 1:100,
                              grid = F,
                              yaxt.steps,
                              ylim.adjust = 0, # adjust the upper limit of y-axis
                              legend = T, 
                              legend.pos = "topright", 
                              ...
){
  dt.list <- lapply(dt.list, setorder) # order data.tables alphabetically 
  for(i in 1:length(dt.list)){ # selecting countries/industries
    if(names(dt.list[[1]])[1] == "Country") dt.list[[i]] <- dt.list[[i]][Country %in% sectors]
    if(names(dt.list[[1]])[1] == "Industry") dt.list[[i]] <- dt.list[[i]][Industry %in% sectors]
  }
  if(scale){
    mat <- matrix(unlist(rbindlist(lapply(dt.list, "[", , 2))), nrow = length(sectors), ncol = length(dt.list))
    newmat <- mat
    for(i in 1:length(sectors)){
      if(max(mat[i,]) == 0) newmat[i,] <- mat[i,]
      else{
        newmat[i,which.max(mat[i,])] <- 100
        newmat[i,-which.max(mat[i,])] <- (100/mat[i,which.max(mat[i,])]) * mat[i,-which.max(mat[i,])]
      }
    }
    for(i in 1:length(dt.list)){
      dt.list[[i]][,2] <- newmat[,i]
    }
    ticks <- seq(0, 100, yaxt.steps)
  }
  
  x_values <- barplot(height = rep(0, length(sectors)), 
                      space = c(rep(length(dt.list), length(sectors)-1), length(dt.list) + 2), col = 0, ylim = c(0, max(unlist(lapply(dt.list, "[", ,2))) + ylim.adjust), 
                      las = 2, yaxt = "n", border = NA)
  if(!scale) ticks <- seq(0, signif(max(unlist(lapply(dt.list, "[", ,2))), 1), yaxt.steps) # set ticks for the y axis
  axis(2, at = ticks[c(T, F)], labels = ticks[c(T, F)], las = 2) # draw y axis
  if(grid) abline(h = ticks, col = "grey30", lty = 2, lwd = 0.5)
  
  for(i in 1:length(dt.list)){
    barplot(height = dt.list[[i]]$V1, las = 2, 
            space = c(length(dt.list)+i-1,rep(length(dt.list), length(sectors) - 1)), 
            col = colorvec[i], add = T, axes = F, ...)
    
    
  }
  x_values[length(x_values)] <- x_values[length(x_values)] - 2
  mtext(side = 1, text = unlist(dt.list[[1]][,1]), at = x_values+(length(dt.list)/2)-.5, las = 2, line = 1, cex = 0.7)
  if(legend) legend(legend.pos, legend = names(dt.list), fill = colorvec,box.col = "white", bg = "white", border = colorvec)
}

