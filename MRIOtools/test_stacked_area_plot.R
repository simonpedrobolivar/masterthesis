colors <- RColorBrewer::brewer.pal(12, "Set3")[-2] # colors for plots
load("stacked_area_plot_example.RData")

stacked_area_plot(Y_by_sector_cumul, colorvec = colors[2:5], 10, 500)
