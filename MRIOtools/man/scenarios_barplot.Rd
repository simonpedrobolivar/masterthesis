% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/scenarios_barplot.R
\name{scenarios_barplot}
\alias{scenarios_barplot}
\title{Barplots to compare scenarios according to Region/INdusstry etc..}
\usage{
scenarios_barplot(dt.list, sectors, scale = F, colorvec = 1:100, grid = F,
  yaxt.steps, ylim.adjust = 0, legend = T, legend.pos = "topright", ...)
}
\arguments{
\item{dt.list}{list with data.tables. Each element of the list represents one scenario. The data.tables need to have 2 columns: one with Country/Industry etc, one with the values. If legend = T the names of the list elements need to the scenario names}

\item{sectors}{A character vector with names of the Regions/Industries etc. that want to be plotted. They need to be present in the first column of each data.table}

\item{colorvec}{vector with colorcodes}

\item{yaxt.steps}{steps for drawing tick marks at x-axis}

\item{ylim.adjust}{adjusts the upper limit of the y-axis. Adjust if you want more space for the legend}

\item{legend}{boolean,  draw a legend??}

\item{legend.pos}{coordinates (c(x,y) or position (e.g."bottomleft") of the legend}
}
\value{
a plot
}
\description{
Barplots to compare scenarios according to Region/INdusstry etc..
}

