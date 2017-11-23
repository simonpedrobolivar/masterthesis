###################################################### #
#### test functions ####################################
###################################################### #

# load packages ---------------------------------------

require(devtools)
install_github('simonpedrobolivar/masterthesis/MRIOtools')
require(MRIOtools)
require(gdata)
require(data.table) # for handling large data sets
require(parallel)
require(mgcv)


# set constant parameters --------------------------------
n_ind       <- 35 # number of industries
n_countries <- 41 # number of countries (40 + 1 RoW)
years_obs   <- 1995:2011 # years for which IO table exists
years_est <- c(2020, 2030) #c(2012:2014, seq(2015, 2045, 10)) # year for which to make prediction
years <- c(years_obs, years_est)
n_years_obs     <- length(years_obs)
n_years <- length(years)
n_sectors   <- 4 # final demand sectors (household, non-gov, gov, capital formation)
exchange_rates <- c(1.36626,	1.30024	,1.12945	,1.11308	,1.06580	,0.92360	,0.89560	,0.94560	,1.13120,	1.24390,	1.24410	,1.25560,	1.37050	,1.47080,	1.39480,	1.32570,	1.39200) # 1 € = ...$US (according to WIOD)

# read names of countries and industries:
config_sheet = read.xls("/home/simon/Dokumente/Master/3. FS/ResearchMethodsIndustrialEcology/myproject/Calculation/WIOD_Config.xlsx", sheet = 3, header = T)
config_sheet2 = read.xls("/home/simon/Dokumente/Master/3. FS/ResearchMethodsIndustrialEcology/myproject/Calculation/WIOD_Config.xlsx", sheet = 5, header = T)
countries <- as.character(config_sheet$Symbol)[1:41]
regions <- as.character(config_sheet$Region)[1:41]
industries <- as.character(config_sheet2$X.1)[-1]

industriesXcountries <- paste(rep(industries, n_countries),
                              as.vector(sapply(countries,
                                               FUN = function(x) return(rep(x, n_ind)), simplify = "vector")))
# create 2nd test dataset (subset of WIOD)
setwd("/home/simon/Dokumente/Master/3. FS/ResearchMethodsIndustrialEcology/myproject/data/WIOD/IO_tables")
A_list <- list() #
for(i in 1:n_years_obs){
  # A_mat <- as.data.frame(read.table(paste("A_matrix",1994+i,".csv",sep = ""),
  #                                   header = F, sep = " "))
  A_mat <- fread(paste("A_matrix",1994+i,".csv",sep = ""),
                 header = F, sep = " ")

  colnames(A_mat) <- rownames(A_mat) <- industriesXcountries
  A_list[[i]] <-  A_mat # append to list
  print(paste("year", i + 1994, "done!"))
}
names(A_list) <- years_obs
A_list[[1]][1:10, 1:10]
A_list_AUS <- lapply(A_list, FUN = function(x) return(x[1:35, 1:35]))

dim(A_list_AUS[[1]])
length(A_list_AUS)
rm(A_list)


###################################################### #
#### 1. extrapolate_matrix ####################################
###################################################### #
A_list_AUS_extrapolated <- extrapolate_matrix(mat.list = A_list_AUS,
                                              years.obs = years_obs,
                                              years.est = years_est,
                                              parallel = T,
                                              n.cores = 2)


lapply(A_list_AUS_extrapolated, FUN = function(x) setnames(x, names(A_list_AUS[[1]])))

#lapply(A_list_AUS_extrapolated, FUN = function(x) (rownames(x) <- names(A_list_AUS[[1]])))

row.names(A_list_AUS_extrapolated[[1]])

A_list_AUS_total <- c(A_list_AUS, A_list_AUS_extrapolated)
length(A_list_AUS_total)
A_list_AUS_total[[1]]
###################################################### #
#### 2. plot_timeseries ####################################
###################################################### #
plot_timeseries(A_list_AUS, 1, 1, years_obs, ylab = "bla")

ts <- as.numeric(sapply(A_list_AUS,"[", 17, 17))
plot(years_obs, ts, xlab = "Years", type = "l")

################################################### #
#### 3. fit gam ####################################
################################################# #

# 1. basic settings
fm1 <- gam(ts ~ s(years_obs))
plot(fm1, shade = T, xlim = c(min(years), max(years)))

# 2. changing the bs-argument
fm2cs <- gam(ts ~ s(years_obs, bs = "cs" ))
plot(fm2cs, shade = T, xlim = c(min(years), max(years)))

fm2cr <- gam(ts ~ s(years_obs, bs = "cr" ))
plot(fm2cr, shade = T, xlim = c(min(years), max(years)))

fm2tp <- gam(ts ~ s(years_obs, bs = "tp" )) # standart settings ( = fm1)
plot(fm2tp, shade = T, xlim = c(min(years), max(years)))

lines(years_obs, ts)
# 3. adding temporal autocorrelation term
fm3p1q1 <- gamm(ts ~ s(years_obs, bs = "cr"),
          correlation = corARMA(form = ~ 1|years_obs, p = 1, q = 1))
plot(fm3p1q1$gam, shade = T, xlim = c(min(years), max(years)))


fm3p2q1 <- gamm(ts ~ s(years_obs, bs = "cr"),
                correlation = corARMA(form = ~ 1|years_obs, p = 2, q = 1))
plot(fm3p2q1$gam, shade = T, xlim = c(min(years), max(years)))

fm3p2q2 <- gamm(ts ~ s(years_obs, bs = "cr"),
                correlation = corARMA(form = ~ 1|years_obs, p = 2, q = 2))
plot(fm3p2q2$gam, shade = T, xlim = c(min(years), max(years)))

# comparing the models
anova(fm3p1q1$lme, fm3p2q1$lme)

plot(years_obs, fm3p2q2$gam$fitted.values, type = "l", ylim = c(min(ts), max(ts)))

summary(gls(ts ~ years_obs, cor = corAR1(0.8)))

######################## #
# bullshit #############
####################### #

tsexp <- exp(ts)
fmexp1 <- gamm(tsexp ~ s(years_obs, bs = "cs"),
            correlation = corARMA(form = ~ 1|years_obs, p = 1))
plot(years_obs, log(fmexp1$gam$fitted.values), type = "l", ylim = c(min(ts), max(ts)))
lines(years_obs, fm1$gam$fitted.values)

plot((fmexp1$gam), shade = T, xlim = c(min(years), max(years)))

plot(fm2$gam, shade = T, xlim = c(min(years), max(years)))
plot(fm1$gam, shade = T)
plot(fm2$lme, shade = T)
plot(fm3, shade = T, xlim = c(min(years), max(years)))

acf(ts)

preds <- predict.gam(fm, newdata = data.frame("years_obs" = years_est), se.fit = T)
points(years_est, preds$fit)
points(years_est, preds$fit + 2 * preds$se.fit)










