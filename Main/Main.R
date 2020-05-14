## Main run file for estimating CFR in Canada

rm(list = ls())
library(stats)
library(utils)
library(ggplot2)
library(lubridate)
library(HDInterval)
library(fitdistrplus)

set.seed(123456)

source('functions.R')




run_estim <- function(region, nsim,  pre.mu, pre.sd) {
  # Loading Data
  my_data = load_data(region)
  # Loading r estimate, which is the ML of r at each time
  r       = load_r(region)
  
  # Estimation of the adjusted CFR
  res = calc_adj_CFR(nsim, my_data, r, pre.mu, pre.sd)
  
  # Plots 
  pdf(paste0('plot-',region,'.pdf'))
  plot_results(res, region)
  dev.off()
  
  # Write results to a CSV file:
  output.file = paste0("CFR_",region,"_Confirmed.csv")
  write.table(res, file = output.file, sep=",")
}


# Parameters for CFR estimation
nsim   = 1000    # Replication number
pre.mu = 13.59   # Mean onset-to-death lag
pre.sd = 7.85    # Std. dev. onset-to-death lag

for(region in c('Canada', 'US', 'NYC'))
  run_estim(region, nsim,  pre.mu, pre.sd)




