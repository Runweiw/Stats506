library(data.table)
library(doParallel)
library(foreach)

source('ps4_q2_funcs.R')

n = 1000
p = 100

ncores = 4
cl = makeCluster(ncores)
registerDoParallel(cl)

result_b =  foreach(rho = (-3:3)*0.25, .combine = 'rbind', .packages = c('data.table', 'doParallel')) %dopar% {
  foreach(sigma = c(0.25, 0.5, 1), .combine = 'rbind', .packages = c('data.table', 'doParallel')) %dopar% {
  sim_beta(rho = rho, sigma = sigma)
  }
}

result_b

stopCluster(cl)

