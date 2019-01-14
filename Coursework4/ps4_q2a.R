library(data.table)
library(parallel)

source('ps4_q2_funcs.R')

n = 1000
p = 100
rho = 0.25*{-3:3}
sigma = 1

result_a = rbindlist(mclapply(rho, sim_beta))

                     