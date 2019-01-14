library(data.table)
library(future)
library(parallel)
source('ps4_q2_funcs.R')
plan(multisession)

n=1000
p=100
rho=0.25*{-3:3}

args=commandArgs(trailingOnly = TRUE)

args_to_list = function(args){
  ind = grep('=', args)
  args_list = strsplit(args[ind], '=')
  names(args_list) = sapply(args_list, function(x) x[1])
  args_list = lapply(args_list, function(x) as.numeric(x[2]))
  return(args_list)
}

args_list_in = args_to_list(args)
sigma0 = args_list_in$sigma

sim_beta1 = function(rho, sigma, mc_rep)
{
  beta=c(rep(0.1, 10), rep(0, 90))
  X=gen_X(rho)
  QR = qr( crossprod(X) )
  QX = X %*% qr.Q(QR) 
  XtXinv = solve( qr.R(QR), t( qr.Q(QR) ))
  n = nrow(X)
  p = ncol(X)
  # Generate mc_rep copies of Y at once, each in a column.
  Y = as.numeric(X %*% beta) + rnorm(n*mc_rep)
  dim(Y) = c(n, mc_rep)
  # estimate betas and residual standard errors
  b = solve(qr.R(QR), crossprod( QX, Y ) )
  s_sq = colSums( {Y - as.numeric(X %*% b)}^2 / {n - p})
  # standard error of b
  v = sqrt( diag(XtXinv) * rep(s_sq, each = p) )
  P = matrix(2*pt(abs(b/v), df = {n-p}, lower.tail = FALSE), p, mc_rep)
  all0 =lapply(c('holm', 'bonferroni', 'BH', 'BY'), function(x)
    {
    evaluate( apply(P, 2, p.adjust, method = x))
    })
  all = rbindlist(all0)
  # compute est,se
  metric = rep(c('fwer', 'fdr', 'sensitivity', 'specifity'),4)
  method = rep(c('holm', 'bonferroni', 'BH', 'BY'), each=4)
  est = as.vector(cbind(all$fwer,all$fdr,all$sens,all$spec))
  se = as.vector(cbind(all$fwer_se,all$fdr_se,all$sens_se,all$spec_se))
  Rho = c(rep(rho,16))
  data.frame(Rho,metric,method,est,se)
}

result_c_list=list()

for (i in 1:length(rho))
{
  metric = rep(c('fwer', 'fdr', 'sensitivity', 'specifity'),4)
  method = rep(c('holm', 'bonferroni', 'BH', 'BY'), each=4)
  Rho = rep(rho[i],16)
  result_c_list[[i]]=future({paste(Rho,
                                   rep(sigma0,16),
                                   metric,
                                   method,
                                   sim_beta1(rho=rho[i], sigma=sigma0, mc_rep = 1e4)$est,
                                   sim_beta1(rho=rho[i], sigma=sigma0, mc_rep = 1e4)$se)})
}
x = unlist(lapply(result_c_list, value))
out = strsplit(x, ' ') 
result_c = data.frame(matrix(unlist(out), nrow=112, byrow=T))
names(result_c) = c('rho', 'sigma', 'metric', 'method', 'est', 'se')

result_c

