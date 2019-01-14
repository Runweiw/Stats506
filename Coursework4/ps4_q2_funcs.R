#generate X
gen_X = function(rho, beta=c(rep(0.1, 10), rep(0, 90)), p=100)
{
  Sigma = matrix(0, p, p)
  for (i in 1:p)
    {
    for (j in 1:p)
      {
      Sigma[i,j] = rho*beta[i]*beta[j]
      }
    }
  diag(Sigma) = 1
  R = chol(Sigma) 
  X = matrix(rnorm(n*p), n, p) %*%  R
  return(X)
}



# Compute est and se for fwer, fdr, sens, spec
evaluate = function(P, tp_ind=1:10, alpha = 0.05)
{
  #Return logic value 0/1, True = 1 FALSE = 0.
  P = P < alpha
  p = nrow(P)
  n = ncol(P)
  
  # Compute TP, FP, TN, FN for each replcation.
  TP = colSums(P[tp_ind, ])
  FP = colSums(P[-tp_ind,])
  TN = colSums(!P[-tp_ind,])
  FN = colSums(!P[tp_ind,])
  
  # Call FDR 0 when no discoveries. 
  P = FP + TP
  fdr = ifelse(P>0, FP/{FP+TP}, 0)
  fwer = mean( FP>0 )
  sens = TP/{TP + FN}
  spec = TN/{FP + TN}
  list(fwer = fwer, fwer_se = sqrt(fwer*{1-fwer}/n), 
       fdr =  mean(fdr), fdr_se = sd(fdr)/sqrt(n),
       sens = mean(sens), sens_se = sd(sens)/sqrt(n),
       spec = mean(spec), spec_se = sd(spec)/sqrt(n))
}



#Monte Carlo simulation function for fixed X
sim_beta = function(rho, sigma = 1, beta=c(rep(0.1, 10), rep(0, 90)), mc_rep = 1e4)
{
  X = gen_X(rho)
  QR = qr(crossprod(X))
  QX = X %*% qr.Q(QR) 
  XtXinv = solve(qr.R(QR), t(qr.Q(QR)))
  
  n = nrow(X)
  p = ncol(X)
  
  # Generate mc_rep copies of Y at once, each in a column.
  Y = as.numeric(X %*% beta) + rnorm(n*mc_rep)
  dim(Y) = c(n, mc_rep)
  
  # estimate betas and residual standard errors
  b = solve(qr.R(QR), crossprod( QX, Y ) )
  s_sq = colSums({Y - as.numeric(X %*% b)}^2/{n - p})
  
  # standard error of b
  v = sqrt(diag(XtXinv)*rep(s_sq, each = p))
  # return a matirx of p-values
  
  # Use pt to replicate lm, but the normal approximation is fine here. 
  P = matrix(2*pt(abs(b/v), df = {n-p}, lower.tail = FALSE), p, mc_rep)
  
  # define metric and method
  metric = rep(c('fwer', 'fdr', 'sensitivity', 'specifity'),4)
  method = rep(c('holm', 'bonferroni', 'BH', 'BY'), each=4)
  all0 =lapply( c('holm', 'bonferroni', 'BH', 'BY'), function(x)
    {
    evaluate(apply(P, 2, p.adjust, method = x))
    })
  all = rbindlist(all0)
  
  # compute est,se
  est = as.vector(cbind(all$fwer, all$fdr, all$sens, all$spec))
  se = as.vector(cbind(all$fwer_se, all$fdr_se, all$sens_se, all$spec_se))
  Sigma = c(rep(sigma,16))
  Rho = c(rep(rho,16))
  
  # get data set
  cbind.data.frame(Rho,Sigma,method,metric,est,se)
}



