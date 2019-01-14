########a
library(MASS)
p_value = function(X, beta, sigma, mc_rep)
{
  n = 1000
  p = 100
  set.seed(1000)
  QR = qr(t(X) %*% X)
  Q = qr.Q(QR)
  R = qr.R(QR)
  beta_hat = matrix(0, nrow = mc_rep, ncol = p)
  sigma_hat = c(rep(0,mc_rep))
  Y = mvrnorm(n = mc_rep, mu = X%*%beta, Sigma = (sigma^2)*diag(1, n, n))
  for(i in 1:mc_rep)
  {
    beta_hat[i,] = solve(R, t(Q) %*% t(X) %*% Y[i,])
    sigma_hat[i] = (1/(n-p))*t(as.matrix(Y[i,])-X%*%(as.matrix(beta_hat[i,])))%*%(as.matrix(Y[i,])-X%*%(as.matrix(beta_hat[i,])))
  }
  v = (sigma_hat^2)*chol2inv(chol(t(X)%*%X))
  Z = matrix(0, nrow = mc_rep, ncol = p)
  p_value = matrix(0, nrow = mc_rep, ncol = p) 
  for(i in 1:mc_rep)
  {
    Z[i,] = beta_hat[i,]/sqrt(v[i,i])
    for(j in 1:p)
    {
      p_value[i,j] = 2*(1-pnorm(abs(Z[i,j])))
    }
  }
  p_value = t(p_value)
  return(p_value)
}

posdef <- function (n, ev = runif(n, 0, 10)) 
{
  Z <- matrix(rnorm(n^2), ncol=n)
  decomp <- qr(Z)
  Q <- qr.Q(decomp) 
  R <- qr.R(decomp)
  d <- diag(R)
  ph <- d / abs(d)
  O <- Q %*% diag(ph)
  Z <- t(O) %*% diag(ev) %*% O
  return(Z)
}
#define a positive definite matrix
n = 1000
p = 100
E = posdef(p)
mc_rep = 2
X = mvrnorm(n = n, mu = c(rep(0,p)), Sigma = E)
beta = c(rep(1, 10), rep(0, dim(X)[2]-10))
sigma = 1
a = p_value(X, beta, sigma, mc_rep)
set.seed(1000)
Y = mvrnorm(n = mc_rep, mu = X%*%beta, Sigma = (sigma^2)*diag(1, n, n))
b = summary(lm(t(Y) ~ 0 + X))
c1 = b$`Response Y1`$coefficients[,4]
c2 = b$`Response Y2`$coefficients[,4]
compare = cbind(a[1:15,1], c1[1:15], a[1:15,2], c2[1:15])
colnames(compare) = c('test_p1', 'real_p1', 'test_p2', 'real_p2')



###########b
n = 1000
p = 100
mc_rep = 5
E = posdef(p)
R = chol(sigma)
beta = c(rep(1, 10), rep(0, dim(X)[2]-10))
M = mvrnorm(n = n, mu = c(rep(0,p)), Sigma = E)
sigma = 1
a = p_value(M, beta, sigma, mc_rep)
a1 = a[1:15,1:5]
colnames(a1) = c('rep_1', 'rep_2', 'rep_3', 'rep_4', 'rep_5')

#######c
pv = p_value(X, beta, sigma, mc_rep)
evaluate = function(pv)
{
  mc_rep = dim(pv)[2]
  sum = matrix(nrow = 4, ncol = mc_rep)
  for (i in 1:mc_rep)
  {
    e1 = as.data.frame(pv<0.05)[1:10, i]
    e2 = as.data.frame(pv<0.05)[11:100, i]
    FN = sum(e1 == 0)
    FP = sum(e2 == 1)
    TN = 90 - FP
    TP = 10 - FN
    FWER = 1L * any(FP)
    FDR = FP/(TP+FP)
    Sens = TP/(TP+FN)
    Spec = TN/(FP+TN)
    sum[1,i] = FWER
    sum[2,i] = FDR 
    sum[3,i] = Sens
    sum[4,i] = Spec
  }
  sum
}

q = evaluate(pv)
m = apply(q, 1, mean)
s = apply(q, 1, sd)
sum_q = cbind(c('FWER','FDR','Sensity','Specity'), m,s)
colnames(sum_q) = c('var',' mean', 'sd')


######d
m1 = evaluate(as.matrix(p.adjust(pv, 'bonferroni'), ncol = mc_rep))
m2 = evaluate(as.matrix(p.adjust(pv, 'holm'), ncol = mc_rep))
m3 = evaluate(as.matrix(p.adjust(pv, 'BH'), ncol = mc_rep))
m4 = evaluate(as.matrix(p.adjust(pv, 'BY'), ncol = mc_rep))
total = cbind(m1, m2, m3, m4)
colnames(total) = c('bonferroni', 'holm', 'BH', 'BY')



#######e
a1 = apply(m1, 2, mean)
a2 = apply(m2, 2, mean)
a3 = apply(m3, 2, mean)
a4 = apply(m4, 2, mean)
s1 = apply(m1, 2, sd)
s2 = apply(m2, 2, sd)
s3 = apply(m3, 2, sd)
s4 = apply(m4, 2, sd)
sum_e = cbind(c('mean', 'sd'), c(a1, s1), c(a2, s2), c(a3, s3), c(a4, s4))
colnames(sum_e) = c('var', 'bonferroni', 'holm', 'BH', 'BY')

