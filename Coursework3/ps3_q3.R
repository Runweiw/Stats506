###########a
library(faraway)
library(data.table)
library(tidyverse)
data("mtcars")
vars = data.table(mtcars$disp, mtcars$hp, mtcars$wt)
car = data.table(mtcars$mpg, mtcars$cyl, vars)
setnames(car, c("mpg", "cyl", "disp", "hp", "wt"))

beta_1 = car[order(cyl)] %>%
  .[, ':='(var_1_mean = mean(disp), var_2_mean = mean(hp), var_3_mean = mean(wt)), by = .(cyl)] %>%
  .[, ':='(var_1_gc = disp - var_1_mean, var_2_gc = hp - var_2_mean, var_3_gc = wt - var_3_mean)] %>%
  .[, ':='(var_1_mpg = var_1_gc*mpg, var_2_mpg = var_2_gc*mpg, var_3_mpg = var_3_gc*mpg)] %>%
  .[, ':='(var_1_sq = var_1_gc^2, var_2_sq = var_2_gc^2, var_3_sq = var_3_gc^2)] %>%
  .[, .(sum_1_mpg = sum(var_1_mpg), sum_1_sq = sum(var_1_sq), sum_2_mpg = sum(var_2_mpg),  sum_2_sq = sum(var_2_sq), sum_3_mpg = sum(var_3_mpg),  sum_3_sq = sum(var_3_sq)), by = .(cyl)] %>%
  .[, .(cyl = c(4,6,8), beta_disp = sum_1_mpg/sum_1_sq, beta_hp = sum_2_mpg/sum_2_sq, beta_wt = sum_3_mpg/sum_3_sq)]

write.csv(beta, file = 'mpg_beats_cyl.csv')



############b
beta_2 = function(d, group, x, y)
{
  dt = d[, c(group, x, y), with = FALSE]
  colnames(dt) = c('group', 'x', 'y')
  
  order = dt[order(group)]
  var_mean = order[, var_mean := mean(x), by = .(group)]
  var_gc = var_mean[, var_gc := x - var_mean]
  var_y = var_gc[, var_y := var_gc*(y)]
  var_sq = var_y[, var_sq := var_gc^2]
  sum_y = var_sq[, .(sum_y = sum(var_y), sum_sq = sum(var_sq)), by = .(group)]
  beta = sum_y[, .(beta = sum_y/sum_sq)]
  return(beta)
}

#####test
beta_2(d = car, group = 'cyl', x = 'disp', y = 'mpg')
beta_2(d = car, group = 'cyl', x = 'hp', y = 'mpg')
beta_2(d = car, group = 'cyl', x = 'wt', y = 'mpg')


#######c
beta_3 = group_by(car, cyl) %>% 
  summarise_at(vars(c('disp', 'hp', 'wt')),
               funs(beta_cyl = sum((. -mean(.))*mpg)/sum((. -mean(.))^2)))


###########d
beta_4 = function(d, group, x, y)
{
  group = enquo(group)
  x = enquo(x)
  y = enquo(y)
  beta = group_by(d, (!!group)) %>%
    summarise_at(vars(!!x),
                 funs(beta_x = sum((!!x -mean(!!x))*!!y)/sum((!!x -mean(!!x))^2)))
  return(beta)
}


############test
v1 = beta_4(car, cyl, disp, mpg)
v2 = beta_4(car, cyl, hp, mpg)
v3 = beta_4(car, cyl, wt, mpg)
                 
                 
                 
                 