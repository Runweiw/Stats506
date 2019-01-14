library(dplyr)
library(tidyr)
library(data.table)
recs =  fread('https://www.eia.gov/consumption/residential/data/2015/csv/recs2015_public_v3.csv')
#read data as a data table


####A
recs_A = recs[,c("DOEID","WALLTYPE","DIVISION","NWEIGHT")]
division = recs_A[, .(division_sum = sum(NWEIGHT)), by = .(DIVISION)] %>%
  .[order(DIVISION)]
wall_material_division = recs_A[WALLTYPE == 4, .(wall_material_division_sum = sum(NWEIGHT)), by = .(DIVISION)] %>%
  .[order(DIVISION)] %>%
  .[division, , on = 'DIVISION'] %>%
  .[, PERCENT := wall_material_division_sum/division_sum] %>%
  .[order(-PERCENT)]
#calculate percentage

weights = recs[,c(1, 475:571)]
weights_long = melt(data = weights, id.vars = "DOEID", measure.vars = c(3:98), variable.name = 'repl', value.name = 'w') %>%
  .[, NWEIGHT := NULL]
#in order to calculate variance, change the wide chart into a long one
  
p_stucco_r_A = weights_long %>%
  .[recs_A, , on = 'DOEID'] %>%
  .[, .(r_stucco =sum( w*{WALLTYPE == 4} ) / sum(w)), by = .(DIVISION, repl)]

p_stucco_r_A = p_stucco_r_A %>% 
  .[wall_material_division, on = 'DIVISION']

p_stucco_A = p_stucco_r_A %>%
  .[, .(se_stucco = 2 * sqrt(mean((r_stucco - PERCENT)^2))), by = 'DIVISION' ] %>%
  .[order(DIVISION)]

wall_material_division = wall_material_division %>%
  .[p_stucco_A, on = 'DIVISION'] %>%
  .[, DIVISION := c("New England", 
                    "Middle Atlantic", 
                    "East North Central", 
                    "West North Central", 
                    "South Atlantic", 
                    "East South Central", 
                    "West South Central", 
                    "Mountain North", 
                    "Mountain South", 
                    "Pacific")]




####B
recs_B = recs[,c("DOEID","DIVISION", "UATYP10", "KWH", "NWEIGHT")]
kwh_division = recs_B[, .(mean_kwh = sum(KWH*NWEIGHT)/sum(NWEIGHT)), by = .(DIVISION)] %>%
  .[order(DIVISION)]

p_stucco_r_B1 = weights_long %>%
  .[recs_B, , on = 'DOEID'] %>%
  .[, .(r_stucco =sum(KWH*w) / sum(w)), by = .(DIVISION, repl)]

p_stucco_r_B1 = p_stucco_r_B1 %>% 
  .[kwh_division,on = 'DIVISION']

p_stucco_B1 = p_stucco_r_B1 %>%
  .[, .(se_stucco = 2 * sqrt(mean((r_stucco - mean_kwh)^2))), by = 'DIVISION' ] %>%
  .[order(DIVISION)]

kwh_division = kwh_division %>%
  .[p_stucco_B1, on = 'DIVISION'] %>%
  .[, DIVISION := c("New England", 
                    "Middle Atlantic", 
                    "East North Central", 
                    "West North Central", 
                    "South Atlantic", 
                    "East South Central", 
                    "West South Central", 
                    "Mountain North", 
                    "Mountain South", 
                    "Pacific")]



kwh_type_division = recs_B[UATYP10 == 'U' | UATYP10 == 'R', .(mean_kwh_type = sum(KWH*NWEIGHT)/sum(NWEIGHT)), by = .(DIVISION, UATYP10)] %>%
  .[order(DIVISION)]
kwh_type_division_sum = spread(kwh_type_division, key = UATYP10, value = mean_kwh_type) %>%
  .[order(DIVISION)]

p_stucco_r_B2_U = weights_long %>%
  .[recs_B, , on = 'DOEID'] %>%
  .[UATYP10 == 'U', .(r_stucco =sum(KWH*w) / sum(w)), by = .(DIVISION, repl)]

p_stucco_r_B2_U = p_stucco_r_B2_U %>% 
  .[kwh_type_division_sum, on = 'DIVISION']

p_stucco_B2_U = p_stucco_r_B2_U %>%
  .[, .(se_stucco_U = 2 * sqrt(mean((r_stucco - U)^2))), by = 'DIVISION' ] %>%
  .[order(DIVISION)]


p_stucco_r_B2_R = weights_long %>%
  .[recs_B, , on = 'DOEID'] %>%
  .[UATYP10 == 'R', .(r_stucco =sum(KWH*w) / sum(w)), by = .(DIVISION, repl)]

p_stucco_r_B2_R = p_stucco_r_B2_R %>% 
  .[kwh_type_division_sum, on = 'DIVISION']

p_stucco_B2_R = p_stucco_r_B2_R %>%
  .[, .(se_stucco_R = 2 * sqrt(mean((r_stucco - R)^2))), by = 'DIVISION' ] %>%
  .[order(DIVISION)] %>%
  .[p_stucco_B2_U, on = 'DIVISION'] %>%
  .[kwh_type_division_sum, on = 'DIVISION']

kwh_type_division_sum = setcolorder(p_stucco_B2_R, c(1,4,2,5,3))
#reorder

kwh_type_division_sum[, DIVISION := c("New England", 
                    "Middle Atlantic", 
                    "East North Central", 
                    "West North Central", 
                    "South Atlantic", 
                    "East South Central", 
                    "West South Central", 
                    "Mountain North", 
                    "Mountain South", 
                    "Pacific")]



####C
recs_C = recs[,c("DOEID","DIVISION", "UATYP10", "INTERNET", "NWEIGHT")]
internet_division = recs_C[UATYP10 == 'U' | UATYP10 == 'R', .(mean_internet = sum(INTERNET*NWEIGHT)/sum(NWEIGHT)), by = .(DIVISION,UATYP10)] %>%
  .[order(DIVISION)]
internet_division_sum = spread(internet_division, key = UATYP10, value = mean_internet) %>%
  .[, Difference := U-R]


p_stucco_r_C_U = weights_long %>%
  .[recs_C, , on = 'DOEID'] %>%
  .[UATYP10 == 'U', .(r_stucco =sum(INTERNET*w) / sum(w)), by = .(DIVISION, repl)]

p_stucco_r_C_U = p_stucco_r_C_U %>% 
  .[internet_division_sum, on = 'DIVISION']

p_stucco_C_U = p_stucco_r_C_U %>%
  .[, .(se_stucco_U = 2 * sqrt(mean((r_stucco - U)^2))), by = 'DIVISION' ] %>%
  .[order(DIVISION)]


p_stucco_r_C_R = weights_long %>%
  .[recs_C, , on = 'DOEID'] %>%
  .[UATYP10 == 'R', .(r_stucco =sum(INTERNET*w) / sum(w)), by = .(DIVISION, repl)]

p_stucco_r_C_R = p_stucco_r_C_R %>% 
  .[internet_division_sum, on = 'DIVISION']

p_stucco_C_R = p_stucco_r_C_R %>%
  .[, .(se_stucco_R = 2 * sqrt(mean((r_stucco - R)^2))), by = 'DIVISION' ] %>%
  .[order(DIVISION)]

p_stucco_r_C_D = weights_long %>%
  .[recs_C, , on = 'DOEID'] %>%
  .[UATYP10 == 'R', .(r_stucco =sum(INTERNET*w) / sum(w)), by = .(DIVISION, repl)]

p_stucco_r_C_D = p_stucco_r_C_D %>% 
  .[internet_division_sum, on = 'DIVISION']

p_stucco_C_D = p_stucco_r_C_D %>%
  .[, .(se_stucco_D = 2 * sqrt(mean((r_stucco - Difference)^2))), by = 'DIVISION' ] %>%
  .[order(DIVISION)] %>%
  .[p_stucco_C_R, on = 'DIVISION'] %>%
  .[p_stucco_C_U, on = 'DIVISION'] %>%
  .[internet_division_sum, on = 'DIVISION']


internet_division_sum = setcolorder(p_stucco_C_D, c(1,5,3,6,4,7,2))
#reorder

internet_division_sum[, DIVISION := c("New England", 
                                      "Middle Atlantic", 
                                      "East North Central", 
                                      "West North Central", 
                                      "South Atlantic", 
                                      "East South Central", 
                                      "West South Central", 
                                      "Mountain North", 
                                      "Mountain South", 
                                      "Pacific")]




####D
#Percent of homes that Months swimming pool used more than 6 months in the last year in each division.
recs_D = recs[,c("DOEID","DIVISION", "MONPOOL", "NWEIGHT")]
monpool = recs_D[, .(monpool_sum = sum(NWEIGHT)), by = .(DIVISION)] %>%
  .[order(DIVISION)]
monpool_division = recs_D[MONPOOL > 6, .(monpool_greater_than_six_sum = sum(NWEIGHT)), by = .(DIVISION)] %>%
  .[order(DIVISION)] %>%
  .[monpool, , on = 'DIVISION']
monpool_division[, PERCENT := monpool_greater_than_six_sum/monpool_sum]

p_stucco_r_D = weights_long %>%
  .[recs_D, , on = 'DOEID'] %>%
  .[, .(r_stucco =sum( w*(MONPOOL > 6)) / sum(w)), by = .(DIVISION, repl)]

p_stucco_r_D = p_stucco_r_D %>% 
  .[monpool_division, on = 'DIVISION']

p_stucco_D = p_stucco_r_D %>%
  .[, .(se_stucco = 2 * sqrt(mean((r_stucco - PERCENT)^2))), by = 'DIVISION' ] %>%
  .[order(DIVISION)]

monpool_division = monpool_division %>%
  .[p_stucco_D, on = 'DIVISION'] %>%
  .[, DIVISION := c("New England", 
                    "Middle Atlantic", 
                    "East North Central", 
                    "West North Central", 
                    "South Atlantic", 
                    "East South Central", 
                    "West South Central", 
                    "Mountain North", 
                    "Mountain South", 
                    "Pacific")]
