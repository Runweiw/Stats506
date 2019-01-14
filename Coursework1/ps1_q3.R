#Q3
#A
library(dplyr)
library(tidyr)
library(ggplot2)
recs = readr::read_delim("https://www.eia.gov/consumption/residential/data/2015/csv/recs2015_public_v3.csv", delim = ',')
recs_A = recs[,c("WALLTYPE","DIVISION","NWEIGHT")]
wall_material_division = group_by(recs_A,WALLTYPE,DIVISION)
wall_material_division_sum = summarise(wall_material_division,sum=sum(NWEIGHT))
division = group_by(recs_A,DIVISION)
division_sum = summarise(division,sum=sum(NWEIGHT))
stucco_division = wall_material_division_sum[31:40,]
stucco_division[,"percent"] = format(stucco_division$sum/division_sum$sum,digits = 2)
stucco_division[,2] = c("New England", 
                        "Middle Atlantic", 
                        "East North Central", 
                        "West North Central", 
                        "South Atlantic", 
                        "East South Central", 
                        "West South Central", 
                        "Mountain North", 
                        "Mountain South", 
                        "Pacific")




#B
recs_B1 = recs[,c("DIVISION","KWH","NWEIGHT")]
kwh_devision = group_by(recs_B1,DIVISION)
kwh_devision_sum = summarise(kwh_devision,mean_kwh = sum(KWH*NWEIGHT)/sum(NWEIGHT))
kwh_devision_sum[,1] = c("New England", 
                         "Middle Atlantic", 
                         "East North Central", 
                         "West North Central", 
                         "South Atlantic", 
                         "East South Central", 
                         "West South Central", 
                         "Mountain North", 
                         "Mountain South", 
                         "Pacific")



recs_B2 = recs[,c("DIVISION", "UATYP10", "KWH", "NWEIGHT")]
kwh_type_division = group_by(recs_B2,DIVISION,UATYP10)
kwh_type_division_sum = summarise(kwh_type_division,mean_kwh_type = sum(KWH*NWEIGHT)/sum(NWEIGHT))
kwh_type_division_sum_new = spread(kwh_type_division_sum,key = UATYP10, value = mean_kwh_type)
kwh_type_division_sum_new[,1] = c("New England", 
                                  "Middle Atlantic", 
                                  "East North Central", 
                                  "West North Central", 
                                  "South Atlantic", 
                                  "East South Central", 
                                  "West South Central", 
                                  "Mountain North", 
                                  "Mountain South", 
                                  "Pacific")
names(kwh_type_division_sum_new)=c("DIVISION","Urban Cluster","Rural","Urban Area")


#C
recs_C = recs[,c("DIVISION", "UATYP10", "INTERNET", "NWEIGHT")]
internet_division = group_by(recs_C,DIVISION,UATYP10)
internet_division_sum = summarise(internet_division,mean_internet = sum(INTERNET*NWEIGHT)/sum(NWEIGHT))
internet_division_sum_new=spread(internet_division_sum,key = UATYP10, value = mean_internet)
internet_division_sum_new[,1] = c("New England", 
                                  "Middle Atlantic", 
                                  "East North Central", 
                                  "West North Central", 
                                  "South Atlantic", 
                                  "East South Central", 
                                  "West South Central", 
                                  "Mountain North", 
                                  "Mountain South", 
                                  "Pacific")
names(internet_division_sum_new)=c("DIVISION","Urban Cluster","Rural","Urban Area")

