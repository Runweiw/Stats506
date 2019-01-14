#Q2
#A
library(dplyr)
library(tidyr)
library(tidyverse)
library(nycflights13)
flights13 = nycflights13::flights
f13 = subset(flights13,flights13$month<11) #pick a subset which month<11
airlines_f13 = group_by(f13,carrier) #group 
airlines_f13_sum = summarise(airlines_f13,percent = n()/length(f13$carrier)) #count numbers and do calculation
#reference: https://jbhender.github.io/Stats506/F18/dplyr_tidyr.html
airlines_more_than_one_percent_13 = subset(airlines_f13_sum, airlines_f13_sum$percent>0.01)
airlines_more_than_onepercent = airlines_more_than_one_percent_13
airlines_more_than_onepercent$carrier = c("Endeavor Air Inc.", "American Airlines Inc.", "JetBlue Airways", "Delta Air Lines Inc.", "ExpressJet Airlines Inc.", "AirTran Airways Corporation", "Envoy Air", "United Air Lines Inc.", "US Airways Inc.", "Virgin America", "Southwest Airlines Co.")



#B
flights14 = read.csv("https://raw.githubusercontent.com/wiki/arunsrinivasan/flights/NYCflights14/flights14.csv",header = T)
f14 = subset(flights14, flights14$month<11)
airlines_f14 = group_by(f14,carrier)
airlines_f14_sum = summarise(airlines_f14, percent=n()/length(f14$carrier))
airlines_more_than_one_percent_13$carrier = as.character(airlines_more_than_one_percent_13$carrier)
#turn both coloumn into characters so that left_join won't go wrong
airlines_f14_sum$carrier = as.character(airlines_f14_sum$carrier)
change = left_join(airlines_more_than_one_percent_13, airlines_f14_sum, by="carrier")
#reference: http://stat545.com/bit001_dplyr-cheatsheet.html

#the diftribution of p is Bernoulli distribution
ci = function(p,n) #calculate its ci
{
  estimate = 1.96*(p*(1-p)/n)^0.5
  ci1 = p - estimate
  ci2 = p + estimate
  ci = sprintf("(%4.4f, %4.4f)", ci1, ci2)
  return(ci)
}
c1 = ci(change$percent.x,281373)
c2 = ci(change$percent.y,253316)
change[,4:5] = c(c1,c2)
change[,3:5] = change[,5:3]
change[,4:5] = change[,5:4]
change[,6] = change[,4]-change[,2]
change[1:11,1] = c("Endeavor Air Inc.", "American Airlines Inc.", "JetBlue Airways", "Delta Air Lines Inc.", "ExpressJet Airlines Inc.", "AirTran Airways Corporation", "Envoy Air", "United Air Lines Inc.", "US Airways Inc.", "Virgin America", "Southwest Airlines Co.")
names(change) = c("Carrier", "Percent_13", "CI_13", "Percent_14", "CI_14", "Change")


#C
new_list = rbind(flights13[,c(10,13)], flights14[,c(9,12)])
flight_airport = group_by(new_list, carrier,origin)
flight_airport_sum = summarise(flight_airport, count=n())
airlines_more_than_one_percent_13$carrier = as.character(airlines_more_than_one_percent_13$carrier) 
flight_airport_sum$carrier = as.character(flight_airport_sum$carrier) #allow these two factors can be left_join without warning
flight_airport_11 = left_join(airlines_more_than_one_percent_13,flight_airport_sum,by="carrier")
flight_airport_11 = flight_airport_11[,-2]
airport_new = group_by(new_list,origin)
airport_new_sum = summarise(airport_new,n())
flight_airport_percent = c()
for (i in 1:30)
{
  if(flight_airport_11$origin[i] == "EWR")
  {
    flight_airport_percent[i] = flight_airport_11$count[i]/208235
  }
  else if(flight_airport_11$origin[i] == "JFK")
  {
    flight_airport_percent[i] = flight_airport_11$count[i]/192762
  }
  else
  {
    flight_airport_percent[i] = flight_airport_11$count[i]/189095
  }
}
flight_airport_11[,4] = flight_airport_percent
ci = function(p,n)
{
  estimate = 1.96*(p*(1-p)/n)^0.5
  ci1 = p - estimate
  ci2 = p + estimate
  ci = sprintf("(%4.4f, %4.4f)", ci1, ci2)
  return(ci)
}
ci = ci(flight_airport_11$V4, 590092)
flight_airport_11[,5] = ci
names(flight_airport_11)=c("Carrier","Origin","Count","Percent","CI")
ewr = c()
jfk = c()
lga = c()
for (i in 1:30)
{
  if(flight_airport_11$Origin[i] == "EWR")
  {
    ewr[i] = flight_airport_11$Percent[i]
  }
  else if(flight_airport_11$Origin[i] == "JFK")
  {
    jfk[i] = flight_airport_11$Percent[i]
  }
  else
  {
    lga[i] = flight_airport_11$Percent[i]
  }
}
flight_airport_11[,1] = c(rep("Endeavor Air Inc.",3), rep("American Airlines Inc.", 3), rep("JetBlue Airways", 3), rep("Delta Air Lines Inc.",3), rep("ExpressJet Airlines Inc.", 3), "AirTran Airways Corporation", rep("Envoy Air", 3), rep("United Air Lines Inc.", 3), rep("US Airways Inc.", 3), rep("Virgin America", 3), rep("Southwest Airlines Co.",2))
