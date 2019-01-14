##1
library(SASxport)
A = read.xport("DEMO_D.xpt")
B = read.xport("OHX_D.xpt")
C = merge(A,B,by="SEQN")

##2
oral_data = C[!is.na(C$OHX04HTC),]
oral_data = oral_data[!is.na(oral_data$RIDAGEMN),]
oral_data = oral_data[oral_data$OHX04HTC!=9,]
oral_data$OHX04HTC[oral_data$OHX04HT == 5] = 2
oral_data$OHX04HTC[oral_data$OHX04HT == 2 | oral_data$OHX04HT == 4] = 0
mylogit1 = glm(oral_data$OHX04HTC ~ oral_data$RIDAGEMN, data = oral_data, family = "binomial")
bic1 = BIC(mylogit1)
pop_25 = trunc(((-log((1-0.25)/0.25))-8.359362)/(-0.0696778))
pop_50 = trunc(((-log((1-0.50)/0.25))-8.359362)/(-0.0696778))
pop_75 = trunc(((-log((1-0.75)/0.25))-8.359362)/(-0.0696778))
range = c(floor(((-log((1-0.75)/0.75))-8.359362)/(-0.0696778)/12),
          ceiling(((-log((1-0.25)/0.25))-8.359362)/(-0.0696778)/12))

#3
mylogit2 = glm(oral_data$OHX04HTC ~ oral_data$RIDAGEMN + oral_data$RIAGENDR, data = oral_data, family = "binomial")
bic2 = BIC(mylogit2)
#model with smaller BIC is better
oral_data$RIDRETH1[oral_data$RIDRETH1 == 5] = 2

oral_data$race1 = oral_data$RIDRETH1
oral_data$race2 = oral_data$RIDRETH1
oral_data$race4 = oral_data$RIDRETH1

oral_data$race1[oral_data$race1 == 2 | oral_data$race1 == 3 | oral_data$race1 == 4] = 0

oral_data$race2[oral_data$race2 == 1 | oral_data$race2 == 3 | oral_data$race2 == 4] = 0
oral_data$race2[oral_data$race2 == 2] = 1

oral_data$race4[oral_data$race4 == 1 | oral_data$race4 == 2 | oral_data$race4 == 3] = 0
oral_data$race4[oral_data$race4 == 4] = 1

oral_data$race1 = factor(oral_data$race1)
oral_data$race2= factor(oral_data$race2)
oral_data$race4 = factor(oral_data$race4)

mylogit3 = glm(oral_data$OHX04HTC ~ oral_data$RIDAGEMN + oral_data$race1, data = oral_data, family = "binomial")
bic3 = BIC(mylogit3)
mylogit4 = glm(oral_data$OHX04HTC ~ oral_data$RIDAGEMN + oral_data$race4, data = oral_data, family = "binomial")
bic4 = BIC(mylogit4)
mylogit5 = glm(oral_data$OHX04HTC ~ oral_data$RIDAGEMN + oral_data$race4 + oral_data$race1, data = oral_data, family = "binomial")
bic5 = BIC(mylogit5)

oral_data = oral_data[!is.na(oral_data$INDFMPIR),]
mylogit6 = glm(OHX04HTC ~ RIDAGEMN + race4 + INDFMPIR, data = oral_data, family = "binomial")
bic6 = BIC(mylogit6)




#4
library("margins")
oral_data$age_year = floor(oral_data$RIDAGEMN/12)
OHX04HTC = c(oral_data$OHX04HTC)
AGE = c(oral_data$age_year)
race4 = c(oral_data$race4)
INDFMPIR = c(oral_data$INDFMPIR)

mylogit7 = glm(OHX04HTC ~ AGE + race4 + INDFMPIR, family = "binomial")




#1
library("prediction")
a = mean(race4)
b = mean(INDFMPIR)
AGE = c(8,9,10,11)
race4 = c(a,a,a,a)
INDFMPIR = c(b,b,b,b)
pd = cbind.data.frame(AGE,race4,INDFMPIR)
margin1 = data.matrix(predict(mylogit7,pd))
row.names(margin1) = c("8","9","10","11")

#2
oral_data$age_year = floor(oral_data$RIDAGEMN/12)
OHX04HTC = c(oral_data$OHX04HTC)
AGE = c(oral_data$age_year)
race4 = c(oral_data$race4)
INDFMPIR = c(oral_data$INDFMPIR)
data1 = cbind.data.frame(OHX04HTC,AGE,race4,INDFMPIR)
data1$race4 = mean(data1$race4)
data1$INDFMPIR = mean(data1$INDFMPIR)

margin2 = summary(margins(mylogit7, data = data1, at = list(AGE = c(8,9,10,11)), variables = "race4"))
margin3 = summary(margins(mylogit7, data = data1, at = list(AGE = c(8,9,10,11)), variables = "INDFMPIR"))

#3
margin4 = summary(margins(mylogit7, at = list(AGE = c(8,9,10,11)), variables = "race4"))
margin5 = summary(margins(mylogit7, at = list(AGE = c(8,9,10,11)), variables = "INDFMPIR"))
