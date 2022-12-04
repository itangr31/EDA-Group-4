# Part 1 Load Packages ----------------------------------------------------

library(tidyverse)
library(lubridate)
library(anytime)
library(rpart)
library(rsample)
library(dials)
library(tidymodels)
library(rpart.plot)
library(vip) 
library(dplyr)
library(Metrics)
library(mlr)
library(ggplot2)
library(plotly)
library(randomForest)
library(openair)
library(e1071)
library(corrplot)
library(relaimpo)
library(fpc)
library(glmnet)
library(ggpubr)

# Part 2 Load Datasets for single turbine ----------------------------------------------------
Turbine2020 <- read.csv("Turbine_Data_Penmanshiel_01_2020-01-01_-_2021-01-01_1042.csv")
Turbine2021 <- read.csv("Turbine_Data_Penmanshiel_01_2021-01-01_-_2021-07-01_1042.csv")

#Combine all datasets to get final time series data
Turbine<- rbind(Turbine2020, Turbine2021)

Turbinetest<-Turbine%>% filter(X..Date.and.time >= "23/02/2021 00:00")
Turbinetrain<-Turbine %>% filter(X..Date.and.time < "22/02/2021 00:00")
# Part 3 Data Processing --------------------------------------------------

#Convert timestamp variable to time
Turbinetrain<-Turbinetrain%>%
  mutate(date=dmy_hm(X..Date.and.time))


#Extract value from timestamp into new variables
Turbinetrain2<-Turbinetrain%>%
  mutate(hour=hour(date))%>%
  mutate(year=year(date))%>%
  mutate(month=month(date))%>%
  mutate(day=day(date))%>%
  mutate(week=week(date))

Turbinetrain2<-Turbinetrain2%>%
  mutate(sinmonth = sin(2 * pi * month / 12),
         cosmonth = cos(2 * pi * month / 12),
         sinday = sin(2 * pi * day / 7),
         cosday = cos(2 * pi * day / 7),
         cosweek = cos(2* pi * week / 52),
         sinweek = sin(2 * pi * week / 52),
         sinhour = sin(2 * pi * hour / 24),
         coshour = cos(2 * pi * hour / 24))

# Part 4 Data exploration -------------------------------------------------

#Extract variables of interest
Turbinetrain3<-Turbinetrain2[c(1, 28, 2:24, 44, 95, 247:249, 302:314)]

#Explore data
summary(Turbinetrain3)

#plot power curve
plot(Turbinetrain3$Wind.speed..m.s., Turbinetrain3$Energy.Export..kWh., main="Penmanshiel wind turbine #1 power curve before outlier removal",
     xlab="Wind speed ", ylab="Export generation ", pch=19)

#remove outliers and plot power curve
Turbinetrain4<-subset(Turbinetrain3, Lost.Production.Total..kWh.<=0)
plot(Turbinetrain4$Wind.speed..m.s., Turbinetrain4$Energy.Export..kWh., main="Penmanshiel wind turbine #1 power curve after outlier removal",
     xlab="Wind speed ", ylab="Export generation ", pch=19)


#create lagged values for variables of interest - explore what number of lags might be needed
Turbinetrain4<-Turbinetrain4%>%
  drop_na(Energy.Export..kWh.)
pacf(Turbinetrain4$Energy.Export..kWh., main = "Export Generation (kWh) Partial Autocorrelation Plot")
#the first 6 lagged values look to be the most significant
#create new variables that represent the 6 lagged values
Turbinetrain4$Energy.Export..kWh.1 <- sapply(1:nrow(Turbinetrain4), function(x) Turbinetrain4$Energy.Export..kWh.[x-1])
Turbinetrain4$Energy.Export..kWh.2 <- sapply(1:nrow(Turbinetrain4), function(x) Turbinetrain4$Energy.Export..kWh.[x-2])
Turbinetrain4$Energy.Export..kWh.3 <- sapply(1:nrow(Turbinetrain4), function(x) Turbinetrain4$Energy.Export..kWh.[x-3])
Turbinetrain4$Energy.Export..kWh.4 <- sapply(1:nrow(Turbinetrain4), function(x) Turbinetrain4$Energy.Export..kWh.[x-4])
Turbinetrain4$Energy.Export..kWh.5 <- sapply(1:nrow(Turbinetrain4), function(x) Turbinetrain4$Energy.Export..kWh.[x-5])
Turbinetrain4$Energy.Export..kWh.6 <- sapply(1:nrow(Turbinetrain4), function(x) Turbinetrain4$Energy.Export..kWh.[x-6])


Turbinetrain4<-tail(Turbinetrain4, -6)
Turbinetrain4<-Turbinetrain4%>%
  mutate(Energy.Export..kWh.1=as.numeric(Energy.Export..kWh.1))%>%
  mutate(Energy.Export..kWh.2=as.numeric(Energy.Export..kWh.2))%>%
  mutate(Energy.Export..kWh.3=as.numeric(Energy.Export..kWh.3))%>%
  mutate(Energy.Export..kWh.4=as.numeric(Energy.Export..kWh.4))%>%
  mutate(Energy.Export..kWh.5=as.numeric(Energy.Export..kWh.5))%>%
  mutate(Energy.Export..kWh.6=as.numeric(Energy.Export..kWh.6))
         

#Explore correlations of shortlisted variables
Turbinetrain4<-Turbinetrain4[c(1:6, 17:24, 27:49)]
Turbinetrain4<-Turbinetrain4[c(1:3,7,8, 15:18, 24:37)]


correlationturbine<-Turbinetrain4[c(2:23)]
correlationturbine<-correlationturbine%>%
  drop_na()
mydata.cor = cor(correlationturbine, method = c("pearson"))
corrplot(mydata.cor, type='upper', method='ellipse', col = COL2('BrBG', 10),tl.col = 'black', tl.srt = 45, tl.cex=0.75)


#Use linear regression to identify variables with statistical significance
#remove the date variable
explore1<-Turbinetrain4[c(2:23)]

linear_model1 <- lm(Energy.Export..kWh. ~ ., explore1)

# calculate relative importance
relImportance1 <- calc.relimp(linear_model1, type = "lmg", rela = TRUE)  
 
# Sort
cat('Relative Importances: \n')
RI <- sort(round(relImportance1$lmg, 3), decreasing=TRUE)
feature_importance <- as.data.frame(RI)

#All features relative importance plot
ggballoonplot(feature_importance, fill="value", size.range = c(1, 11))+
  scale_fill_viridis_c(option = "H")+
  labs(title="Relative importance of all features")

#Filter relevant features relative importance plot
feature_importance <- feature_importance %>% 
  filter_all(all_vars(. > 0.000))

sum(feature_importance$RI)

ggballoonplot(feature_importance, fill="value", size.range = c(1, 11))+
  scale_fill_viridis_c(option = "H")+
  labs(title="Relative importance of selected features")

#Lasso regression
#Scale data
explore1_scaled <- explore1%>%
  mutate(Wind.speed..m.s.=scale(Wind.speed..m.s.),
         Wind.direction....=scale(Wind.direction....),
         Energy.Export..kWh.=scale(Energy.Export..kWh.),
         Nacelle.position....=scale(Nacelle.position....),
         Nacelle.ambient.temperature...C.=scale(Nacelle.ambient.temperature...C.),
         sinmonth=scale(sinmonth),
         cosmonth=scale(cosmonth), 
         sinday=scale(sinday),
         cosday=scale(cosday),
         sinweek=scale(sinweek),
         cosweek=scale(cosweek),
         sinhour=scale(sinhour),
         coshour=scale(coshour),
         Blade.angle..pitch.position..A....=scale(Blade.angle..pitch.position..A....),
         Blade.angle..pitch.position..B....=scale(Blade.angle..pitch.position..B....),
         Blade.angle..pitch.position..C....=scale(Blade.angle..pitch.position..C....),
         Energy.Export..kWh.1=scale(Energy.Export..kWh.1),
         Energy.Export..kWh.2=scale(Energy.Export..kWh.2),
         Energy.Export..kWh.3=scale(Energy.Export..kWh.3),
         Energy.Export..kWh.4=scale(Energy.Export..kWh.4),
         Energy.Export..kWh.5=scale(Energy.Export..kWh.5),
         Energy.Export..kWh.6=scale(Energy.Export..kWh.6))

explore1_scaled<-explore1_scaled%>%
  drop_na()

y<-data.matrix(explore1_scaled[c(1)])

x<-data.matrix(explore1_scaled[c(2:22)])
cv_model <- cv.glmnet(x, y, alpha = 1)

#find optimal lambda value that minimizes test MSE
best_lambda <- cv_model$lambda.min
best_lambda
plot(cv_model)

best_model <- glmnet(x, y, alpha = 1, lambda = best_lambda)
coef(best_model)

#Create final training dataset, dropping non-important variables
finaltrain<-Turbinetrain4[c(1:3,5:7, 9:11, 18:21, 23)]
write.csv(finaltrain,"finaltrain.csv", row.names = FALSE)

#Create final test dataset, dropping non-important variables
finaltest<-Turbinetest[c(1,28,2,17,95,247,249)]
finaltest<-finaltest%>%
  mutate(date=dmy_hm(X..Date.and.time))

finaltest<-finaltest%>%
  mutate(month=month(date),
         sinmonth = sin(2 * pi * month / 12),
         cosmonth = cos(2 * pi * month / 12))

finaltest<-finaltest%>%
  drop_na(Energy.Export..kWh.)
finaltest$Energy.Export..kWh.1 <- sapply(1:nrow(finaltest), function(x) finaltest$Energy.Export..kWh.[x-1])
finaltest$Energy.Export..kWh.2 <- sapply(1:nrow(finaltest), function(x) finaltest$Energy.Export..kWh.[x-2])
finaltest$Energy.Export..kWh.3 <- sapply(1:nrow(finaltest), function(x) finaltest$Energy.Export..kWh.[x-3])
finaltest$Energy.Export..kWh.4 <- sapply(1:nrow(finaltest), function(x) finaltest$Energy.Export..kWh.[x-4])
finaltest$Energy.Export..kWh.6 <- sapply(1:nrow(finaltest), function(x) finaltest$Energy.Export..kWh.[x-6])

finaltest<-tail(finaltest, -6)
finaltest<-finaltest%>%
  mutate(Energy.Export..kWh.1=as.numeric(Energy.Export..kWh.1))%>%
  mutate(Energy.Export..kWh.2=as.numeric(Energy.Export..kWh.2))%>%
  mutate(Energy.Export..kWh.3=as.numeric(Energy.Export..kWh.3))%>%
  mutate(Energy.Export..kWh.4=as.numeric(Energy.Export..kWh.4))%>%
  mutate(Energy.Export..kWh.6=as.numeric(Energy.Export..kWh.6))

finaltest<-finaltest[-c(8:9)]
write.csv(finaltest,"finaltest.csv", row.names = FALSE)

