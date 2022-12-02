
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



# Part 2 Load Datasets for single turbine ----------------------------------------------------

Turbine2016 <- read.csv("Data/Turbine_Data_Penmanshiel_01_2016-06-06_-_2017-01-01_1042.csv", skip = 9)
Turbine2017 <- read.csv("Data/Turbine_Data_Penmanshiel_01_2017-01-01_-_2018-01-01_1042.csv", skip = 9)
Turbine2018 <- read.csv("Data/Turbine_Data_Penmanshiel_01_2018-01-01_-_2019-01-01_1042.csv", skip = 9)
Turbine2019 <- read.csv("Data/Turbine_Data_Penmanshiel_01_2019-01-01_-_2020-01-01_1042.csv", skip = 9)
Turbine2020 <- read.csv("Data/Turbine_Data_Penmanshiel_01_2020-01-01_-_2021-01-01_1042.csv", skip = 9)
Turbine2021 <- read.csv("Data/Turbine_Data_Penmanshiel_01_2021-01-01_-_2021-07-01_1042.csv", skip = 9)

#Combine all datasets to get final time series data
Turbine<- rbind(Turbine2016, Turbine2017, Turbine2018, Turbine2019, Turbine2020, Turbine2021)
Turbine<- rbind(Turbine2020, Turbine2021)



# Part 3 Data Processing --------------------------------------------------

#Explore data
summary(Turbine)

#Convert timestamp variable to time
Turbine<-Turbine%>%
  mutate(date=dmy_hm(Date.and.time))


#Extract value from timestamp into new variables
Turbine2<-Turbine%>%
  mutate(hour=hour(date))%>%
  mutate(year=year(date))%>%
  mutate(month=month(date))%>%
  mutate(day=day(date))%>%
  mutate(week=week(date))

Turbine2<-Turbine2%>%
  mutate(sinmonth = sin(2 * pi * month / 12),
         cosmonth = cos(2 * pi * month / 12),
         sinday = sin(2 * pi * day / 7),
         cosday = cos(2 * pi * day / 7),
         cosweek = cos(2* pi * week / 52),
         sinweek = sin(2 * pi * week / 52),
         sinhour = sin(2 * pi * hour / 24),
         coshour = cos(2 * pi * hour / 24))




# Part 3 Data exploration -------------------------------------------------

Turbine3<-Turbine2[c(1, 28, 2:24, 95, 247:249, 302:314)]
Turbine3<-Turbine3%>%
  drop_na()
mydata.cor = cor(Turbine3, method = c("pearson"))
corrplot(mydata.cor)


train_data <- Turbine3 %>% filter(Date.and.time < "21/02/2021 00:00")
test_data <- Turbine3 %>% filter(Date.and.time >= "22/02/2021 00:00")


# Build linear regression model
explore1<-train_data[c(2, 3:11)]
explore2<-train_data[c(2, 12:21)]
explore3<-train_data[c(2, 22:31)]
explore4<-train_data[c(2, 32:42)]


linear_model1 <- lm(Energy.Export..kWh. ~ ., explore1)
linear_model2 <- lm(Energy.Export..kWh. ~ ., explore2)
linear_model3 <- lm(Energy.Export..kWh. ~ ., explore3)
linear_model4 <- lm(Energy.Export..kWh. ~ ., explore4)


# calculate relative importance
relImportance1 <- calc.relimp(linear_model1, type = "lmg", rela = TRUE)  
relImportance2 <- calc.relimp(linear_model2, type = "lmg", rela = TRUE)  
relImportance3 <- calc.relimp(linear_model3, type = "lmg", rela = TRUE)  
relImportance4 <- calc.relimp(linear_model4, type = "lmg", rela = TRUE)  


# Sort
cat('Relative Importances: \n')
sort(round(relImportance1$lmg, 3), decreasing=TRUE)
sort(round(relImportance2$lmg, 3), decreasing=TRUE)
sort(round(relImportance3$lmg, 3), decreasing=TRUE)
sort(round(relImportance4$lmg, 3), decreasing=TRUE)






# Part X Dataset creation -------------------------------------------------

#Create new final dataset with relevant variables for first model
Turbine4<-Turbine2[c(1, 2, 4, 5, 16, 28, 17,95, 302:314)]
#Convert timestamp variable to time
Turbine4<-Turbine4%>%
  mutate(date=dmy_hm(Date.and.time))

#Create test and train data
train_data <- Turbine4 %>% filter(Date.and.time < "21/02/2021 00:00")
test_data <- Turbine4 %>% filter(Date.and.time >= "22/02/2021 00:00")

train_data<-train_data%>%
  drop_na()



#Create new final dataset with relevant variables for second model
Turbine5<-Turbine2[c(28, 1, 7, 4, 5, 2,9,10, 11, 13, 15, 12, 14,95, 247:249,311, 308, 307, 312)]
#Convert timestamp variable to time
Turbine5<-Turbine5%>%
  mutate(date=dmy_hm(Date.and.time))

#Create test and train data
train_data2 <- Turbine5 %>% filter(Date.and.time < "21/02/2021 00:00")
test_data2 <- Turbine5 %>% filter(Date.and.time >= "22/02/2021 00:00")

train_data2<-train_data2%>%
  drop_na()

# Part 4 model deployment and evaluation ----------------------------------
random_forest1 <- randomForest(Energy.Export..kWh. ~ .-Date.and.time -date, 
                              data = train_data,
                              ntree = 50)

random_forest2 <- randomForest(Energy.Export..kWh. ~ .-Date.and.time, 
                               data = train_data2,
                               ntree = 50)
# Part 4.1 Random Forest --------------------------------------------------

#Use random forest model to predict export generation
pred_turbine <- test_data %>% 
  mutate(random_forest = predict(random_forest1, test_data))

pred_turbine<-pred_turbine%>%
  mutate(date=dmy_hm(Date.and.time))

#plot actual versus predicted for a random day
plot_random_day <- function(model_name){
  # Sample a random date
  random_date <- sample(unique(date(pred_turbine$date)), 1)
  pred_turbine %>% 
    filter(date(date) == random_date) %>% # filter to the date
    select(date, Energy.Export..kWh., model_name) %>% # select specified model and the actual values
    pivot_longer(Energy.Export..kWh.:model_name) %>% # pivot  
    ggplot(aes(x = date, y = value, color = name)) + 
    geom_line() + 
    theme_light()
}
plot_random_day('random_forest')


pred_turbine<-pred_turbine%>%
  drop_na()
mean((pred_turbine$Energy.Export..kWh. - pred_turbine$random_forest)^2)



#Feature selection model
pred_turbine2 <- test_data2 %>% 
  mutate(random_forest = predict(random_forest2, test_data2))

pred_turbine2<-pred_turbine2%>%
  mutate(date=dmy_hm(Date.and.time))

pred_turbine2<-pred_turbine2%>%
  drop_na()
mean((pred_turbine2$Energy.Export..kWh. - pred_turbine2$random_forest)^2)


#Lasso regression
install.packages('glmnet')
library(glmnet)

#perform k-fold cross-validation to find optimal lambda value
Turbine2<-Turbine2%>%
  drop_na()

y<-data.matrix(Turbine3[c(2)])

x<-data.matrix(Turbine3[c(3:42)])
cv_model <- cv.glmnet(x, y, alpha = 1)

#find optimal lambda value that minimizes test MSE
best_lambda <- cv_model$lambda.min
best_lambda
plot(cv_model)

best_model <- glmnet(x, y, alpha = 1, lambda = best_lambda)
coef(best_model)




#tune random forest model
bestmtry <- tuneRF(train_data,train_data$Energy.Export..kWh.,stepFactor = 1.2, improve = 0.01, trace=T, ntree=500, plot= T)


# Part 4.2 SVM ------------------------------------------------------------

#Scale data
train_data_scaled <- train_data%>%
  mutate(Wind.speed..m.s.=scale(Wind.speed..m.s.),
         Wind.speed..Maximum..m.s.=scale(Wind.speed..Maximum..m.s.),
         Wind.speed..Minimum..m.s.=scale(Wind.speed..Minimum..m.s.),
         Wind.direction....=scale(Wind.direction....),
         Energy.Export..kWh.=scale(Energy.Export..kWh.),
         Nacelle.position....=scale(Nacelle.position....),
         Nacelle.ambient.temperature...C.=scale(Nacelle.ambient.temperature...C.),
         hour=scale(hour),
         month=scale(month), 
         day=scale(day),
         week=scale(week),
         sinmonth=scale(sinmonth),
         cosmonth=scale(cosmonth), 
         sinday=scale(sinday),
         cosday=scale(cosday),
         sinweek=scale(sinweek),
         cosweek=scale(cosweek),
         sinhour=scale(sinhour),
         coshour=scale(coshour))
         


test_data_scaled <- test_data%>%
  mutate(Wind.speed..m.s.=scale(Wind.speed..m.s.),
         Wind.speed..Maximum..m.s.=scale(Wind.speed..Maximum..m.s.),
         Wind.speed..Minimum..m.s.=scale(Wind.speed..Minimum..m.s.),
         Wind.direction....=scale(Wind.direction....),
         Energy.Export..kWh.=scale(Energy.Export..kWh.),
         Nacelle.position....=scale(Nacelle.position....),
         Nacelle.ambient.temperature...C.=scale(Nacelle.ambient.temperature...C.),
         hour=scale(hour),
         month=scale(month), 
         day=scale(day),
         week=scale(week),
         sinmonth=scale(sinmonth),
         cosmonth=scale(cosmonth), 
         sinday=scale(sinday),
         cosday=scale(cosday),
         sinweek=scale(sinweek),
         cosweek=scale(cosweek),
         sinhour=scale(sinhour),
         coshour=scale(coshour))


linear.tune<-tune.svm(Energy.Export..kWh.~.-Date.and.time -date,data=train_data_scaled,kernel="linear",cost = c(.001,.01,.1,1,5,10))

best.linear<-linear.tune$best.model


