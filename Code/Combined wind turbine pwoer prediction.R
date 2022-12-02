
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

# Part 2 Load Datasets for single turbine ----------------------------------------------------

Turbine2016 <- read.csv("Data/Turbine_Data_Penmanshiel_01_2016-06-06_-_2017-01-01_1042.csv")
Turbine2017 <- read.csv("Data/Turbine_Data_Penmanshiel_01_2017-01-01_-_2018-01-01_1042.csv")
Turbine2018 <- read.csv("Data/Turbine_Data_Penmanshiel_01_2018-01-01_-_2019-01-01_1042.csv")
Turbine2019 <- read.csv("Data/Turbine_Data_Penmanshiel_01_2019-01-01_-_2020-01-01_1042.csv")
Turbine2020 <- read.csv("Data/Turbine_Data_Penmanshiel_01_2020-01-01_-_2021-01-01_1042.csv")
Turbine2021 <- read.csv("Data/Turbine_Data_Penmanshiel_01_2021-01-01_-_2021-07-01_1042.csv")



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

#Create new final dataset with relevant variables
Turbine3<-Turbine2[c(1, 2, 4, 5, 16, 28, 17,95, 302:314)]
#Convert timestamp variable to time
Turbine3<-Turbine3%>%
  mutate(date=dmy_hm(Date.and.time))


#Create test and train data
train_data <- Turbine3 %>% filter(Date.and.time < "21/02/2021 00:00")
test_data <- Turbine3 %>% filter(Date.and.time >= "22/02/2021 00:00")

train_data<-train_data%>%
  drop_na()


# Part 4 model deployment and evaluation ----------------------------------
linear_model <- lm(Energy.Export..kWh. ~ .-Date.and.time, train_data)
random_forest <- randomForest(Energy.Export..kWh. ~ .-Date.and.time, 
                              data = train_data,
                              ntree = 200)

# Part 4.1 Random Forest --------------------------------------------------

#Use random forest model to predict export generation
pred_turbine <- test_data %>% 
  mutate(random_forest = predict(random_forest, test_data))

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


