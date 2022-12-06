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
library(ranger)



# Part 1 Run Basic Random Forest Model -------------------------------------------
finaltrain<-finaltrain%>%
  drop_na()

n_features <- length(setdiff(names(finaltrain), "Energy.Export..kWh."))


random_forest <- randomForest(Energy.Export..kWh. ~ .-Date.and.time, 
                               data = finaltrain,
                              mtry = floor(n_features / 3))

#Use random forest model to predict export generation - basic model
pred_turbine <- finaltest %>% 
  mutate(random_forest = predict(random_forest, finaltest))

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
    ggplot()+
    geom_line(aes(x = date, y = value, colour = name, alpha =0.5)) +
    geom_point(aes(x = date, y = value, colour = name, shape = name)) +
    theme_minimal() +
    theme(legend.position = "right") +
    labs(x = "Date", y = "Energy Export (kWh)",
         title = " Actual vs Prediction")
}
plot_random_day('random_forest')


# Part 2 Tune Random Forest Model -----------------------------------------

n_features <- length(setdiff(names(finaltrain), "Energy.Export..kWh."))


train_rf1 <- ranger(
  Energy.Export..kWh. ~ .-Date.and.time, 
  data = finaltrain,
  mtry = floor(n_features / 3),
  respect.unordered.factors = "order",
  seed = 123
)

(default_rmse <- sqrt(train_rf1$prediction.error))


hyper_grid <- expand.grid(
  mtry = floor(n_features * c(.05, .15, .25, .333, .4)),
  min.node.size = c(1, 3, 5, 10), 
  replace = c(TRUE, FALSE),                               
  sample.fraction = c(.5, .63, .8),                       
  rmse = NA                                               
)


for(i in seq_len(nrow(hyper_grid))) {
  fit <- ranger(
    formula         = Energy.Export..kWh. ~ .-Date.and.time, 
    data            = finaltrain, 
    num.trees       = n_features * 10,
    mtry            = hyper_grid$mtry[i],
    min.node.size   = hyper_grid$min.node.size[i],
    replace         = hyper_grid$replace[i],
    sample.fraction = hyper_grid$sample.fraction[i],
    verbose         = FALSE,
    seed            = 123,
    respect.unordered.factors = 'order',
  )  
  hyper_grid$rmse[i] <- sqrt(fit$prediction.error)
}

hyper_grid %>%
  arrange(rmse) %>%
  mutate(perc_gain = (default_rmse - rmse) / default_rmse * 100) %>%
  head(10)



# Part 3 Run Tuned Random Forest Model with optimal parameters ------------
finaltrain<-finaltrain%>%
  drop_na()

n_features <- length(setdiff(names(finaltrain), "Energy.Export..kWh."))

random_forest1 <- randomForest(Energy.Export..kWh. ~ .-Date.and.time, 
                               data = finaltrain,
                               mtry=5,
                               min.node.size=10,
                               replace=TRUE,
                               sample.fraction=0.63)

finaltest<-finaltest%>%
  drop_na(Energy.Export..kWh.)

pred_turbine <- finaltest %>% 
  mutate(random_forest = predict(random_forest1, finaltest))

pred_turbine<-pred_turbine%>%
  mutate(date=dmy_hm(Date.and.time))


#Plot actual versus predicted
plot_random_day <- function(model_name){
  # Sample a random date
  random_date <- sample(unique(date(pred_turbine$date)), 1)
  pred_turbine %>% 
    filter(date(date) == random_date) %>% # filter to the date
    select(date, Energy.Export..kWh., model_name) %>% # select specified model and the actual values
    pivot_longer(Energy.Export..kWh.:model_name) %>% # pivot  
    ggplot()+
    geom_line(aes(x = date, y = value, colour = name, alpha =0.5)) +
    geom_point(aes(x = date, y = value, colour = name, shape = name)) +
    theme_minimal() +
    theme(legend.position = "right") +
    labs(x = "Date", y = "Energy Export (kWh)",
         title = " Actual vs Prediction")
}
plot_random_day('random_forest')


# Part 4 Check Accuracy of model ------------------------------------------
#Remove NAs in the true and predicted columns
pred_turbine<-pred_turbine%>%
  drop_na(Energy.Export..kWh.)%>%
  drop_na(random_forest)

#Calculate MAPE
pred_turbine$APE<-abs((pred_turbine$Energy.Export..kWh.-pred_turbine$random_forest)/pred_turbine$Energy.Export..kWh.) * 100

pred_turbine<-pred_turbine%>%
  filter_at(vars(APE), all_vars(!is.infinite(.)))

mean(pred_turbine$APE)

#Calculate RMSE
RMSE <- rmse(pred_turbine$random_forest, pred_turbine$Energy.Export..kWh.)
#calculate MAE
MAE <- mae(pred_turbine$random_forest, pred_turbine$Energy.Export..kWh.)


