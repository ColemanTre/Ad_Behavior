
## Libraries ##
library(ggplot2)
library(tidyverse)
library(datasets)
library(bestglm)
library(caret)
library(countrycode)
library(DataExplorer)





## Read in data ##
data = read.csv('bq-results-20201013-133031-gi2msesmnqa8.csv')





## Simulation ##

#Create a prompt id to attach an ad to
data <- data %>%
  mutate(prompt_id = paste(promptTimestamp, promptText))


#This attaches an ad to every prompt
prompt_id <- unique(data$prompt_id)
advert <- c("Coca-cola", 'Fanatics', "Nike", "Adidas", "Under Armor", "Pepsi", "ABC", "Empty")
prompt_id_advert <- data.frame(prompt_id, advert = sample(advert, length(prompt_id), replace = T))
data <- left_join(data, prompt_id_advert, by = "prompt_id")

# This attaches a rule to every prompt
rules = c('1-player-ejection', 
          '2-flgrant-foul',
          '3-defense-goaltending',
          '4-player-3pt-streak',
          '5-streak-in-paint',
          '6-team-point-streak',
          '7-team-miss-streak',
          '8-player-dominating',
          '10-pre-game',
          '11-player-scoring-average',
          '12-missed-reverse-layup',
          '13-jump-ball',
          '14-ref-review',
          '15-offensive-foul',
          '16-replay-challenge',
          '17-travel',
          '18-technical-foul',
          '19-defensive-rebounds',
          '20-shot-clock-violation',
          '21-dunk',
          '23-center-makes-3pointer',
          '27-foul-trouble')

prompt_id_rule <- data.frame(prompt_id, rules.var = sample(rules, length(prompt_id), replace = T))
data <- left_join(data, prompt_id_rule, by = "prompt_id")

#This creates a distribution to be used in calculating whether or not the user interacted with the ad
users <- unique(data$userHandle)
user_effect <- data.frame(userHandle = users, user_effect = rnorm(length(users))) #Generates a unique probability for each user
advert_effect <- data.frame(advert, advert_effect = rnorm(length(advert))) #Generates a unique probability for each ad type
intensity_effect <- -.01 #I wanted to include some effect for the intensity
baseline <- -4.5 #this is log odds based on a 1% probability .01/.99
baseline2 <- -2.95 #I tried two baselines because the first was so small that no purchases were made. This one is base on a 5% chance. Log(.05/95)

#Add user effect and advert effect to the data
data <- data %>% left_join(user_effect, by = "userHandle") %>% left_join(advert_effect, by = "advert")

#Calculate the probability using the effects
data <- data %>% mutate(log_odds = baseline2 + user_effect + advert_effect + intensity_effect * intensity) %>%
  mutate(prob = plogis(log_odds))

#Use the probability to determine if the user clicked the ad
data <- data %>% mutate(clicked = rbinom(nrow(.) , 1, prob))

#simulate if the user made a purchase
data <- data %>% mutate(log_odds2 = baseline2 + user_effect + advert_effect + intensity_effect * intensity) %>%
  mutate(prob2 = plogis(log_odds2) * clicked)

data <- data %>% mutate(purchase = rbinom(nrow(.) , 1, prob2))

#change clicked and purchase to factors
data$clicked <- as.factor(data$clicked)
data$purchase <- as.factor(data$purchase)



## Feature Engineering ##

## feature engineer response as positive and negative
data$firstResponseOption <- as.character(data$firstResponseOption)
data$secondResponseOption <- as.character(data$secondResponseOption)
ans_rating = ifelse(data$selectedResponse == data$firstResponseOption, 'Positive', 'Negative')

data <- data %>% mutate(ans_rating = as.factor(ans_rating))

## Feature Engineer country by continent
continent <- countrycode(data$userCountry, origin = 'country.name', destination = 'region')
data <- data %>% mutate(continent = as.factor(continent))

data$userCountry <- as.character(data$userCountry)
isUS <- ifelse(data$userCountry == "United States of America", 'US', 'Elsewhere')

data <- data %>% mutate(isUS = as.factor(isUS))



## Variable Selection ##
str(data)
select_data <- data %>% select(intensity, advert, clicked, purchase)


  
  
  
## Exploratory Data Analysis ##
plot_correlation(select_data)

histogram.clicked <- ggplot(data = data, mapping = aes(x = clicked)) +
  geom_histogram(stat = "count", fill = 'lightblue') +
  theme(aspect.ratio = 1)
histogram.clicked

histogram.purchase <- ggplot(data = data, mapping = aes(x = purchase)) +
  geom_histogram(stat = "count", fill = 'lightblue')+
  theme(aspect.ratio = 1)
histogram.purchase

intensity.v.clicked <- ggplot(data = data, mapping = aes(x = clicked, y = intensity)) +
  geom_boxplot(fill = "lightblue")+
  theme(aspect.ratio = 1)
intensity.v.clicked

intensity.v.purchase <- ggplot(data = data, mapping = aes(x = clicked, y = intensity)) +
  geom_boxplot(fill = "lightblue")+
  theme(aspect.ratio = 1)
intensity.v.purchase

advert.v.purchase <- ggplot(data = data, mapping = aes(x = advert, y = sum(as.numeric(purchase)))) +
  geom_col(fill = "lightblue")+
  theme(aspect.ratio = 1)
advert.v.purchase

advert.v.clicked <- ggplot(data = data, mapping = aes(x = advert, y = sum(as.numeric(clicked)))) +
  geom_col(fill = "lightblue")+
  theme(aspect.ratio = 1)
advert.v.clicked


## Modeling ##

# I've chosen the xgbTree model in the caret library. In my experience it has consistently been the most reliable.
# It is a gradient boosted random forest model. There may be better models for your actual data.


data.train <- data[1:2000000,]
data.test <- data[2000001:2397242,]

clicked.model <- train(form= clicked ~ userBirthYear + intensity + advert + ans_rating + continent,
                     data = data.train,
                     method="xgbTree",
                     tuneGrid = expand.grid(nrounds = 150,
                                            max_depth = 3,
                                            eta =  .5,
                                            gamma = .3,
                                            colsample_bytree = .6,
                                            min_child_weight = .6,
                                            subsample = 1),
                     trControl=trainControl(method='repeatedcv', number = 20, repeats = 2),
                     verboseIter= T,
                     na.action = na.pass)

clicked.preds <- predict(clicked.model, newdata= data.test %>% 
                           select(clicked, userBirthYear, intensity, advert, ans_rating, continent))

purchased.model <- train(form=purchase ~ userBirthYear + intensity + advert + ans_rating + continent,
                       data = data.train,
                       method="xgbTree",
                       tuneGrid = expand.grid(nrounds = 150,
                                              max_depth = 3,
                                              eta =  .5,
                                              gamma = .3,
                                              colsample_bytree = .6,
                                              min_child_weight = .6,
                                              subsample = 1),
                       trControl=trainControl(method='repeatedcv', number = 20, repeats = 2),
                       verboseIter= T,
                       na.action = na.pass)

purchased.preds <- predict(purchased.model, newdata = data.test%>% 
                             select(clicked, userBirthYear, intensity, advert, ans_rating, continent))


