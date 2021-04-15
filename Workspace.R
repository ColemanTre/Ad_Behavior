##Data Simulation

#
user_id = 1:24
ads_seen = rpois(24, 100)
ad_seen1 = rbinom(24, 1, ads_seen/max(ads_seen))
ad_interaction = rbinom(21, 1, prob=(ads_seen/max(ads_seen)/10))
ad_type1 = 'pepsi'


ad_seen2 = rbinom(24, 1, ads_seen/max(ads_seen))
ad_interax2 = rbinom(21,1,prob=(ads_seen/max(ads_seen)/5))
ad_type2 = 'fanatics'

ad_df = data.frame(user_id, ads_seen, ad_interaction, rep(ad_type1, length(user_id)))
##Use r.unif to estimate probability

num_users <- length(data$userHandle)
dist <- rpois(num_users, 110)

#Look up how poisson and binomial distributions work
#Look up probabilities for ad interactions and purchases

#use beta distribution to generate the probability, plug that into the poisson distribution. 

#What companies are you looking at for ads? What are the different types of ads that you are thinking of using?

#Just do different ad types that can be renamed. Use macro variables for probabilities and distribution parameters.

#Time spent with ad. What does it mean to interact with an ad?

#What will your data look like when you have it?

library(tidyverse)
data <- data %>% 
  mutate(prompt_id = paste(promptTimestamp, promptText))


#This attaches an ad to every prompt
prompt_id <- unique(data$prompt_id)
advert <- c("Coca-cola", 'Fanatics', "Nike", "Adidas", "Under Armor", "Pepsi", "ABC", "Empty")
prompt_id_advert <- data.frame(prompt_id, advert = sample(advert, length(prompt_id), replace = T))
data <- left_join(data, prompt_id_advert, by = "prompt_id")

#This creates a distribution to be used in calculating whether or not the user interacted with the ad
users <- unique(data$userHandle)
user_effect <- data.frame(userHandle = users, user_effect = rnorm(length(users))) #Generates a unique probability for each user
advert_effect <- data.frame(advert, advert_effect = rnorm(length(advert))) #Generates a unique probability for each ad type
intensity_effect <- -.01 #I wanted to include some effect for the intensity
baseline <- -4.5 #this is log odds based on a 1% probability .01/.99

data <- data %>% left_join(user_effect, by = "userHandle") %>% left_join(advert_effect, by = "advert")

data <- data %>% mutate(log_odds = baseline + user_effect + advert_effect + intensity_effect * intensity) %>%
  mutate(prob = plogis(log_odds))

#Creates a distribution to be used in calculating whether or not the user made a purchase



#This calculates whether or not the user clicked on the add
data <- data %>% mutate(clicked = rbinom(nrow(.) , 1, prob))

#Logistic regression models
library(bestglm)

data2 <- data %>% 
  mutate(intensity = as.numeric(intensity), clicked = as.factor(clicked), userBirthYear = as.numeric(userBirthYear)) %>% 
  select(feedName, eventid, eventTime, userHandle, userCountry, userBirthYear, promptTimestamp, 
         promptText, firstResponseOption, secondResponseOption, intensity,
         responseTimestamp, advert, clicked)

## This does variable selection, but it won't work with randomized data
## Requires feature engineering. There are too many factor levels with the randomized data. 
# data2.best.subsets.bic <- bestglm(data2,
#                                   IC = "BIC",
#                                   method = "exhaustive",
#                                   TopModels = 1,
#                                   family = binomial)
# summary(data.best.subsets.bic$BestModel)

data3 <- head(data2, n=40)
write.csv(data3, "./data3.csv")

ggplot(data3, aes(x= clicked, y = userHandle)) +
  geom_point() +
  xlab("Ad Interaction (yes/no)") +
  ylab("userHandle")

data2.logistic <- glm(clicked ~ .,
                      data = data2,
                      family = binomial(link = "logit"))
summary(data2.logistic)

