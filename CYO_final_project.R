## Matt Harvill
## Choose-Your-Own Final Project
## Harvardx: PH125.9x - Capstone
## https://github.com/mpharvill

# INTRODUCTION

# OVERVIEW

## Data Loading
### load dataset, necessary packages and libraries

##### Note: this process could take a couple of minutes
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(readr)) install.packages("readr", repos = "http://cran.us.r-project.org")
if(!require(stringer)) install.packages("stringer", repos = "http://cran.us.r-project.org")
if(!require(AER)) install.packages("AER", repos = "http://cran.us.r-project.org")
if(!require(texreg)) install.packages("texreg", repos = "http://cran.us.r-project.org")

library(tidyverse)
library(caret)
library(data.table)
library(readr)
library(stringr)
library(AER)
library(ggplot2)
library(gridExtra)
library(texreg)
library(rpart)
library(randomForest)

data("Affairs")
options(digits = 3)

## METHODS & ANALYSIS

### Data Analysis ###

#### first 7 rows with header

head(Affairs)

#### basic summary statistics

summary(Affairs)

#### number of affairs by survey respondents

table(Affairs$affairs)

#### look at female/male proportion

prop.table(table(Affairs$gender))
                  
#### comparing affair tendency by gender

Affairs %>% 
  group_by(gender) %>%
  summarize(average = mean(affairs), twelve_or_more = mean(affairs == 12))

#### plots looking at the breakdown of the different affair factors

age_brkdwn <- Affairs  %>% ggplot() +
  geom_density(aes(age, fill = gender, col = gender), alpha = 0.5)

yrs_brkdwn <- Affairs  %>% ggplot() +
  geom_density(aes(yearsmarried, fill = gender, col = gender), alpha = 0.5)

chld_brkdwn <- Affairs %>% ggplot() +
  geom_bar(aes(children, fill = gender, col = gender), alpha = 0.5)

rlgn_brkdwn <- Affairs %>% ggplot() +
  geom_density(aes(religiousness, fill = gender, col = gender), alpha = 0.5)

edctn_brkdwn <- Affairs %>% ggplot() +
  geom_bar(aes(education, fill = gender, col = gender), alpha = 0.5)

occptn_brkdwn <- Affairs %>% ggplot() +
  geom_bar(aes(occupation, fill = gender, col = gender), alpha = 0.5)

rtng_brkdwn <-Affairs %>% ggplot() +
  geom_bar(aes(rating, fill = gender, col = gender), alpha = 0.5)

grid.arrange(age_brkdwn, yrs_brkdwn, chld_brkdwn, rlgn_brkdwn, edctn_brkdwn,
             occptn_brkdwn, rtng_brkdwn, ncol = 2)

#### outcome is binary so transform affairs into yes/no

Affairs$y_n_affairs[Affairs$affairs > 0] <- 1
Affairs$y_n_affairs[Affairs$affairs == 0] <- 0
Affairs$y_n_affairs <- factor(Affairs$y_n_affairs, levels = c(0,1), labels = c("No", "Yes"))

#### check success
table(Affairs$y_n_affairs)

### simple linear regession models
age_lm <- lm(affairs ~ age, data = Affairs)
yearsmarried_lm <- lm(affairs ~ yearsmarried, data = Affairs)
children_lm <- lm(affairs ~ children, data = Affairs)
religiousness_lm <- lm(affairs ~ religiousness, data = Affairs)
education_lm <- lm(affairs ~ education, data = Affairs)
occupation_lm <- lm(affairs ~ occupation, data = Affairs)
rating_lm <- lm(affairs ~ rating, data = Affairs)

screenreg(list(age_lm, yearsmarried_lm, children_lm, religiousness_lm,
               education_lm, occupation_lm, rating_lm))
summary(age_lm)

fit_all <- glm(y_n_affairs ~ gender + age + yearsmarried + children + religiousness + education +
               occupation + rating, data = Affairs, family = binomial())
summary(fit_all)

fit_fewer <- glm(y_n_affairs ~ age + yearsmarried + religiousness + rating, data = Affairs,
                 family = binomial())
summary(fit_fewer)
coef(fit_fewer)

## Probablities based on significant factors

new_ages <- tibble(age = c(18, 28, 38, 48, 58), yearsmarried = mean(Affairs$yearsmarried),
                      religiousness = mean(Affairs$religiousness), rating = mean(Affairs$rating))
new_ages$probability <- predict(fit_fewer, newdata = new_ages, type = "response")
new_ages

new_years <- tibble(age = mean(Affairs$age), yearsmarried = c(0.125,0.417,0.75,1.5,4,7,10,15), religiousness = mean(Affairs$religiousness),
                    rating = mean(Affairs$rating))
new_years$probability <- predict(fit_fewer, newdata = new_years, type = "response")
new_years 

new_rlgn <- tibble(age = mean(Affairs$age), yearsmarried = mean(Affairs$yearsmarried),
                   religiousness = c(1,2,3,4,5), rating = mean(Affairs$rating))
new_rlgn$probability <- predict(fit_fewer, newdata = new_rlgn, type = "response")
new_rlgn

new_rating <- tibble(age = mean(Affairs$age), yearsmarried = mean(Affairs$yearsmarried),
                   religiousness = mean(Affairs$religiousness), rating = c(1,2,3,4,5))
new_rating$probability <- predict(fit_fewer, newdata = new_rating, type = "response")
new_rating

### knn

Affairs %>% as_tibble()
table(Affairs$rating)
Affairs <- select(Affairs, -gender, -children, -education, -occupation)

set.seed(1983, sample.kind = "Rounding")

fit <- train(rating ~ .,  method = "knn", 
             tuneGrid = data.frame(k = seq(1, 15, 2)), 
             data = Affairs)
ggplot(fit)

Affairs %>% 
  mutate(y_hat = predict(fit)) %>% 
  ggplot() +
  geom_jitter(aes(rating, affairs), col = "darkblue",alpha = 0.5) +
  geom_jitter(aes(rating, y_hat), col="darkred", alpha = 0.2)

### regression tree

fit <- rpart(affairs ~ ., data = Affairs)

fit <- rpart(affairs ~ ., data = Affairs, control = rpart.control(cp = 0, minsplit = 2))
Affairs %>% 
  mutate(y_hat = predict(fit)) %>% 
  ggplot() +
  geom_jitter(aes(rating, affairs), col = "darkblue", alpha = 0.5) +
  geom_jitter(aes(rating, y_hat), col="darkred", alpha = 0.2)

train_rpart <- train(affairs ~ ., method = "rpart", tuneGrid = data.frame(cp = seq(0, 0.05, len = 25)), data = Affairs)
ggplot(train_rpart)

plot(train_rpart$finalModel, margin = 0.1)
text(train_rpart$finalModel, cex = 0.75)

Affairs %>% 
  mutate(y_hat = predict(train_rpart)) %>% 
  ggplot() +
  geom_jitter(aes(rating, affairs), col = "darkblue", alpha = 0.5) +
  geom_jitter(aes(rating, y_hat), col="darkred", alpha = 0.2)

### random forest

fit <- randomForest(affairs ~., data = Affairs) 
plot(fit)

Affairs %>%
  mutate(y_hat = predict(fit, newdata = Affairs)) %>% 
  ggplot() +
  geom_jitter(aes(rating, affairs), col = "darkblue", alpha = 0.5) +
  geom_jitter(aes(rating, y_hat), col="darkred", alpha = 0.2)


## RESULTS




## CONCLUSION

#