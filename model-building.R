# This file is used to build predictive models for session conversion

# load library
library(nnet)
library(forcats)
library(NeuralNetTools)
library(rpart)
library(rpart.plot)
library(ranger)
library(gridExtra)
library(radiant)
options(scipen = 999) # disable scientific notation

#I.Feature selection & engineering ---------------------------------------------------------

### 1. Feature selection 
# 1.1. check correlation for all numeric variables
M <- cor(select_if(gap_train, is.numeric))  
corrplot::corrplot(M, method = "circle")
M > 0.8 
# high correlation: no_page vs. recommendation / product view, recommendation vs. productview / addtocart, 
# addtocart vs. productview

# if remove addtocart, productview, recommendation, no_page, viewcart:
M <- cor(select_if(gap_train, is.numeric)[, c(1,3,4,6, 7,8)])  
corrplot::corrplot(M, method = "circle")
M > 0.8 # no correlation score over 0.8

# 1.2. check correlation among categorical variables:

# device type vs. ismobile
ggplot(gap_train, aes(buyer, viewcart)) + stat_summary(fun.y = "mean", geom = "bar")
ggplot(filter(gap_train, viewcart < 30), aes(viewcart, fill = buyer)) + 
  geom_histogram(stat = "bin", bin = 100) + 
  ggtitle("No. of Viewcart vs. Conversion")

# device type vs. ismobile
ggplot(gap_train, aes(devicetype, fill = ismobile)) + geom_bar(stat = "count")
chisq.test(gap_train$ismobile, gap_train$devicetype, correct = FALSE) 
#since the p-value is less than 0.05, we can reject the null hypothesis and conclude that the variables 
#are dependent to each other.

# domain vs. paidcampaign
ggplot(gap_train, aes(domain, fill = paidcampaign)) + geom_bar(stat = "count") # remove paidcampaign
chisq.test(gap_train$domain, gap_train$paidcampaign, correct = FALSE) 

# domain vs. class (external search term)
ggplot(gap_train, aes(domain, fill = class)) + geom_bar(stat = "count") # remove class
chisq.test(gap_train$domain, gap_train$class, correct = FALSE) 

# internalsearch vs. search_internal
ggplot(gap_train, aes(internalsearch, fill = search_internal)) + geom_bar(stat = "count") # remove internalsearch
chisq.test(gap_train$internalsearch, gap_train$search_internal, correct = FALSE) 

# 1.3. correlation between nominal and numeric values
# weather condition vs. temperature
ggplot(gap_train, aes(weathercondition, temperature, fill = weathercondition)) + 
  stat_summary(fun.y="mean", geom="bar")

# device type vs. temperature
ggplot(gap_train, aes(devicetype, temperature, fill = devicetype)) + 
  stat_summary(fun.y="mean", geom="bar")


### 2. Feature engineering: data skewness: "no_direvent", "viewcart", "wishlist"
par(mfrow = c(2, 2))
hist(gap_train$no_direvent, main = "Distribution of Direct Event")    
hist(gap_train$viewcart, main = "Distribution of View Cart")
hist(gap_train$wishlist, main = "Distribution of Wishlist")
hist(gap_train$no_page, main = "Distribution of Session Depth")

# tranform data into logarithm in train, val and test set
gap_train <- gap_train %>% 
  mutate(log_direvent = log(no_direvent + 0.00000001), 
#         log_viewcart = log(viewcart + 0.0000001),
         log_wishlist = log(wishlist + 0.0000001))

gap_test <- gap_test %>% 
  mutate(log_direvent = log(no_direvent + 0.0000001), 
#         log_viewcart = log(viewcart + 0.0000001), 
         log_wishlist = log(wishlist + 0.0000001))

gap_val <- gap_val %>% 
  mutate(log_direvent = log(no_direvent + 0.0000001), 
#         log_viewcart = log(viewcart + 0.0000001), 
         log_wishlist = log(wishlist + 0.0000001))

gap <- gap %>% 
  mutate(log_direvent = log(no_direvent + 0.0000001), 
#         log_viewcart = log(viewcart + 0.0000001), 
         log_wishlist = log(wishlist + 0.0000001))

# check correlation again:
M <- cor(select_if(gap_train, is.numeric)[, c(1, 7, 10:11)])  
corrplot::corrplot(M, method = "circle")
M > 0.8 # no correlation score over 0.8

# II. Modeling
## 1. Neural Network
## Standardize data for nn
gap_train_scaled <- scaledf(gap_train, sf = 2)

gap_scaled <- gap %>%
  copy_attr(gap_train_scaled, c("ms","sds")) %>%
  scaledf(calc = FALSE)

## Tune nn
auc <- function(data, lev = NULL, model = NULL) {
  c(auc = radiant.model::auc(data$yes, data$obs, "yes"))
}

rvar <- "buyer"
evar <- c("day_of_week", "weathercondition", "devicetype", "temperature", 
          "search_internal", "domain", "time_of_day", "no_direvent",
          "wishlist","addtocart")

grid <- expand.grid(size = 1:5, decay = seq(0.1, 1, 0.1))

ctrl <- trainControl(
  method = "cv", 
  number = 10, 
  classProbs = TRUE,
  summaryFunction = auc, 
  verboseIter = TRUE
)

#Tune model
# set.seed(1234)
# result <- train(
#   select(gap_train_scaled, evar), 
#   gap_train_scaled[[rvar]], 
#   method = "nnet",
#   trControl = ctrl, 
#   tuneGrid = grid, 
#   metric = "auc", 
#   rang = .1,
#   skip = FALSE, 
#   linout = FALSE, 
#   trace = FALSE, 
#   maxit = 10000
# )

#Aggregating results: Selecting tuning parameters: Fitting size = 5, decay = 0.2 on full training set

## Run nn
nn_result <- ann(
  gap_train, 
  rvar = "buyer", 
  evar = c(
    "day_of_week", "weathercondition", "devicetype", "temperature", 
    "search_internal", "domain", "time_of_day", "no_direvent",
    "wishlist"
  ), 
  lev = "yes", 
  size = 5, 
  decay = 0.1, 
  seed = 1234
)
## predict buyer
eval_dat$nn <- predict(nn_result, pred_data = gap)$Prediction

## Performance check
## calculate auc in test dataset
#install.packages("Metrics")
auc_nn <- Metrics::auc(actual = ifelse(eval_dat[training == 0, ]$buyer == "yes", 1, 0),
                       predicted = eval_dat[training == 0, ]$nn)

## auc in val
### test <- predict(nn_refit, pred_data = gap_val)[, 16]
auc_nn_val <-  Metrics::auc(actual = ifelse(gap_val$buyer == "yes", 1, 0),
                            predicted = predict(nn_result, pred_data = gap_val)$Prediction)

## confusion matrix
confusionMatrix(data = ifelse(eval_dat[training == 0, ]$nn > 0.5, "yes", "no"),
                reference = eval_dat[training == 0, ]$buyer)

## evaluate overfitting
evalbin(
  eval_dat,
  pred = "nn",
  rvar = rvar,
  lev = lev,
  qnt = 50,
  train = "Both",
  data_filter = "training == 1"
) %>% plot(plots = "gains")

## plot nn
plot(nn_result, plots = "olden", custom = FALSE)
plot(nn_result, plots = "garson", custom = FALSE)

## 2. Logistic regression
### Model building
set.seed(1234)
logit <- train(
  select(gap_train, c("day_of_week", "weathercondition", "devicetype", "temperature", 
                      "search_internal", "domain", "time_of_day", "log_direvent",
                      "log_wishlist")),
  gap_train[[rvar]],
  method="glm", 
  family="binomial",
  trControl = fitControl,
  metric = "ROC")

## predict buyer
eval_dat$logit <- predict(logit, newdata = gap, type = "prob")[, 2]

## calculate auc in test dataset
auc_logit <- Metrics::auc(actual = ifelse(eval_dat[training == 0, ]$buyer == "yes", 1, 0),
                       predicted = eval_dat[training == 0, ]$logit) 

## confusion matrix in test set: accuracy
confusionMatrix(data = ifelse(predict(logit, newdata = gap_test, type = "prob")[, 2] > 0.5, "yes", "no"),
                reference = gap_test$buyer)

##prediction in validation set
table(gap_val$buyer)
result <- logistic(
  gap_train, 
  rvar = "buyer", 
  evar = c(
    "day_of_week", "weathercondition", "devicetype", "temperature", 
    "search_internal", "domain", "time_of_day", "log_direvent", "log_wishlist"
  ), 
  lev = "yes",
  #wts = "wts",
  check = "VIF"
)

# make prediction on validation set
pred <- predict(result, pred_data = gap_val)[, "Prediction"]

## auc in val
auc_logit_val <-  Metrics::auc(actual = ifelse(gap_val$buyer == "yes", 1, 0),
                               predicted = predict(result, pred_data = gap_val)[, "Prediction"]) 

## confusion matrix in val set: accuracy
confusionMatrix(data = ifelse(pred > 0.95, "yes", "no"),
                reference = gap_val$buyer) 

## evaluate overfitting
evalbin(
  eval_dat,
  pred = "logit",
  rvar = rvar,
  lev = lev,
  qnt = 50,
  train = "Both",
  data_filter = "training == 1"
) %>% plot(plots = "gains")


#===I try to add interactions to improve performance of logistic regression
# add interaction: dayofweek vs. domain / devicetype
# logit_int <- train(
#   buyer ~ . + day_of_week:devicetype + day_of_week:domain,
#   data = gap_train[, c("buyer", "day_of_week", "weathercondition", "devicetype", "temperature", 
#                        "search_internal", "domain", "time_of_day", "log_direvent","log_wishlist")],
#   method="glm", 
#   family="binomial",
#   trControl = fitControl,
#   metric = "ROC")
# 
# ## auc in val
# auc_logit_val_int <-  Metrics::auc(actual = ifelse(gap_val$buyer == "yes", 1, 0),
#                                predicted = predict(logit_int, newdata = gap_val, type = "prob")[, 2]) # 76.1% / 76%
# 
# ## confusion matrix in val set: accuracy
# result_logit_int <- logistic(
#   gap_train, 
#   rvar = "buyer", 
#   evar = c(
#     "day_of_week", "weathercondition", "devicetype", "temperature", 
#     "search_internal", "domain", "time_of_day", "log_direvent", "log_wishlist"
#   ), 
#   lev = "yes",
#   wts = "wts",
#   check = "VIF"
# )
# 
# # make prediction on validation set
# pred_int <- predict(result_logit_int, pred_data = gap_val)[, "Prediction"]
# 
# confusionMatrix(data = ifelse(pred_int > 0.5, "yes", "no"), reference = gap_val$buyer) # 83.2%
## interaction does not help enhance model performance.

