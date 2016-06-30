library(readr)
library(ggplot2)
library(scales)
library(broom)
library(dplyr)
library(lubridate)
library(ISLR)
library(glmnet)
library(plotrix) # in order to use standard deviation error function (std.error)

trips_with_weather <- inner_join(trips, weather, by="ymd")

#Predicting daily Citibike trips
  
# 1.You can use any features you like that are available prior to the day in question, ranging from the weather, to the time of year and day of week, 
#to activity in previous days or weeks, but don't cheat and use features from the future (e.g., the next day's trips).

df1 <- trips_with_weather %>% group_by(ymd, tmin,tmax,snow,snwd,prcp) %>% summarize(num_trips = n())
df1 <- df1 %>% mutate(days_of_the_week = wday(ymd,label = TRUE))
model1 <- lm(num_trips ~ days_of_the_week, df1)
holidays <- as.Date(c("2014-01-01", "2014-01-20", "2014-02-17","2014-05-26", "2014-07-04", "2014-09-01", "2014-10-13", "2014-11-27", "2014-12-25", "2014-12-31"))
df1<-mutate(df1, is_holiday = ymd %in% holidays)



is_weekend = function(vec)
{
  col = vector(mode= "numeric", length = length(vec))
  if (wday(vec) ==1 | wday(vec)==7)
  {
    TRUE
  }
  else
  {
    FALSE
  }
}
is_weekend = Vectorize(is_weekend)
df1$is_week_end = is_weekend(df1$ymd)


# 2.As usual, split your data into training and testing subsets and evaluate performance on each.
indexes <- sample(1:nrow(df1), size=0.2*nrow(df1))
TTtest=df1[indexes, ]
TTtrain=df1[-indexes, ]

# 3.Quantify your performance in two ways: R^2 (or the square of the correlation coefficient), as we've been doing, and with root mean-squared error.
model2 <- lm(num_trips ~ days_of_the_week, TTtest)
TTtest$predicted <- predict(model2)
cor(TTtest$predicted, TTtest$num_trips)^2
#R^2(honest) = 0.0381

summary(model2) 
# R^2 = 0.04408


###Using train data
model4 <- lm(num_trips ~ days_of_the_week + poly(tmax,10) + is_holiday + prcp:is_week_end, TTtrain)
TTtest$predicted <- predict(model4, TTtest)
cor(TTtest$predicted, TTtest$num_trips)^2
#R^2(honest) = 0.8743
summary(model4)
#R^2 = 0.8765
RMSE <- sqrt(mean((TTtest$num_trips-TTtest$predicted)^2))
#3512.585

###Using a for loop to determine best degree value for tmax
X <- c(1:20)
train_cor1 = c()
test_cor1 = c()
for (i in 1:20){
  lm.fit1 <- lm(num_trips ~ poly(tmax,i) + is_holiday, TTtrain)
  
  
  TTtest$predicted <- predict(lm.fit1, TTtest)
  TTtrain$predicted <- predict(lm.fit1, TTtrain)
  
  train_cor1[i] <- cor(TTtrain$predicted,TTtrain$num_trips)^2
  test_cor1[i] <- cor(TTtest$predicted, TTtest$num_trips)^2
  
  
}
ggplot() + geom_line(mapping = aes(x = X,y = test_cor1, color = "blue")) + geom_line(mapping = aes(x = X,y = train_cor1, color = "green")) ### k = 10
ggplot(TTtrain) + geom_histogram(aes(x = prcp)) + scale_x_log10(TTtest$prcp) 

# 4.Report the model with the best performance on the test data. Watch out for overfitting.
model4 <- lm(num_trips ~ days_of_the_week + poly(tmax,10) + is_holiday + prcp:is_week_end, TTtrain) # best model 

# 5.Plot your final best fit model in two different ways. First with the date on the x-axis and the number of trips on the y-axis, showing the actual values as 
#points and predicted values as a line. Second as a plot where the x-axis is the predicted value and the y-axis is the actual value, with each point representing one day.
ggplot(TTtest) + geom_point(aes(x = ymd, y = num_trips, color = is_week_end)) + geom_line(aes(x = ymd, y = predicted, color = is_week_end))
ggplot(TTtest) + geom_point(aes(x = predicted , y = num_trips, color = is_week_end)) 

# 6.Inspect the model when you're done to figure out what the highly predictive features are, and see if you can prune away any negligble features that don't matter much.
model4 <- lm(num_trips ~ days_of_the_week + poly(tmax,10) + is_holiday + prcp:is_week_end, TTtrain) # best model 

#Using glmnet
x <- model.matrix(num_trips ~. , df1)
y <- df1$num_trips
grid = 10^seq(10,-2,length=100)
model5 <- glmnet(x, y, alpha = 0, lambda = grid)
dim(coef(model5))

# We now split the samples into a training set and a test set in order to estimate the test error of ridge regression and the lasso
set.seed(1)
train = sample(1:nrow(x), 0.2*nrow(x))
test = (-train)
y.test = y[test]

set.seed(1)
cv.out = cv.glmnet(x[test,],y[test],alpha = 0) # Using cv.glmnet(0 function to choose a cross validation value for alpha)
plot(cv.out)

bestlam = cv.out$lambda.min # pick up best value for lambda
bestlam
# 963.3034

ridge.pred = predict(ridge.mod, s = bestlam ,newx = x[test,])
MSE <- sqrt(mean((ridge.pred-y.test)^2))
MSE
#3831.34

#############################################################################################################################################################################

# Implement 5-fold cross-validation for your Citibike model to get a better estimate of the error on the testing data.
# 1.Hint: you can use something like df$fold <- sample(1:5, nrow(df), replace=T) to randomly assign each row of a data frame to one of five folds, 
#and then select the training and test data using this (e.g., train <- filter(df, fold != 1)] and test <- filter(df, fold == 1))

df1$fold <- sample(1:5, nrow(df1), replace=T)
train <- filter(df1, fold != 1)
test <- filter(df1, fold == 1)

# 2.Do this within a for-loop over folds, and keep track of the mean-squared error on the test data in each iteration
set.seed(17)
cv.error.5 = rep(0,5)
for (i in 1:5) {
  train <- filter(df1, fold != i)
  test <- filter(df1, fold == i)
  lm.fit = lm(num_trips ~ poly(tmax,10) + is_holiday*prcp + snow + is_week_end, data = train) 
  test$predicted <- predict(lm.fit,test)
  RMSE[i] = sqrt(mean((test$num_trips - test$predicted)^2))
  Ave = mean(RMSE)
  }
RMSE
Ave
#3629.306
sd (RMSE)
#377.8253
std.error(RMSE)
#168.9686

# 3.Then compute the average of the five mean-squared errors that you get and the standard error on that average
Ave
#3629.306
sd (RMSE)
#377.8253
std.error(RMSE)
#168.9686
