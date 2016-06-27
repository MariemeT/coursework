library(readr)
library(ggplot2)
library(scales)
library(broom)
library(dplyr)

# Cross-validation for Citibike trips

# 1.Create a data frame with one row for each day, the number of trips taken on that day, and the minimum temperature on that day.
df <- trips_with_weather %>% group_by(ymd, tmin) %>% summarize(num_trips = n())

# 2.Split the data into a randomly selected training and test set, as in the above exercise, with 80% of the data for training the model and 20% for testing.
indexes <- sample(1:nrow(df), size=0.2*nrow(df))
TWtest=df[indexes, ]
TWtrain=df[-indexes, ]

# 3.Fit a model to predict the number of trips as a (linear) function of the minimum temperature, and evaluate the fit on the training and testing data sets. Do this first visually by plotting the predicted and actual values as a function of the minimum temperature. Then do this with R^2, as above. You'll want to use the predict and cor functions for this.
model1 <- lm(num_trips ~ tmin, df)
df$predicted = predict(model1)
ggplot(df, aes(x = tmin, y = num_trips)) + geom_point() + geom_line(aes(x = tmin, y = predicted))

model2 <- lm(num_trips ~ tmin, TWtrain)
TWtrain$predicted <- predict(model2)
cor(TWtrain$predicted, TWtrain$num_trips)^2
#R^2(honest) = 0.65548
summary(model2) 
# R^2 = 0.6543

# 4.Repeat this procedure, but add a quadratic term to your model (e.g., + tmin^2, or equivalently + poly(k,2)). How does the model change, and how do the fits between the linear and quadratic models compare?

model3 <- lm(num_trips ~ tmin + poly(tmin,2), df)
df$predicted = predict(model3)

model4 <- lm(num_trips ~ tmin + poly(tmin,2), TWtrain)
TWtrain$predicted <- predict(model4)
cor(TWtrain$predicted, TWtrain$num_trips)^2

#R^2(honest) = 6.5589
summary(model4)
#R^2 = 0.6535
#The quadretic model fits better than the linear model(but just slightly)

#5.Now automate this, extending the model to higher-order polynomials with a for loop over the degree k. For each value of k, fit a model to the training data and save the R^2 on the training data to one vector and test vector to another. Then plot the training and test R^2 as a function of k. What value of k has the best performance?
X <- c(1:20)
train_cor = c()
test_cor = c()
for (i in 1:20){
  lm.fit1 <- lm(num_trips ~ poly(tmin,i), TWtrain)
  
  
  
  
  TWtest$predicted <- predict(lm.fit1, TWtest)
  TWtrain$predicted <- predict(lm.fit1, TWtrain)
  
  train_cor[i] <- cor(TWtrain$predicted,TWtrain$num_trips)^2
  test_cor[i] <- cor(TWtest$predicted, TWtest$num_trips)^2
  

}
ggplot() + geom_line(mapping = aes(x = X,y = train_cor, color = "blue")) + geom_line(mapping = aes(x = X,y = test_cor, color = "green"))
# It seems that k = 3 has the best performance

# 6.Finally, fit one model for the value of k with the best performance in 6), and plot the actual and predicted values for this model.
train_cor1 = c()
test_cor1 = c()
for (i in 1:3){
  model5 <- lm(num_trips~ poly(tmin,i) ,TWtrain)
  
  
  TWtest$predicted <- predict(model5, TWtest)
  TWtrain$predicted <- predict(model5, TWtrain)
  
  train_cor1[i] <- cor(TWtrain$predicted,TWtrain$num_trips)^2
  test_cor1[i] <- cor(TWtest$predicted, TWtest$num_trips)^2
  
  
}
ggplot() + geom_line(mapping = aes(x = S,y = train_cor1, color = "blue")) + geom_line(mapping = aes(x = S,y = test_cor1, color = "green"))
