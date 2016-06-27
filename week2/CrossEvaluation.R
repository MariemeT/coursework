library(readr)
library(ggplot2)
library(scales)
library(broom)
library(dplyr)

#################################In this assignment we'll predict number of trips per day as a function of the weather on that day.##############################################

#Create a data frame with one row for each day, the number of trips taken on that day, and the minimum temperature on that day.
trips_with_weather <- inner_join(trips, weather, 'ymd')
df <- trips_with_weather %>% group_by(ymd, tmin) %>% summarize(numtrips = n())

#Split the data into a randomly selected training and test set, as in the above exercise, with 80% of the data for training the model and 20% for testing.
indexes <- sample(1:nrow(df), size=0.2*nrow(df))
DFtest=df[indexes, ] 
Dftrain=df[-indexes, ]

View(df)

#Fit a model to predict the number of trips as a (linear) function of the minimum temperature, and evaluate the fit on the training and testing data sets.
model <- lm(numtrips~tmin,DFtest )
DFtest$predicted <- fitted(model)

model1 <- lm(numtrips~tmin, Dftrain)
Dftrain$predicted <- fitted(model1)

#Do this first visually by plotting the predicted and actual values as a function of the minimum temperature. Then do this with R^2, as above. 
#plot of the test file
ggplot(DFtest, aes(x=tmin, y=numtrips)) +
  geom_point(alpha=0.1) +
  geom_line(aes(x=tmin, y=predicted)) +
  xlab('minimum temperature') +
  ylab('number of trips')

#plot of the train file
ggplot(Dftrain, aes(x=tmin, y=numtrips)) +
  geom_point(alpha=0.1) +
  geom_line(aes(x=tmin, y=predicted)) +
  xlab('minimum temperature') +
  ylab('number of trips')
#You'll want to use the predict and cor functions for this.

#Repeat this procedure, but add a quadratic term to your model (e.g., + tmin^2, or equivalently + poly(k,2)). 
model3 <- lm(numtrips~poly(tmin,2), DFtest)
DFtest$predicted <- fitted(fit3)
model4 <- lm(numtrips~poly(tmin,2), Dftrain)
Dftrain$predicted <- fitted(model4)

ggplot(DFtest, aes(x=tmin, y=numtrips)) +
  geom_point(alpha=0.1) +
  geom_line(aes(x=tmin, y=predicted)) +
  xlab('minimum temperature') +
  ylab('number of trips')

ggplot(Dftrain, aes(x=tmin, y=numtrips)) +
  geom_point(alpha=0.1) +
  geom_line(aes(x=tmin, y=predicted)) +
  xlab('minimum temperature') +
  ylab('number of trips')


#How does the model change, and how do the fits between the linear and quadratic models compare?
#With a polynomial function of degree2, we can see that our prediction is more accurate because we obtained higher R^2 values.

#Now automate this, extending the model to higher-order polynomials with a for loop over the degree k. 

train_cor = c()
test_cor = c()
for (i in 1:20){
  lm.fit1 <- lm(numtrips ~ poly(tmin,i), Dftrain)
  
  
  
  
  DFtest$predicted <- predict(lm.fit1, DFtest)
  Dftrain$predicted <- predict(lm.fit1, Dftrain)
  
  train_cor[i] <- cor(Dftrain$predicted,Dftrain$numtrips)^2
  test_cor[i] <- cor(DFtest$predicted, DFtest$numtrips)^2
  
}

ggplot() + geom_line(aes(x = X,y = train_cor), color = "blue") + geom_line(aes(x = X,y = test_cor), color = "green")

#For each value of k, fit a model to the training data and save the R^2 on the training data to one vector and test vector to another. 


#Then plot the training and test R^2 as a function of k. What value of k has the best performance?

#Finally, fit one model for the value of k with the best performance in 6), and plot the actual and predicted values for this model.
