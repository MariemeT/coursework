library(readr)
library(ggplot2)
library(scales)
library(broom)
library(dplyr)

#More sales data
#Let’s return to the orange juice assignment and investigate how store demographics are related to demand.
#Let’s start with the following model: logmove ~ log(price)*brand*feat and add in the store demographics as linear features (e.g., + AGE60 + EDUC + ETHNIC + INCOME). Try them individually and then all together.
model <- lm(logmove~log(price)*brand*feat + AGE60 + EDUC + ETHNIC + INCOME + HHLARGE + HVAL150+SSTRDIST + SSTRVOL + CPDIST5 + CPWVOL5, oj)

#What demographics are significantly (t > 2 standard deviations) related to demand?
summary(model)
#Based on our computations, all the demographics have a significant effects, some more then others.

#How much did the adjusted R-squared improve with the addition of these variables?
#R^2(with variables) = 0.585, R^2(without variables) = 0.5354

#Let’s focus on two variables HHLARGE ("fraction of households that are large") and EDUC ("fraction of shoppers with advanced education").
#What are the means and percentiles of each of these variables?
#Using your coefficient estimates from the regression in 1b:
#If we move from the median value of HHLARGE to the 75th percentile (3rd quartile), how much does logmove change each week on average? You can estimate this visually if you plot the fitted model, or you can compare the predicted values for rows that have the median and 75th percentiles for HHLARGE.
#If we move from the median value of EDUC to the 75th percentile (3rd quartile), how much does logmove change each week on average?
model3 <- lm(logmove~log(price)*brand*feat + EDUC + HHLARGE, oj)
oj$predicted <- fitted(model3)
median_75_oj = oj %>% filter(HHLARGE == quantile(oj$HHLARGE, 0.75) | HHLARGE == median(oj$HHLARGE))
ggplot(median_75_oj, aes(x=HHLARGE, y=predicted)) + geom_smooth(method="lm")
ggplot(median_75_oj, aes(x=EDUC, y=predicted)) + geom_smooth(method="lm")
mean(oj$HHLARGE)
#0.115
mean(oj$EDUC)
#0.225
quantile(oj$HHLARGE)
quantile(oj$EDUC)

#Based on this analysis, which is the more important predictor of demand?
#Based on our computations, the EDUC has a greater effect.

#Now let’s see if these variables impact price sensitivity. Add two interaction terms (with logprice) to the model to test this.
#What are the coefficients on the interaction terms?
model5 <- lm(logmove~log(price)* AGE60*EDUC, oj)
coef(model5)
#(Intercept)            log(price)                 AGE60                  EDUC      log(price):AGE60       log(price):EDUC            AGE60:EDUC log(price):AGE60:EDUC 
#10.561798             -2.755209              2.880888             -1.031788              1.024564              3.599862            -10.453520              3.685910 

#Recall, positive values indicate lower price sensitivity and negative values indicate greater price sensitivity. Do your estimates make sense based on your intuition?
#Yes, because, this means that age and educations has a greater sensitivity to the price.

#What are the coefficient estimates on the constants EDUC and HHLARGE? How do they compare to your regression from 1b?
#The coefficients estimates are: -0.698 and 4.66019

#Similar to 2b, if we move from the median value of each variable to the 3rd quartile, how much does elasticity change?
#Based on this, which is more important to price sensitivity?
oj$predicted <- fitted(model5)
median_75_oj = oj %>% filter(AGE60 == quantile(ojAGE60, 0.75) | AGE60 == median(oj$AGE60))
ggplot(median_75_oj, aes(x=AGE60, y=predicted)) + geom_smooth(method="lm")
ggplot(median_75_oj, aes(x=EDUC, y=predicted)) + geom_smooth(method="lm")

#You should notice that the coefficients on EDUC and HHLARGE have flipped sign once we include interaction terms with price. HHLARGE now appears to be a positive demand shifter and increases price sensitivity. Explain in words or pictures what is going on.

#Part3
#Let’s split our data into a training set and a test set. An easy way to do this is with the sample command. 
#The following will randomly select 20% of the rows in our data frame: indexes <- sample(1:nrow(oj), size=0.2*nrow(oj))
#Now let’s use this index to create a training and a test set, try: OJtest=oj[index, ] and Ojtrain=oj[-index, ]. 
#What did this do? How many rows does the test set have? How many rows does the training set have?
#The ojtest and the ojtrain have 5789 rows each.

#Part4
#Now let’s run the very simple model logmove ~ log(price) + brand on the training data.
model7<- lm(logmove ~ log(price) + brand, ojtrain)
summary(model7)

#Use LM on this model and report the R-squared.
#R^2 = 0.411

#Use predict(model, Ojtest) to predict log sales for the test set.
model8<- lm(logmove ~ log(price) + brand, ojtest)
summary(model8)
ojtest$predicted <- fitted(model8)

#Compute cor(predicted_sales,logmove)^2 on the test set. This is our "honest R-squared". How does it compare to the value in (a)?
cor(ojtest$predicted, ojtest$logmove)^2
# 0.4109533

#Part 5
#Now let’s run better models.
#Run our "previous favorite" logmove ~ brand*log(price)*feat on the training data. Use LM to get regular R-squared. 
#Now, follow the procedure in (3) to compute "honest R-squared". What is it? How do they compare?
model1 <- lm(logmove~brand*log(price)*feat, oj)
summary(model1)
#R^2 = 0.5354
model9 <- lm(logmove~brand*log(price)*feat,ojtest)
ojtest$predicted <- fitted(model9)
cor(ojtest$predicted, ojtest$logmove)^2
#honest R-squared = 0.5549

#Now add in all the demographics. What is the regular R-squared on training data? What is the honest R-squared on the test set?
summary(model9)
#R^2 = 0.585
model10 <- lm(logmove~log(price)*brand*feat + AGE60 + EDUC + ETHNIC + INCOME + HHLARGE + HVAL150+SSTRDIST + SSTRVOL + CPDIST5 + CPWVOL5, ojtest)
ojtest$predicted <- fitted(model10)
cor(ojtest$predicted, ojtest$logmove)^2
#honest R-squared = 0.605