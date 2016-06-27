library(readr)
library(ggplot2)
library(scales)
library(broom)
library(dplyr)

#More sales data

# 1.Let’s return to the orange juice assignment and investigate how store demographics are related to demand.

#    i.Let’s start with the following model: logmove ~ log(price)*brand*feat and add in the store demographics as linear features (e.g., + AGE60 + EDUC + ETHNIC + INCOME). Try them individually and then all together.
model1 <- lm(logmove ~ log(price)*brand*feat + AGE60 + EDUC + ETHNIC + INCOME + HHLARGE + WORKWOM + HVAL150 + SSTRDIST + SSTRVOL + CPDIST5 + CPWVOL5, oj)

#    ii.What demographics are significantly (t > 2 standard deviations) related to demand?
summary(model1)
#Based on our computations, all demographics are significant related to demand because their t values are greater than 2.

#    iii.How much did the adjusted R-squared improve with the addition of these variables?
model <- lm(logmove ~ log(price)*brand*feat)
summary(model)
summary(model1)
#R^2(with all the demographics) = 0.5848
#R^2(without all demographics) = 0.5354

# 2.Let’s focus on two variables HHLARGE ("fraction of households that are large") and EDUC ("fraction of shoppers with advanced education").

#    i.What are the means and percentiles of each of these variables?
mean(oj$HHLARGE)
#[1] 0.1156024
quantile(oj$HHLARGE)
#0%        25%        50%        75%       100% 
#0.01350636 0.09793763 0.11122120 0.13516767 0.21635434 
mean(oj$EDUC)
#[1] 0.2252196
quantile(oj$EDUC)
#0%        25%        50%        75%       100% 
#0.04955029 0.14598491 0.22939040 0.28439465 0.52836201 

#    ii.Using your coefficient estimates from the regression in 1b:

#       a.If we move from the median value of HHLARGE to the 75th percentile (3rd quartile), how much does logmove change each week on average? You can estimate this visually if you plot the fitted model, or you can compare the predicted values for rows that have the median and 75th percentiles for HHLARGE.
oj$predicted = fitted(model2)
median_75_HHLARGE_oj = oj %>% filter(HHLARGE == quantile(oj$HHLARGE, 0.75) | HHLARGE == median(oj$HHLARGE))
ggplot(median_75_HHLARGE_oj, aes(x=HHLARGE, y=predicted)) + geom_smooth(method="lm")

#       b.If we move from the median value of EDUC to the 75th percentile (3rd quartile), how much does logmove change each week on average?
median_75_EDUC_oj = oj %>% filter(EDUC == quantile(oj$EDUC, 0.75) | EDUC == median(oj$EDUC))
ggplot(median_75_EDUC_oj, aes(x=EDUC, y=predicted)) + geom_smooth(method="lm")

#       c.Based on this analysis, which is the more important predictor of demand?
#Based on our computations, the EDUC has a greater effect.

#    iii.Now let’s see if these variables impact price sensitivity. Add two interaction terms (with logprice) to the model to test this.

#       a.What are the coefficients on the interaction terms?
model3 <- lm(logmove ~ log(price)*AGE60*WORKWOM + EDUC + HHLARGE, oj)
coef(model3)
#(Intercept)               log(price)                    AGE60                  WORKWOM                     EDUC                  HHLARGE         log(price):AGE60 
#12.7326228               -3.7018950                6.6844737               -4.6150613                0.6267225               -2.6141439               -4.9151422 
#log(price):WORKWOM            AGE60:WORKWOM log(price):AGE60:WORKWOM 
#3.9341438              -27.1234014               25.2081128 

#       b.Recall, positive values indicate lower price sensitivity and negative values indicate greater price sensitivity. Do your estimates make sense based on your intuition?
#Yes, these coefficients make sense because it shows that the price, large households, and working women are important factors.

#       c.What are the coefficient estimates on the constants EDUC and HHLARGE? How do they compare to your regression from 1b?
#Coef(EDUC) = 0.6267, coef(HHLARGE) = -2.6141
#In 1.ii, coef(EDUC) = 0.9559, coef(HHLARGE) = -0.92009

#       d.Similar to 2b, if we move from the median value of each variable to the 3rd quartile, how much does elasticity change? Based on this, which is more important to price sensitivity?
oj$predicted <- fitted(model3)
median_75_log(price) = oj %>% filter(log(price) == quantile(oj$log(price), 0.75)) | log(price) == median(oj$log(price))
median_75_AGE60_oj = oj %>% filter(AGE60 == quantile(oj$AGE60, 0.75)) | AGE60 == median(oj$AGE60)
median_75_WORKWOM_oj = oj %>% filter(WORKWOM == quantile(oj$WORKWOM, 0.75) | WORKWOM == median(oj$WORMWOM))
median_75_EDUC_oj = oj %>% filter(EDUC == quantile(oj$EDUC, 0.75) | EDUC == median(oj$EDUC))
median_75_HHLARGE_oj = oj %>% filter(HHLARGE == quantile(oj$HHLARGE, 0.75) | HHLARGE == median(oj$HHLARGE))
#    iv.You should notice that the coefficients on EDUC and HHLARGE have flipped sign once we include interaction terms with price. HHLARGE now appears to be a positive demand shifter and increases price sensitivity. Explain in words or pictures what is going on.

# 3.Let’s split our data into a training set and a test set. An easy way to do this is with the sample command. The following will randomly select 20% of the rows in our data frame: indexes <- sample(1:nrow(oj), size=0.2*nrow(oj))
indexes <- sample(1:nrow(oj), size=0.2*nrow(oj))

#    i.Now let’s use this index to create a training and a test set, try: OJtest=oj[index, ] and Ojtrain=oj[-index, ]. What did this do? How many rows does the test set have? How many rows does the training set have?
OJtest=oj[indexes, ]
Ojtrain=oj[-indexes, ]
#This splits the data into two separate data frames. Indeed, OJtest has 20% of the the observations while Ojtrain has 80% of the total observations

# 4.Now let’s run the very simple model logmove ~ log(price) + brand on the training data.
model4 <-lm(logmove ~ log(price) + brand, oj)

#    i.Use LM on this model and report the R-squared.
summary(model4)
#R^2 = 0.394

#    ii.Use predict(model, Ojtest) to predict log sales for the test set.
model5 <- lm(logmove ~ log(price) + brand, OJtest)
OJtest$predicted <- predict(model5)

#    iii.Compute cor(predicted_sales,logmove)^2 on the test set. This is our "honest R-squared". How does it compare to the value in (a)?
cor(OJtest$predicted, OJtest$logmove)^2
#[1] 0.3877666

# 5.Now let’s run better models.

#    i.Run our "previous favorite" logmove ~ brand*log(price)*feat on the training data. Use LM to get regular R-squared. Now, follow the procedure in (3) to compute "honest R-squared". What is it? How do they compare?
model <- lm(logmove ~ log(price)*brand*feat)
summary(model)
#R^2 = 0.5352

model6 <- lm(logmove ~ log(price)* brand*feat, OJtest)
OJtest$predicted <- predict(model6)
cor(OJtest$predicted, OJtest$logmove)^2
#R^2 = 0.5230372

#    ii.Now add in all the demographics. What is the regular R-squared on training data? What is the honest R-squared on the test set?
model1 <- lm(logmove ~ log(price)*brand*feat + AGE60 + EDUC + ETHNIC + INCOME + HHLARGE + WORKWOM + HVAL150 + SSTRDIST + SSTRVOL + CPDIST5 + CPWVOL5, oj)
summary(model1)
#R^2 = 0.5848
model7 <- lm(logmove ~ log(price)* brand*feat + AGE60 + EDUC + ETHNIC + INCOME + HHLARGE + WORKWOM + HVAL150 + SSTRDIST + SSTRVOL + CPDIST5 + CPWVOL5, OJtest)
OJtest$predicted <- predict(model7)
cor(OJtest$predicted, OJtest$logmove)^2
#R^2(honest) = 0.5703