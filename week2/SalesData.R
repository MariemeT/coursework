library(readr)
library(ggplot2)
library(scales)
library(broom)
library(dplyr)

# 1.Load the orange juice data. See here for a description of the columns.

# 2.Visualizing price.

#    i.Make a plot of the distribution of prices.
ggplot(oj, aes(x = price)) + geom_histogram()

#    ii.Change the x-axis on this plot to use a logarithmic scale using scale_x_log10().
ggplot(oj, aes(x = log(price))) + geom_histogram()

#    iii.Repeat i), faceted by brand.
ggplot(oj, aes(x = price)) + geom_histogram() + facet_wrap(~brand)

#    iv.Repeat ii), faceted by brand.
ggplot(oj, aes(x = log(price))) + geom_histogram() + facet_wrap(~brand)

#    v.What do these graphs tell you about the variation in price? Why do the log plots look different? Do you find them more/less informative?


# 3.Visualizing the quantity/price relationship.

#    1.Plot logmove (the log of quantity sold) vs. log price.
ggplot(oj, aes(x = log(price), y = logmove)) + geom_point()

#    ii.Color each point by brand. What do insights can you derive that were not apparent before?
ggplot(data = oj) + geom_point(mapping = aes(x = log(price), y = logmove, color = brand))

# 4.Estimating the relationship.

#    i.Do a regression of logmove on log price. How well does the model fit? What is the elasticity (the coefficient on log price), and does it make sense? See here for some background on elasticity and below for a tip on plotting the fitted model. Also, see here for more on log-log transformations in regression.
model <- lm(logmove~log(price), oj) # Regression og logmove on log price
summary(model)
# R^2 = 0.2801, therefore the model does not fit well
#The coefficient of log price is -1.601307
#Yes, it makes sense because as the price of a good goes up, people tend to buy less that good. There it makes sense to observe a negative slope.

#    ii.Now add in an intercept term for each brand (by adding brand to the regression formula). How do the results change? How should we interpret these coefficients?
model1 <- lm(logmove~log(price) + brand, oj)
summary(model1)
#R^2 = 0.394, therefore the model is getting slightly better
#The coefficient of log price is -3.1386914, we can see that the slope is more negative.

#    iii.Now add interaction terms to allow the elasticities to differ by brand, by including a brand:log price term in the regression formula. Note the estimate coefficients will "offset" the base estimates. What is the insights we get from this regression? What is the elasticity for each firm? Do the elasticities make sense?
model2 <- lm(logmove~log(price)*brand, oj)
#Coef(log price) = -3.3775, coef(brandminute.maid) = 0.8882, coef(brandtropicana) = 0.9623, coef(log price:brandminute.maid) = 0.05679, coef(log price:brandtropicana) = 0.6657, coef(branddominicks) = 10.95468, 

# 5.Impact of "featuring in store".

#    i.Which brand is featured the most? Make a plot to show this.
df <- oj %>% group_by(brand) %>% filter(feat ==1) %>% summarize(count = n())
ggplot(df, aes(x = brand, y = count)) + geom_point()
#Based on our results, minute maid is the most featured brand

#    ii.How should we incorporate the "featured in store" variable into our regression? Start with an additive formulation (e.g. feature impacts sales, but not through price).
model3 <- lm(logmove~feat, oj)
summary(model3)
#R^2 = 0.2876, coef(feat = 1.285), therefore the featured in store variable has a positive impact on sales.

#    iii.Now run a model where features can impact sales and price sensitivity.
model4 <- lm(logmove~log(price) + feat, oj)
oj$predicted <-fitted(model4)
ggplot(oj, aes(x = log(price), y = logmove, color = as.factor(feat))) + geom_point() + geom_line(aes(x = log(price), y = predicted, color = as.factor(feat)))

#    iv.Now run a model where each brand can have a different impact of being featured and a different impact on price sensitivity. Produce a table of elasticties for each brand, one row for "featured" and one row for "not featured" (you need 6 estimates).
model5 <- lm(logmove~brand*feat*log(price) -1 , oj)
#Coef(brandminute.maid) = 0.047, coef(brandminute.maid:feat) = 1.1729
#Coef(brandtropicana) = 0.7079, coef(brandtropicana:feat) = 0.7852
#Coef(branddominicks)  = 10.406, coef(branddominicks:feat) = 
