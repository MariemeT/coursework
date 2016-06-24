#Visualizing price.

#Make a plot of the distribution of prices. 
ggplot(oj,aes(price)) + geom_histogram() +geom_vline(xintercept=mean(oj$price), linetype=2, color="red")

#Change the x-axis on this plot to use a logarithmic scale using scale_x_log10().
ggplot(oj,aes(price)) + geom_histogram() + scale_x_log10() +geom_vline(xintercept=mean(oj$price), linetype=2, color="red")

#Repeat i), faceted by brand.
dom <- oj %>% filter(brand == "dominicks")
ggplot(dom,aes(price)) + geom_histogram() +geom_vline(xintercept=mean(dom$price), linetype=2, color="red")

min <- oj %>% filter(brand == "minute.maid")
ggplot(min,aes(price)) + geom_histogram() +geom_vline(xintercept=mean(min$price), linetype=2, color="red")

trop<- oj %>% filter(brand == "tropicana")
ggplot(trop,aes(price)) + geom_histogram() +geom_vline(xintercept=mean(trop$price), linetype=2, color="red")

ggplot() + geom_histogram(mapping = aes(dom$price), fill = "red") + geom_histogram(mapping = aes(min$price), fill = "yellow") + geom_histogram(mapping = aes(trop$price), fill = "orange")

#Repeat ii), faceted by brand.
#same as above

#What do these graphs tell you about the variation in price? Why do the log plots look different? Do you find them more/less informative?

#Visualizing the quantity/price relationship.
#Plot logmove (the log of quantity sold) vs. log price.
ggplot(oj, aes(logmove, price)) + geom_point()

#Color each point by brand. What do insights can you derive that were not apparent before?
ggplot(oj, aes(logmove, price, color = brand)) + geom_point()

#Estimating the relationship.

#Do a regression of logmove on log price. How well does the model fit? What is the elasticity (the coefficient on log price), and does it make sense? See here for some background on elasticity.
lm.log = lm(logmove~log(price), oj)
summary(lm.log)
#R^2 = 0.2081, therefore the model does not fit well
#The coefficient on log price is -1.601307 and it makes sense because it follows the law of demand

#Now add in an intercept term for each brand (by adding brand to the regression formula). How do the results change? How should we interpret these coefficients?
lm.log1 = lm(logmove~log(price) + brand, oj) 
 summary(lm.log1)
 #R^2 = 0.3941, therefore the model is fitting well (better than previous result)
 #The coefficient is -3.13 and it still makes sense

#Now add interaction terms to allow the elasticities to differ by brand, by including a brand:log price term in the regression formula. Note the estimate coefficients will "offset" the base estimates. What is the insights we get from this regression? What is the elasticity for each firm? Do the elasticities make sense?
 lm.log2 = lm(logmove~log(price) * brand, oj) 
 summary(lm.log2)
 #R^2 = 0.3978, the model is getting better
 #The coefficient = -3.377
 
#Impact of "featuring in store".
#Which brand is featured the most? Make a plot to show this.
ft <- oj %>% filter(feat == 1) %>% group_by(brand) %>% summarize(count = n())
#Based on that computation, minute.maid has the most features
ggplot(ft, aes(brand, count)) + geom_point()

#How should we incorporate the "featured in store" variable into our regression? Start with an additive formulation (e.g. feature impacts sales, but not through price).
model <- lm(logmove~ log(price) + as.factor(feat), oj) 
ojf$predicted <- fitted(model)
ggplot(oj, aes(log(price), logmove, color = as.factor(feat))) + geom_point() + geom_line(aes(log(price), predicted, color = as.factor(feat)))

#Now run a model where features can impact sales and price sensitivity.


#Now run a model where each brand can have a different impact of being featured and a different impact on price sensitivity. Produce a table of elasticties for each brand, one row for "featured" and one row for "not featured" (you need 6 estimates).
levels(oj$brand)
par(mfrow=c(1,2))
plot(log(price) ~ brand, data=oj, col=brandcol)
plot(logmove ~ log(price), data=oj, col=brandcol[oj$brand])