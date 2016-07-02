library(readr)
library(ggplot2)
library(scales)
library(broom)
library(dplyr)
library(lubridate)
library(ISLR)
library(glmnet)
library(plotrix)

weather_2015 <- read.table('weather_2015.csv', header=T, sep=',')

# Add a new colunm for ymd data
weather_2015 <- select(weather_2015, DATE, PRCP, SNWD, SNOW, TMAX, TMIN)
names(weather_2015) <- tolower(names(weather_2015))
weather_2015 <- mutate(weather_2015,
                  tmin = tmin / 10,
                  tmax = tmax / 10,
                  ymd = as.Date(parse_datetime(date, "%Y%m%d")))
weather_2015 <- tbl_df(weather_2015)

# Add new columns for num_trips, is_week_end, and is_holiday
df2 <- trips_with_weather1 %>% group_by(ymd, tmin, tmax, snow, snwd, prcp) %>% summarize(Num_Trips = n())
df2 <- df2 %>% mutate(days_of_the_week = wday(ymd,label = TRUE)) # Added the days of the week

holidays <- as.Date(c("2015-01-01", "2015-01-18", "2015-02-15","2015-05-30"))
df2<-mutate(df2, is_holiday = ymd %in% holidays) # Added is_holiday

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
df2$is_week_end = is_weekend(df2$ymd) # Added is_week_end

# Predicting the number of trips
model4 <- lm(num_trips ~ days_of_the_week + poly(tmax,4) + is_holiday + prcp:is_week_end, df1)
df2$Predicted <- predict(model4, df2)

# Plotting the predicted num_of_trips vs actual_num_trips
ggplot(df2,aes( x = Predicted, y = Num_Trips)) + geom_point()
ggplot(df2) + geom_point(aes(x = ymd, y = Num_Trips, color = is_week_end)) + geom_point(aes(x = ymd, y = Predicted, color = is_week_end))

#Computing the RMSE
RMSE1 <- sqrt(mean((df2$Num_Trips-df2$Predicted)^2))
RMSE1
#4097.53
cor(df2$Predicted, df2$Num_Trips)^2
#0.872