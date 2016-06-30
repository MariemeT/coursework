library(readr)
library(ggplot2)
library(scales)
library(broom)
library(dplyr)
library(lubridate)
library(ISLR)
library(glmnet)
library(plotrix)

#extracting weather_2015 files
weather_2015 <- read.table('weather_2015.csv', header=T, sep=',')

# Add a new colunm for ymd data
weather_2015 <- select(weather_2015, date, prcp, snwd, snow, tmax, tmin)
names(weather_2015) <- tolower(names(weather_2015))
weather_2015 <- mutate(weather_2015,
                       tmin = tmin / 10,
                       tmax = tmax / 10,
                       ymd = as.Date(parse_datetime(date, "%Y%m%d")))
weather_2015 <- tbl_df(weather_2015)

# Add new columns for num_trips, is_week_end, and is_holiday
df1 <- trips_with_weather %>% group_by(ymd, tmin,tmax,snow,snwd,prcp) %>% summarize(num_trips = n())
weather_2015 <- weather_2015 %>% mutate(days_of_the_week = wday(ymd,label = TRUE)) # Added the days of the week

holidays <- as.Date(c("2015-01-01", "2015-01-18", "2015-02-15","2015-05-30"))
weather_2015<-mutate(weather_2015, is_holiday = ymd %in% holidays) # Added is_holiday

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
weather_2015$is_week_end = is_weekend(weather_2015$ymd) # Added is_week_end