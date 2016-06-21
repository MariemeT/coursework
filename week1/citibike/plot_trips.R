########################################
# load libraries
########################################

# load some packages that we'll need
library(dplyr)
library(ggplot2)
library(reshape)
<<<<<<< HEAD
=======
library(scales)
library(tidyr)
library(lubridate)
>>>>>>> e48b810058836086405754c853ab753ba764126e

# be picky about white backgrounds on our plots
theme_set(theme_bw())

# load RData file output by load_trips.R
load('trips.RData')


########################################
# plot trip data
########################################

# plot the distribution of trip times across all rides
ggplot(trips, aes(tripduration)) + geom_histogram() + xlim(0, 3600*3)
# plot the distribution of trip times by rider type
ggplot(trips, aes(tripduration, color = usertype)) + geom_density() + xlim(0, 3600*3)
# plot the number of trips over each day
ggplot(trips, aes(x = ymd)) + geom_bar()
# plot the number of trips by gender and age
<<<<<<< HEAD
trips_group <- trips %>% group_by(gender, birth_year) %>% summarise(count =n())
ggplot(trips_group, aes(x = birth_year, y = count, color = as.factor(gender))) + geom_point() + xlim(1950, 2000)
=======

# plot the ratio of male to female trips by age
# hint: use the spread() function to reshape things to make it easier to compute this ratio

########################################
# plot weather data
########################################
# plot the minimum temperature over each day

# plot the minimum temperature and maximum temperature over each day
# hint: try using the gather() function for this to reshape things before plotting

>>>>>>> e48b810058836086405754c853ab753ba764126e
########################################
# plot trip and weather data
########################################

# join trips and weather
trips_with_weather <- inner_join(trips, weather, by="ymd")
<<<<<<< HEAD
# plot the minimum temperature over each day
 ggplot(weather, aes(ymd,tmin)) + geom_point()+geom_smooth()
=======

>>>>>>> e48b810058836086405754c853ab753ba764126e
# plot the number of trips as a function of the minimum temperature, where each point represents a day
# you'll need to summarize the trips and join to the weather data to do this

# repeat this, splitting results by whether there was substantial precipitation or not
# you'll need to decide what constitutes "substantial precipitation" and create a new T/F column to indicate this

# add a smoothed fit on top of the previous plot, using geom_smooth

# compute the average number of trips and standard deviation in number of trips by hour of the day
# hint: use the hour() function from the lubridate package

# plot the above

# repeat this, but now split the results by day of the week (Monday, Tuesday, ...) or weekday vs. weekend days
# hint: use the wday() function from the lubridate package
