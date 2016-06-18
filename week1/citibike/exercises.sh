#!/bin/bash
#
# add your solution after each of the 10 comments below
#

# count the number of unique stations

tail -n+2 201402-citibike-tripdata.csv | cut -d, -f5,9 | tr , '\n' | sort | uniq | wc -l
cut -d, -f5 201402-citibike-tripdata.csv | tail -n+2 | sort | uniq | wc -l

# count the number of unique bikes
tail -n+2 201402-citibike-tripdata.csv | cut -d, -f12 | tr , '\n' | sort | uniq | wc -l

# extract all of the trip start times
tail -n+2 201402-citibike-tripdata.csv | cut -d, -f2 | sort | uniq | wc -l

# count the number of trips per day
tail -n+2 201402-citibike-tripdata.csv | cut -d, -f2 | sort | uniq | cut -d ' '  -f1 | uniq -c

# find the day with the most rides
tail -n+2 201402-citibike-tripdata.csv | cut -d, -f2 | sort | cut -d ' '  -f1 | uniq -c | sort -nr | sed -n '1p'| cut -d'"' -f2

# find the day with the fewest rides
tail -n+2 201402-citibike-tripdata.csv | cut -d, -f2 | sort | cut -d ' '  -f1 | uniq -c | sort -n | sed -n '1p'| cut -d'"' -f2
# find the id of the bike with the most rides
tail -n+2 201402-citibike-tripdata.csv | cut -d, -f12 | tr , '\n' | sort | uniq -c | sort -nr | sed -n '1p'| cut -d\" -f2

# count the number of riders by gender and birth year
tail -n+2 201402-citibike-tripdata.csv | cut -d, -f14,15 | tr , '\n' | sort | uniq -c
cut -d, -f14,15 201402-citibike-tripdata.csv | sort |  uniq -c
cut -d, -f14,15 201402-citibike-tripdata.csv | sort |  uniq -c | sort -t, -k2


# count the number of trips that start on cross streets that both contain numbers (e.g., "1 Ave & E 15 St", "E 39 St & 2 Ave", ...)
tail -n+2 201402-citibike-tripdata.csv | cut -d, -f5 | sort | tr -d '\"'| grep '[0-9].*&.*[0-9]'| wc -l
# compute the average trip duration
cut -d, -f1 201402-citibike-tripdata.csv | tr -d '"' | awk '{ count[1++]=$1} END { for{1 in count) result += count[i]; print (result/(i+1));}'



