#Load required packages for working with data
library(tidyverse)
library(dplyr)
library(readr)
library(readxl)


#Import trip data
jan_2023 <- read_excel("jan_2023.xlsx")
feb_2023 <- read_excel("feb_2023.xlsx")
mar_2023 <- read_excel("mar_2023.xlsx")
apr_2023 <- read_excel("apr_2023.xlsx")
may_2023 <- read_excel("may_2023.xlsx")
jun_2023 <- read_excel("jun_2023.xlsx")
jul_2023 <- read_excel("jul_2023.xlsx")
aug_2023 <- read_excel("aug_2023.xlsx")
sep_2023 <- read_excel("sep_2023.xlsx")
oct_2023 <- read_excel("oct_2023.xlsx")
nov_2023 <- read_excel("nov_2023.xlsx")
dec_2023 <- read_excel("dec_2023.xlsx")


#Confirmation
glimpse(jan_2023)
glimpse(feb_2023)
glimpse(mar_2023)
glimpse(apr_2023)
glimpse(may_2023)
glimpse(jun_2023)
glimpse(jul_2023)
glimpse(aug_2023)
glimpse(sep_2023)
glimpse(oct_2023)
glimpse(nov_2023)
glimpse(dec_2023)

#Merge data
trip_data <- jan_2023 %>%
  bind_rows(feb_2023) %>%
  bind_rows(mar_2023) %>%
  bind_rows(apr_2023) %>%
  bind_rows(may_2023) %>%
  bind_rows(jun_2023) %>%
  bind_rows(jul_2023) %>%
  bind_rows(aug_2023) %>%
  bind_rows(sep_2023) %>%
  bind_rows(oct_2023) %>%
  bind_rows(nov_2023) %>%
  bind_rows(dec_2023)

#View merged data
glimpse(trip_data) #column names and value types
View(trip_data) #view dataframe

#View column names
colnames(trip_data)

#Change member_casual to user_type
trip_data <- rename(trip_data, user_type = member_casual)

#Drop irrelevant columns
trip_data$start_station_id <- NULL
trip_data$end_station_id <- NULL
trip_data$start_lat <- NULL
trip_data$start_lng <- NULL
trip_data$end_lat <- NULL
trip_data$end_lng <- NULL

#View null values in each column
apply(X = is.na(trip_data), MARGIN = 2, FUN = sum)

#Remove null values
trip_data <- na.omit(trip_data)

#Check for spelling errors
checkrideable <- unique(trip_data$rideable_type)
checkuser <- unique(trip_data$user_type)

#Calculate trip duration
trip_data <- trip_data %>%
  mutate(ride_length = difftime(ended_at, started_at, units="mins"))

#Create start hour column
library(lubridate)
trip_data$start_hour <- hour(trip_data$started_at)
View(trip_data)

#check min, max and avg of ride_length
check_ride_length <- trip_data %>%
  summarise(min_ride_length = min(ride_length),
            max_ride_length = max(ride_length),
            avg_ride_length = mean(ride_length))
View(check_ride_length)

#Check for and remove outliers in data
#Calculating interquartile range (IQR)
Q1 <- quantile(trip_data$ride_length, 0.25)
Q3 <- quantile(trip_data$ride_length, 0.75)
IQR <- Q3 - Q1

#Define upper & lower bounds
lower_bound <- Q1 - 1.5 *IQR
upper_bound <- Q3 + 1.5 *IQR

#Identify outliers
outliers <- trip_data %>%
  filter(ride_length < lower_bound | ride_length > upper_bound)
View(outliers)

#Remove outliers from the dataset
filtered_tripdata <- trip_data %>%
  filter(!ride_length %in% outliers$ride_length)
filtered_tripdata <- filtered_tripdata %>%
  filter(ride_length >= 1)

#Compute day of week
#Include weekday in words
filtered_tripdata <- filtered_tripdata %>%
  mutate(weekday = weekdays(started_at))

#Include weekday number (1 is Sunday, 7 is Saturday)
filtered_tripdata <- filtered_tripdata %>%
  mutate(weekdaynum = wday(started_at))

#Month column
filtered_tripdata <- filtered_tripdata %>%
  mutate(month = month(started_at, label = TRUE, abbr = FALSE))

#Summary stats after removing outliers
summary(filtered_tripdata)

#EXPORT CLEANED DATA INTO CSV
write_csv(filtered_tripdata, "C:\\Users\\Abdulmalik\\Desktop\\Data Analysis\\cleaned_2023_data.csv")


#ANALYSIS
#Count number of members and casuals
count_usertype <- table(filtered_tripdata$user_type)
View(count_usertype)
user_count <- as.data.frame(count_usertype)
write_csv(user_count, "C:\\Users\\Abdulmalik\\Desktop\\Data Analysis\\user_count.csv")

#Display  Average trip duration for casuals and members
avg_ridelength <- filtered_tripdata %>%
  group_by(user_type) %>%
  summarise(Avg = mean(ride_length))
write_csv(avg_ridelength, "C:\\Users\\Abdulmalik\\Desktop\\Data Analysis\\avg_ride_length.csv")

#View Average for each day
avg_ridelength_pd <- filtered_tripdata %>%
  group_by(user_type, weekday) %>%
  summarise(Avg = mean(ride_length))
write_csv(avg_ridelength_pd, "C:\\Users\\Abdulmalik\\Desktop\\Data Analysis\\daily_avg_ride_length.csv")

#Usage of each bike type by user groups
trips_bike <- filtered_tripdata %>%
  group_by(rideable_type, user_type) %>%
  summarise(total_trips = n())
write_csv(trips_bike, "C:\\Users\\Abdulmalik\\Desktop\\Data Analysis\\trips_per_bike.csv")

#Usage for each day by time of day
daily_trips <- filtered_tripdata %>%
  group_by(start_hour, user_type) %>%
  summarise(total_trips = n())
write_csv(daily_trips, "C:\\Users\\Abdulmalik\\Desktop\\Data Analysis\\daily_trips.csv")

z#Number of trips for each month by user types
monthly_trips_m <- filtered_tripdata %>%
  group_by(month, user_type) %>%
  filter(user_type == "member") %>%
  summarise(total_trips = n())
#monthly_trips <- filtered_tripdata %>%
#group_by(month, user_type) %>%
#  summarise(total_trips = n())
monthly_trips_c <- filtered_tripdata %>%
  group_by(month, user_type) %>%
  filter(user_type == "casual") %>%
  summarise(total_trips = n())
write_csv(monthly_trips, "C:\\Users\\Abdulmalik\\Desktop\\Data Analysis\\monthly_trips.csv")

#Top 5 start stations
#CASUAL RIDERS
top_start_c <- filtered_tripdata %>%
  filter(user_type == "casual") %>%
  group_by(user_type) %>%
  count(start_station_name) %>%
  arrange(desc(n)) %>%
  slice_head(n = 5)
write_csv(top_start_c, "C:\\Users\\Abdulmalik\\Desktop\\Data Analysis\\casual_top_start.csv")

#ANNUAL MEMBERS
top_start_m <- filtered_tripdata %>%
  filter(user_type == "member") %>%
  group_by(user_type) %>%
  count(start_station_name) %>%
  arrange(desc(n)) %>%
  slice_head(n = 5)

write_csv(top_start_m, "C:\\Users\\Abdulmalik\\Desktop\\Data Analysis\\member_top_start.csv")



