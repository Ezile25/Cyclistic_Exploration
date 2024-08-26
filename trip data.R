setwd("C:/Users/User/Desktop/Data analytics/Cyclist data")

# Loading necessary libraries
library(tidyverse)
library(conflicted)
library(dplyr)
library(ggplot2)
library(lubridate)

# Setting conflict preferences
conflict_prefer("filter", "dplyr")
conflict_prefer("lag", "dplyr")

# Loading the combined Divvy dataset
data <- read.csv("combined_divvy_tripdata.csv")
# Inspect the structure of the dataset
str(data)
summary(data)

# Checking for unique values in `member_casual` 
table(data$member_casual)

# consolidating member_casual labels
data <- data %>% 
  mutate(member_casual = recode(member_casual, 
                                "Subscriber" = "member", 
                                "Customer" = "casual"))

# date, month, day, year, and day_of_week columns
data_clean <- data_clean %>%
  mutate(date = as.Date(started_at, format = "%Y-%m-%d %H:%M:%S"),
         month = format(date, "%m"),
         day = format(date, "%d"),
         year = format(date, "%Y"),
         day_of_week = format(date, "%A"))
# Checking the first few rows of the cleaned data
head(data_clean)

# Checking for missing values again
sum(is.na(data_clean$day_of_week))


head(data$started_at)
head(data$ended_at)
# ride_length calculation
data <- data %>%
  mutate(
    started_at = as.POSIXct(started_at, format = "%m/%d/%Y %H:%M"),
    ended_at = as.POSIXct(ended_at, format = "%m/%d/%Y %H:%M")
  )
data$ride_length <- difftime(data$ended_at, data$started_at, units = "mins")
# Converting ride_length to numeric
data$ride_length <- as.numeric(as.character(data$ride_length))

# Removing bad data
data_clean <- data[!(data$start_station_name == "HQ QR" | data$ride_length < 0),]
# Inspecting the cleaned dataset
str(data_clean)
summary(data_clean)
# Descriptive analysis on ride_length
summary(data_clean$ride_length)

# Comparing ride_length by member type
aggregate(ride_length ~ member_casual, data = data_clean, FUN = mean)
aggregate(ride_length ~ member_casual, data = data_clean, FUN = median)
aggregate(ride_length ~ member_casual, data = data_clean, FUN = max)
aggregate(ride_length ~ member_casual, data = data_clean, FUN = min)

# Checking for missing values
sum(is.na(data_clean$day_of_week))
sum(is.na(data_clean$ride_length))
#checking data type
str(data_clean)
data_clean %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>% 
  group_by(member_casual, weekday) %>% 
  summarise(number_of_rides = n(), 
            average_duration = mean(ride_length, na.rm = TRUE), 
            .groups = "drop") %>% 
  ggplot(aes(x = weekday, y = number_of_rides, fill = member_casual)) +
  geom_col(position = "dodge")



# average duration by rider type
data_clean %>%
  mutate(weekday = wday(started_at, label = TRUE)) %>%
  group_by(member_casual, weekday) %>%
  summarise(number_of_rides = n(),
            average_duration = mean(ride_length, na.rm = TRUE),
            .groups = "drop") %>%
  ggplot(aes(x = weekday, y = average_duration, fill = member_casual)) +
  geom_col(position = "dodge")

