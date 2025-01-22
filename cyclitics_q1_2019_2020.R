# setting up the environment
# helps wrangle data
library(tidyverse)
# managing conflicted packages
library(conflicted)
# setting dplyr::filter and dplyr::lag as defaults
conflict_prefer("filter", "dplyr")
conflict_prefer("lag", "dplyr")


#=====================
# Step 1: COLLECT DATA
#=====================

# Upload Divvy datasets (csv files)
q1_2019 <- read_csv("RStudio/Cyclitics/Divvy_Trips_2019_Q1.csv")
q1_2020 <- read_csv("RStudio/Cyclitics/Divvy_Trips_2020_Q1.csv")


#====================================================
# Step 2: WRANGLE DATA AND COMBINE INTO A SINGLE FILE
#====================================================

# Compare column names of each of the files
colnames(q1_2019)
colnames(q1_2020)

# Renaming columns so they are consistent with Q1_2020
(q1_2019 <- rename(q1_2019
                   ,ride_id = trip_id
                   ,rideable_type = bikeid
                   ,started_at = start_time
                   ,ended_at = end_time
                   ,start_station_name = from_station_name
                   ,start_station_id = from_station_id
                   ,end_station_name = to_station_name
                   ,end_station_id = to_station_id
                   ,member_casual = usertype
                   ))

# Inspecting data frames for any other inconsistencies
str(q1_2019)
str(q1_2020)

# Converting ride_id and rideable_type to character
q1_2019 <- mutate(q1_2019, ride_id = as.character(ride_id)
                  ,rideable_type = as.character(rideable_type))

# Stack individual quarter's frames into one
all_trips <- bind_rows(q1_2019, q1_2020)

# Remove lat, long, birthyear, and gender fields
all_trips <- all_trips %>%
  select(-c(start_lat, start_lng, end_lat, end_lng, birthyear, gender, "tripduration"))


#======================================================
# STEP 3: CLEAN UP AND ADD DATA TO PREPARE FOR ANALYSIS
#======================================================

# Inspect the new table
# list of column names
colnames(all_trips)
# number of rows
nrow(all_trips)
# dimensions of dataframe
dim(all_trips)
# preview fisrt 6 rows
head(all_trips)
# list of columns and data types
str(all_trips)
# satatistical summary of data
summary(all_trips)

# consolidating member_casual values from four to two
# check how many distinct values are in the column
table(all_trips$member_casual)
# rename "Subscriber" and "Customer" to "member" and "casual" respectively
all_trips <- all_trips %>%
  mutate(member_casual = if_else(member_casual == "Subscriber", "member", member_casual))
all_trips <- all_trips %>%
  mutate(member_casual = if_else(member_casual == "Customer", "casual", member_casual))
# Recheck distinct values
table(all_trips$member_casual)

# Add columns: date, month, day, and year
all_trips$date <-as.Date(all_trips$started_at)
all_trips$month <- format(as.Date(all_trips$date), "%m")
all_trips$day <- format(as.Date(all_trips$date), "%d")
all_trips$year <- format(as.Date(all_trips$date), "%Y")
all_trips$day_of_week <- format(as.Date(all_trips$date), "%A")

# Add a "ride_length" calculation to all_trips
all_trips$ride_length <- difftime(all_trips$ended_at,all_trips$started_at)

# Inspect the structure of the columns
str(all_trips)

# Convert "ride_length" to a time format
is.factor(all_trips$ride_length)
all_trips$ride_length <- as.numeric(as.character(all_trips$ride_length))
is.numeric(all_trips$ride_length)

# Remove bad data
# Creating new version of data because of removal of some data
all_trips_v2 <- all_trips[!(all_trips$start_station_name == "HQ QR" | all_trips$ride_length<0),]


#=====================================
# STEP 4: CONDUCT DESCRIPTIVE ANALYSIS
#=====================================

# Descriptive analysis on ride_length
mean(all_trips_v2$ride_length)
median(all_trips_v2$ride_length)
max(all_trips_v2$ride_length)
min(all_trips_v2$ride_length)
summary(all_trips_v2)

# Compare member and casual users
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = mean)
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = median)
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = max)
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = min)

# Avg ride time by each day (members v casuals)
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual + all_trips_v2$day_of_week, FUN = mean)
# Ordering the days of the week
all_trips_v2$day_of_week <- ordered(all_trips_v2$day_of_week, levels=c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))

# Rerun avg ride time
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual + all_trips_v2$day_of_week, FUN = mean)

# Analyze ridership data by type and weekday
all_trips_v2 %>%
  mutate(weekday = wday(started_at, label = TRUE)) %>%
  group_by(member_casual, weekday) %>%
  summarise(number_of_rides = n()
            ,average_duration = mean(ride_length)) %>%
  arrange(member_casual, weekday)

# Visualize number of rides by rider type
all_trips_v2 %>%
  mutate(weekday = wday(started_at, label = TRUE)) %>%
  group_by(member_casual, weekday) %>%
  summarise(number_of_rides = n()
            ,average_duration = mean(ride_length)) %>%
  arrange(member_casual, weekday) %>%
  ggplot(aes(x = weekday, y = average_duration, fill = member_casual)) +
  geom_col(position = "dodge")


#=================================================
# STEP 5: EXPORT SUMMARY FILE FOR FURTHER ANALYSIS
#=================================================

# Creating a csv file for other presentation software
counts <- aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual + all_trips_v2$day_of_week, FUN = mean)
write.csv(counts, file = 'avg_ride_length.csv')