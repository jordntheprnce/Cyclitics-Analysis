# overall number of trips
ggplot(all_trips_v2, aes(x = day_of_week, fill = member_casual)) +
  geom_bar(position = "dodge") +
  labs(x = "Day of Week", y = "Number of Trips", title = "Number of Trips by Members and Casual Riders") +
  theme_minimal()


# Create heatmap
library(ggplot2)
library(dplyr)

# Create a summary table
summary_data <- all_trips %>%
  group_by(day_of_week, hour, member_casual) %>%
  summarize(trips = n(), .groups = "drop")

# Create line plot with facet grid
ggplot(summary_data, aes(x = hour, y = trips, color = member_casual)) +
  geom_line() +
  facet_grid(day_of_week ~ .) +
  labs(x = "Hour of the Day", y = "Number of Trips", color = "Rider Type", 
       title = "Hourly Trips by Members and Casual Riders") +
  theme_minimal()

# distinct station names
num_stations <- all_trips_v2 %>%
  summarize(num_stations = n_distinct(start_station_name))

# top 50 or so least used stations (are they member or casual)
station_summary <- all_trips_v2 %>%
  group_by(start_station_name, member_casual) %>%
  summarize(trips = n(), .groups = "drop")

least_used_stations <- station_summary %>%
  group_by(start_station_name) %>%
  summarize(total_trips = sum(trips), .groups = "drop") %>%
  arrange(total_trips) %>%
  slice(1:50)

least_used_stations_details <- station_summary %>%
  filter(start_station_name %in% least_used_stations$start_station_name)

least_used_stations_final <- least_used_stations_details %>%
  group_by(start_station_name) %>%
  mutate(frequent_rider = ifelse(trips[member_casual == "member"] > trips[member_casual == "casual"], "member", "casual"))

least_used_stations_details
