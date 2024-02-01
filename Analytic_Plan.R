# Import the data and load packages
library(ggplot2)
library(tidyverse)
library(lubridate)
hotel_df <- read.csv("project_data.csv")

# View the data and summary statistics
head(hotel_df)
hotel_df %>% 
  group_by(booking_status) %>%
  count(booking_status)

# Integer encode booking_status
hotel_df <- hotel_df %>%
  mutate(booking_status_int = if_else(booking_status == "not_canceled", 0, 1))

# Extract month and year
hotel_df <- hotel_df %>%
  mutate(arrival_month = month(arrival_date),
         arrival_year = year(arrival_date))

# Group by month and year
grouped_df <- hotel_df %>%
  group_by(arrival_month, arrival_year)

# Calculate cancellation rates
cancellation_rates <- grouped_df %>%
  summarise(cancelation_rate = mean(booking_status_int == 1))

# Print the cancellation_rates to inspect the data
print(cancellation_rates)

# Plot the data over time
ggplot(cancellation_rates, aes(x = as.Date(paste(arrival_year, arrival_month, "01", sep = "-")), y = cancelation_rate)) +
  geom_line() +
  labs(title = "Cancellation Rates Over Time",
       x = "Month/Year",
       y = "Cancellation Rate") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_x_date(breaks = scales::breaks_width("1 month"), date_labels = "%m/%Y")

# Get rate per year
grouped_df2 <- hotel_df %>%
  group_by(arrival_year)

# Calculate cancellation rates
cancellation_rates2 <- grouped_df2 %>%
  summarise(cancelation_rate = mean(booking_status_int == 1))
print(cancellation_rates2)
