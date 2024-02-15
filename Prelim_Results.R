
# Data Preprocessing ------------------------------------------------------

# Loading Data
data <- read.csv("project_data.csv")

# Loading packages
library(dplyr)
library(caret)
library(lubridate)

# Explore the dataset
na_rows <- data[apply(is.na(data), 1, any), ]
print(na_rows)
unique(data$booking_status)

# Remove unnecessary columns (Booking_ID)
data <- data[ , -1]

# Convert booking_status to 0 and 1
data$booking_status <- ifelse(data$booking_status == "not_canceled", 0, 1)

# Calculate booking_date based on arrival_date and lead_time
data$arrival_date <- as.Date(data$arrival_date)
data <- data %>%
  mutate(booking_date = arrival_date - lead_time)

# Extract day of week, day of month, and month from arrival_date and booking_date
data <- data %>%
  mutate(
    arrival_day_of_week = wday(arrival_date, label = TRUE), 
    arrival_day_of_month = day(arrival_date), 
    arrival_month = month(arrival_date, label = TRUE))
data <- data %>%
  mutate(
    booking_day_of_week = wday(booking_date, label = TRUE), 
    booking_day_of_month = day(booking_date), 
    booking_month = month(booking_date, label = TRUE))
data <- data %>%
  select(-c(arrival_date, booking_date))

# Create testing and training sets
training_ind <- createDataPartition(data$booking_status, 
                                    p = 0.75, 
                                    list = FALSE, 
                                    times = 1)
training_set <- data[training_ind, ]
test_set <- data[-training_ind, ]

# Assessing, grouping, and factoring categorical variables
training_set$booking_day_of_week <- as.character(training_set$booking_day_of_week)
training_set$booking_month <- as.character(training_set$booking_month)
training_set$arrival_day_of_week <- as.character(training_set$arrival_day_of_week)
training_set$arrival_month <- as.character(training_set$arrival_month)

unique(training_set$type_of_meal_plan)
unique(training_set$room_type_reserved)
unique(training_set$market_segment_type)
unique(training_set$booking_day_of_week)
unique(training_set$booking_month)
unique(training_set$arrival_day_of_week)
unique(training_set$arrival_month)

training_set$type_of_meal_plan <- factor(training_set$type_of_meal_plan)
training_set$room_type_reserved <- factor(training_set$room_type_reserved)
training_set$market_segment_type <- factor(training_set$market_segment_type)
training_set$booking_day_of_week <- factor(training_set$booking_day_of_week)
training_set$booking_month <- factor(training_set$booking_month)
training_set$arrival_day_of_week <- factor(training_set$arrival_day_of_week)
training_set$arrival_month <- factor(training_set$arrival_month)

class(training_set$type_of_meal_plan)
class(training_set$room_type_reserved)
class(training_set$market_segment_type)
class(training_set$booking_day_of_week)
class(training_set$booking_month)
class(training_set$arrival_day_of_week)
class(training_set$arrival_month)

levels(training_set$type_of_meal_plan)
levels(training_set$room_type_reserved)
levels(training_set$market_segment_type)
levels(training_set$booking_day_of_week)
levels(training_set$booking_month)
levels(training_set$arrival_day_of_week)
levels(training_set$arrival_month)

# One-hot encoding the training set
onehot_encoder <- dummyVars(~ type_of_meal_plan + room_type_reserved + market_segment_type + booking_day_of_week + booking_month + arrival_day_of_week + arrival_month, 
                            training_set[, c("type_of_meal_plan", "room_type_reserved", "market_segment_type", 
                                             "booking_day_of_week", "booking_month", "arrival_day_of_week", "arrival_month")], 
                            levelsOnly = FALSE, 
                            fullRank = TRUE)

onehot_enc_training <- predict(onehot_encoder, 
                               training_set[, c("type_of_meal_plan", "room_type_reserved", "market_segment_type",  
                                                "booking_day_of_week", "booking_month", "arrival_day_of_week", "arrival_month")])
training_set <- cbind(training_set, onehot_enc_training)

# One-hot encoding the test set
test_set$booking_day_of_week <- as.character(test_set$booking_day_of_week)
test_set$booking_month <- as.character(test_set$booking_month)
test_set$arrival_day_of_week <- as.character(test_set$arrival_day_of_week)
test_set$arrival_month <- as.character(test_set$arrival_month)

test_set$type_of_meal_plan <- factor(test_set$type_of_meal_plan)
test_set$room_type_reserved <- factor(test_set$room_type_reserved)
test_set$market_segment_type <- factor(test_set$market_segment_type)
test_set$booking_day_of_week <- factor(test_set$booking_day_of_week)
test_set$booking_month <- factor(test_set$booking_month)
test_set$arrival_day_of_week <- factor(test_set$arrival_day_of_week)
test_set$arrival_month <- factor(test_set$arrival_month)

onehot_enc_test <- predict(onehot_encoder, test_set[, c("type_of_meal_plan", "room_type_reserved", "market_segment_type", 
                                                        "booking_day_of_week", "booking_month", "arrival_day_of_week", "arrival_month")])
test_set <- cbind(test_set, onehot_enc_test)

# Scaling test and training sets
test_set[, -c(5, 7, 9, 15, 16, 18, 19, 21)] <- scale(test_set[, -c(5, 7, 9, 15, 16, 18, 19, 21)], 
                               center = apply(training_set[, -c(5, 7, 9, 15, 16, 18, 19, 21)], 2, mean), 
                               scale = apply(training_set[, -c(5, 7, 9, 15, 16, 18, 19, 21)], 2, sd))
training_set[, -c(5, 7, 9, 15, 16, 18, 19, 21)] <- scale(training_set[, -c(5, 7, 9, 15, 16, 18, 19, 21)])

# Convert data sets to tensors
training_features <- array(data = unlist(training_set[, -c(5, 7, 9, 15, 16, 18, 19, 21)]), 
                           dim = c(nrow(training_set), 42))
training_labels <- array(data = unlist(training_set[, 15]), 
                         dim = c(nrow(training_set)))

test_features <- array(data = unlist(test_set[, -c(5, 7, 9, 15, 16, 18, 19, 21)]), 
                       dim = c(nrow(test_set), 42))
test_labels <- array(data = unlist(test_set[, 15]), 
                     dim = c(nrow(test_set)))

# Remove unnecessary columns from training and test sets for use in linear models
training_set <- training_set[ , -c(5, 7, 9, 16, 18, 19, 21)]
test_set <- test_set[ , -c(5, 7, 9, 16, 18, 19, 21)]
