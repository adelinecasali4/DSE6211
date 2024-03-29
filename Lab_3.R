# Load libraries and data
library(dplyr)
library(caret)
data <- read.csv("lab_3_data.csv")

# Create testing and training sets
training_ind <- createDataPartition(data$lodgepole_pine, 
                                    p = 0.75, 
                                    list = FALSE, 
                                    times = 1)
training_set <- data[training_ind, ]
test_set <- data[-training_ind, ]

# Assessing, grouping, and factoring categorical variables
unique(training_set$wilderness_area)
unique(training_set$soil_type)

top_20_soil_types <- training_set %>% 
  group_by(soil_type) %>% 
  summarize(count = n()) %>% 
  arrange(desc(count)) %>%
  select(soil_type) %>% 
  top_n(20)

training_set$soil_type <- ifelse(training_set$soil_type %in% top_20_soil_types$soil_type, 
                                 training_set$soil_type, 
                                 "other")

training_set$wilderness_area <- factor(training_set$wilderness_area)
training_set$soil_type <- factor(training_set$soil_type)

class(training_set$wilderness_area)
class(training_set$soil_type)

levels(training_set$wilderness_area)
levels(training_set$soil_type)

# One-hot encoding the training set
onehot_encoder <- dummyVars(~ wilderness_area + soil_type, 
                            training_set[, c("wilderness_area", "soil_type")], 
                            levelsOnly = TRUE, 
                            fullRank = TRUE)

onehot_enc_training <- predict(onehot_encoder, 
                               training_set[, c("wilderness_area", "soil_type")])
training_set <- cbind(training_set, onehot_enc_training)

# One-hot encoding the test set
test_set$soil_type <- ifelse(test_set$soil_type %in% top_20_soil_types$soil_type, 
                             test_set$soil_type, 
                             "other")

test_set$wilderness_area <- factor(test_set$wilderness_area)
test_set$soil_type <- factor(test_set$soil_type)

onehot_enc_test <- predict(onehot_encoder, test_set[, c("wilderness_area", "soil_type")])
test_set <- cbind(test_set, onehot_enc_test)

# Scaling test and training sets
test_set[, -c(11:13)] <- scale(test_set[, -c(11:13)], 
                               center = apply(training_set[, -c(11:13)], 2, mean), 
                               scale = apply(training_set[, -c(11:13)], 2, sd))
training_set[, -c(11:13)] <- scale(training_set[, -c(11:13)])

# Convert data sets to tensors
training_features <- array(data = unlist(training_set[, -c(11:13)]), 
                           dim = c(nrow(training_set), 33))
training_labels <- array(data = unlist(training_set[, 13]), 
                         dim = c(nrow(training_set)))

test_features <- array(data = unlist(test_set[, -c(11:13)]), 
                           dim = c(nrow(test_set), 33))
test_labels <- array(data = unlist(test_set[, 13]), 
                         dim = c(nrow(test_set)))






