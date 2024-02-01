# Load packages
library(reticulate)
library(tensorflow)
library(keras)

# Preparing data
mtcars <- mtcars
mtcars_x <- mtcars[, c("cyl", "disp", "hp")]
mtcars_x <- array(data = unlist(mtcars_x), 
                  dim = c(32, 3), 
                  dimnames = list(rownames(mtcars_x), colnames(mtcars_x)))
mtcars_y <- mtcars[, "mpg"]

# Building the model
nn_model <- keras_model_sequential() %>%
  layer_dense(units = 1, input_shape = 3, activation = "linear")
nn_model 

# Adding hidden layers
nn_model <- keras_model_sequential() %>% 
  layer_dense(units = 2, input_shape = 3, activation = "relu") %>% 
  layer_dense(units = 1, activation = "linear")
nn_model

# Adding the loss function and optimization
nn_model %>% compile(optimizer = optimizer_adam(learning_rate = 0.2), 
                     loss = "mean_squared_error", 
                     metrics = "mean_absolute_error")

# Training the neural network model
nn_model_training <- nn_model %>% fit(x = mtcars_x, 
                                      y = mtcars_y, 
                                      epochs = 250, 
                                      verbose = FALSE)

# Plotting the process (learning curve)
plot(nn_model_training)

# Getting weights and predictions
get_weights(nn_model)
prediction <- predict(nn_model, array(c(6, 350, 125), dim = c(1, 3)))
prediction
