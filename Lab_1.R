library(keras)
library(tensorflow)

# Confirming installation
tf$constant("Hello Tensorflow!")

# Preparing the data
mtcars <- mtcars
mtcars_x <- mtcars[, c("cyl", "disp", "hp")]
mtcars_x <- array(data = unlist(mtcars_x), 
                  dim = c(32,3), dimnames = list(rownames(mtcars_x), 
                                                 colnames(mtcars_x)))
mtcars_y <- mtcars[, "mpg"]

# Building the model
nn_model <- keras_model_sequential() %>%
  layer_dense(units = 1, input_shape = 3, activation = "linear")
nn_model %>% compile(optimizer = optimizer_adam(learning_rate = 0.2), 
                     loss = "mean_squared_error")
nn_model_training <- nn_model %>% fit(x = mtcars_x, 
                                      y = mtcars_y, 
                                      epochs = 10000, 
                                      verbose = FALSE)

# Obtain weights
get_weights(nn_model)

# Compare to linear model
lr_model <- lm(mpg ~ cyl + disp + hp, data = mtcars)
lr_model$coefficients