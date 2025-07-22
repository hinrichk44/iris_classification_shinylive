# 1.0 Libraries Needed ----


library(tidymodels)
library(readr)




# 2.0 Importing Data  ----


# Load the iris dataset
iris_raw <- iris

# Checking the see the unique values in the Species column since that is our target variable and it's classification, not prediction so that matters.
# Levels: setosa versicolor virginica
unique(iris_raw$Species)


# 3.0 Splitting the Data  ----

# Splitting into training and test sets helps to prevent overfitting and improve model generalization
# This then allows the model to perform well on future data
# Even better is cross-validation
# We'll split the iris dataset into training and testing sets.

# set.seed() is used to make random processes reproducible
set.seed(123) 

# initial_split() returns a split object with training and test sets and comes from rsample
# We will split it 80-20 with 80% going to the training set and 20% going to the test set
# The strata argument causes the random sampling to be conducted within the stratification variable
# This can help ensure that the number of data points in the analysis data is equivalent to the proportions in the original data set
# For example, the Species feature has three levels, so a random split may not account for all of them in the training set, which is bad.
# We can try to prevent this by adding Species as a stratification variable
iris_split <- initial_split(
  iris_raw, 
  prop = 0.8, 
  strata = Species)

# 120 data points went to the training set. 30 data points went to the test set. There are 150 points total
# <Training/Testing/Total>
#   <120/30/150>
iris_split

# training() returns the training dataset from the rsample split object
# Here we get a 120x5 tibble
iris_train <- training(iris_split)

# testing() returns the test dataset from the rsample split object
# Here we get a 30x5 tibble
iris_test <- testing(iris_split)

# Here we are simply checking if our stratification worked
# We have all 3 Species in the training set. That's good!
iris_train %>%
  distinct(Species)

# We have all 3 Species in the test set. That's good!
iris_test %>%
  distinct(Species)




# 4.0 Preprocessing the Data  ----

# recipe, step, prep, and bake are all important methods in the recipe library
# recipe() initializes a recipe object with a formula that identifies the target variable and predictors.
# It also includes the data from which to calculate the recipe from

# We'll create a preprocessing recipe that normalizes the numeric features.
iris_recipe <- recipe(Species ~ ., data = iris_train) %>%
  # normalize numeric predictors
  step_normalize(all_predictors()) %>%
  # prep() will perform initial calculations prior to applying the recipe
  # It trains the recipe
  # Steps should be added before prep()
  prep()

# bake() applies the prepared recipe to a dataset, performing the transformation
# We get a 120 x 5 tibble
iris_recipe_baked <- bake(iris_recipe, iris_train)


# tidy(recipe) returns information on all of the steps
# So as you can see we have 1 step in our recipe
# A tibble: 1 x 6
# number operation type      trained skip  id             
# <int> <chr>     <chr>     <lgl>   <lgl> <chr>          
#   1   step      normalize TRUE    FALSE normalize_TYDox
tidy(iris_recipe)


# Here we will "bake" our recipe on the train and test sets we created
# So train_transformed_tbl is a 77 x 37 tibble
# Our original training set was 77 x 14, but all the preprocessing steps we added ended up creating many new features
train_transformed_tbl <- bake(iris_recipe, iris_train)

# So test_transformed_tbl is a 20 x 37 tibble
test_transformed_tbl <- bake(iris_recipe, iris_test)




# 5.0 Define the Model ----

# Here we are building a Random Forest model to predict the Iris species. 
iris_rf_model <- rand_forest(mtry = 2, trees = 500) %>%
  set_engine("ranger") %>%
  set_mode("classification") %>%
  fit(Species ~ ., data = train_transformed_tbl)




# 6.0 Create a Workflow ----

# Combine the recipe and model into a workflow.
# iris_workflow <- workflow() %>%
#   add_model(rf_model) %>%
#   add_recipe(iris_recipe)




# 7.0 Fit the Model ----

# Fit the model to the training data.
# iris_fit <- iris_workflow %>%
#   fit(data = iris_train)




# 8.0 Evaluate the Model ----

# Predict on the test set and evaluate the model’s accuracy.
# Predict on test data
iris_rf_model_predictions <- iris_rf_model %>%
  predict(new_data = test_transformed_tbl) %>%
  rename("Species Predicted" = .pred_class) %>%
  bind_cols(iris_test %>% select(Species)) 

# Evaluate the performance
iris_rf_metrics <- iris_rf_model_predictions %>%
  metrics(truth = Species, estimate = `Species Predicted`)

# A tibble: 2 x 3
#     .metric  .estimator .estimate
#     <chr>    <chr>          <dbl>
#   1 accuracy multiclass     0.933
#   2 kap      multiclass     0.9 
print(iris_rf_metrics)

# You can also evaluate the model using a confusion matrix.
iris_conf_mat <- iris_rf_model_predictions %>%
  conf_mat(truth = Species, estimate = `Species Predicted`)


# Truth
# Prediction   setosa versicolor virginica
# setosa         10          0         0
# versicolor      0         10         2
# virginica       0          0         8
print(iris_conf_mat)




# 9.0 Saving the Model ----


# Here we will create a list of all the models we created
total_models_tbl <- list(
  "MODEL_01__RF_RANDOMFOREST" = iris_rf_model
) %>%
  # enframe() turns a list into a data frame
  # So now we have 1x2 tibble with a column named "name" with model names
  # We have another column called "value" with the actual model inside it (nested)
  # We changed the column names to model_id and model_name
  enframe(name = "model_id", value = "model_name")


# Here we are providing a path for where to save our models
# So we are adding it to the Iris_Models folder we just created
# And we are going to call our data frame full of models iris_models_tbl.rds
total_models_tbl %>% write_rds("Iris_Models/iris_models_tbl.rds") 



# Here we will create a list of all the recipes we created
# We only created one in this project
total_recipes_tbl <- list(
  "RECIPE_01" = iris_recipe) %>%
  # We get a 1x2 tibble
  enframe(name = "recipe_id", value = "recipe_object")


# Here we are providing a path for where to save our recipe
# So we are adding it to the Iris_Models folder we just created
# And we are going to call our dataframe with our one recipe recipes_tbl.rds
total_recipes_tbl %>% write_rds("Iris_Models/iris_recipes_tbl.rds")



# 10.0 Reading In Models ----

# Here we are reading in the items we saved
all_iris_models <- read_rds("Iris_Models/iris_models_tbl.rds")
all_iris_recipes <- read_rds("Iris_Models/iris_recipes_tbl.rds")



# 11.0 Applying Read-In Model to New Data ----


# Here we will create a new Iris plant 
new_iris_plant <- tibble(
  Sepal.Length      = 5.8,
  Sepal.Width       = 2.9,
  Petal.Length      = 4.2,
  Petal.Width       = 0.9
) 


# Now we will use all of our models to predict the price of the made up bike model we created
full_predictions_tbl <- all_iris_models %>%
  mutate(species_predictions = map(model_name, predict, new_data = new_iris_plant)) %>%
  unnest(species_predictions)

# A tibble: 1 x 3
#     model_id                  model_name .pred_class
#     <chr>                     <list>     <fct>      
#   1 MODEL_01__RF_RANDOMFOREST <fit[+]>   virginica 
full_predictions_tbl

just_predictions_tbl <- full_predictions_tbl %>%
  select(.pred_class)


# 12.0 Testing Things Out Before API ----


# Here we will create a new Iris plant 
new_iris_plant <- tibble(
  Sepal.Length      = 5.8,
  Sepal.Width       = 2.9,
  Petal.Length      = 4.2,
  Petal.Width       = 0.9
)


# Here we begin with extracting the specific recipe you want (e.g., RECIPE_01)
recipe_list <- all_iris_recipes %>% 
  filter(recipe_id == "RECIPE_01") %>% 
  pull(recipe_object)

# Here we extract the actual recipe object
iris_recipe <- recipe_list[[1]]

# Apply the recipe to the new data (bake it)
preprocessed_new_data <- bake(iris_recipe, new_iris_plant)

new_predictions_tbl <- all_iris_models %>%
  mutate(species_predictions = map(model_name, predict, new_data = preprocessed_new_data)) %>%
  unnest(species_predictions)

# A tibble: 1 x 3
#   model_id                  model_name .pred_class
#   <chr>                     <list>     <fct>      
#   1 MODEL_01__RF_RANDOMFOREST <fit[+]>   versicolor
new_predictions_tbl



