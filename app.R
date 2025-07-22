# 1.0 Libraries ----

library(shiny)
library(shinyWidgets)
library(shinythemes)
library(shinydashboard)
library(shinydashboardPlus)
library(tidyverse)
library(tidymodels)
library(bslib)
library(shinylive)







# 2.0 Loading Files ----


all_iris_recipes <- read_rds("Iris_Models/iris_recipes_tbl.rds")
all_iris_models <- read_rds("Iris_Models/iris_models_tbl.rds")


# 3.0 UI ----
ui <- fluidPage(
  
  theme = shinytheme("superhero"),
  
  titlePanel("Iris Classification Using Tidymodels"),
  
  box(
    title = "Inputs for Classification Model",
    width = 12,
    status = "primary",
    p("Enter an appropriate value in each box within the range provided"),
    numericInput(
      inputId = "sepalLength",
      label   = "Sepal.Length",
      value   = min(iris$Sepal.Length),
      min     = min(iris$Sepal.Length),
      max     = max(iris$Sepal.Length),
      # Controls the interval by which the value will increase or decrease when you press the up/down arrow
      step    = 0.5
    ),
    helpText(
      paste(
        "The allowed range for Sepal Length is:",
        paste0("", min(iris$Sepal.Length), " to ", max(iris$Sepal.Length), "")
      )
    ),
    numericInput(
      inputId = "sepalWidth",
      label   = "Sepal.Width",
      value   = min(iris$Sepal.Width),
      min     = min(iris$Sepal.Width),
      max     = max(iris$Sepal.Width),
      step    = 0.5
    ),
    helpText(
      paste(
        "The allowed range for Sepal Width is:",
        paste0("", min(iris$Sepal.Width), " to ", max(iris$Sepal.Width), "")
      )
    ),
    numericInput(
      inputId = "petalLength",
      label   = "Petal.Length",
      value   = min(iris$Petal.Length),
      min     = min(iris$Petal.Length),
      max     = max(iris$Petal.Length),
      step    = 0.5
    ),
    helpText(
      paste(
        "The allowed range for Petal Length is:",
        paste0("", min(iris$Petal.Length), " to ", max(iris$Petal.Length), "")
      )
    ),
    numericInput(
      inputId = "petalWidth",
      label   = "Petal.Width",
      value   = min(iris$Petal.Width),
      min     = min(iris$Petal.Width),
      max     = max(iris$Petal.Width),
      step    = 0.5
    ),
    helpText(
      paste(
        "The allowed range for Petal Width is:",
        paste0("", min(iris$Petal.Width), " to ", max(iris$Petal.Width), "")
      )
    )
  ),
  br(),
  # Text output to display selected values
  textOutput("result"),
  br(),
  p("When you are done entering the model inputs, please click 'Apply Model' below to retrieve the results"),
  actionButton(
    inputId = "apply_button", 
    label = "Apply Model", 
    icon = icon("play")
  ),
  box(
    title = "Output of Classification Model",
    width = 12,
    status = "primary",
    # Table to display model output
    tableOutput(outputId = "model_result")
  )
)




# 4.0 Server ----
server <- function(input, output, session) {
  
  # Here we begin with extracting the specific recipe you want (e.g., RECIPE_01)
  recipe_list <- all_iris_recipes %>% 
    filter(recipe_id == "RECIPE_01") %>% 
    pull(recipe_object)
  
  # Here we extract the actual recipe object
  iris_recipe <- recipe_list[[1]]
  
  
  
  output$result <- renderText({
    paste(
      "You selected:",
      paste("Sepal.Length =", input$sepalLength),
      paste("| Sepal.Width =", input$sepalWidth),
      paste("| Petal.Length =", input$petalLength),
      paste("| Petal.Width =", input$petalWidth)
    )
  })
  
  
  # A reactiveVal to store the data table once user clicks the button
  user_data <- reactiveVal(NULL)
  
  
  # When the user clicks "Apply Model", generate a tibble of current input values
  observeEvent(input$apply_button, {
    
    df <- tibble(
      Sepal.Length = input$sepalLength,
      Sepal.Width  = input$sepalWidth,
      Petal.Length = input$petalLength,
      Petal.Width  = input$petalWidth
    )
    
    user_data(df)
    
    # Apply the recipe to the new data (bake it)
    preprocessed_new_data <- bake(iris_recipe, user_data())
    
    new_predictions_tbl <- all_iris_models %>%
      mutate(species_predictions = map(model_name, predict, new_data = preprocessed_new_data)) %>%
      unnest(species_predictions)
    
    
    # Here we are just extracting the predicted species based on the user data
    just_predictions_tbl <- new_predictions_tbl %>%
      select(.pred_class) %>%
      rename("Predicted Species" = .pred_class)
    # This will return the actual value and not a tibble
    # pluck(1)
    
    
    # Here we are showing the predicted species based on the custom user inputs
    output$model_result <- renderTable({
      
      just_predictions_tbl
      
    })
    
  })
  
  
  
}




# 5.0 Run the App ----
shinyApp(ui, server)



