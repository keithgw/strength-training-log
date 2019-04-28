library(googlesheets)
library(lubridate)
library(shiny)
library(shinythemes)
library(tidyverse)

# helper functions -------------------------------------------------------------
rpe_to_prop <- function(rpe, reps) {
    # this puts everything in relation to an RPE of 10
    effective_reps <- 10 - rpe + reps
    # these are intercepts fit from prop = 1 - beta_1*x - beta_2 * ln(x)
    1.02398128 - 0.02398127 * effective_reps - 0.02167904 * log(effective_reps)
}

estimate_1rm <- function(weight, rpe, reps) {
    # convert weight to estimated 1-rep max
    weight / rpe_to_prop(rpe, reps)
}

rpe_weight <- function(e1rm, rpe, reps, round=FALSE) {
    # convert 1-rep max to weight at a given RPE
    weight <- e1rm * rpe_to_prop(rpe, reps)
    if (round) {
        # round to the nearest 5 pounds
        round(weight / 5) * 5
    } else {
        weight
    }
}

# load data from datbase -------------------------------------------------------
sheet_key <- "1AWA6Zq_BG2gYPwGFpUC-M-4YDoA58CMDANWvC4qoZ3A"
gs_auth()
str_log <- gs_key(sheet_key) %>% 
    gs_read() %>% 
    mutate(
        # if no date is given, use the date of entry
        date = ifelse(is.na(date), as_date(mdy_hms(Timestamp)), mdy(date)) %>% as_date(), 
        # calculate estimated 1-rep max for each session
        e1rm = pmap_dbl(list(weight, rpe, reps), estimate_1rm)
    )

# user interface ---------------------------------------------------------------
ui <- navbarPage(
    title = "Strength Training Log",
    theme = shinythemes::shinytheme("yeti"),
    windowTitle = "Strength Training Log",
    tabPanel(
        "Progress",
        fluidRow(
            column(
                2,
                uiOutput("movement_choices")
            ),
            column(
                10,
                plotOutput("progress_plot")
            )
        )
    ),
    tabPanel(
        "Training Weight",
        fluidRow(
            column(
                10,
                tableOutput("training_weight")
            ),
            column(
                2,
                selectInput(
                    "reps",
                    "reps",
                    choices = 1:12,
                    selected = 5
                )
            )
        ),
        fluidRow(
            plotOutput("estimated_1rm")
        )
    )
)
    
# server -----------------------------------------------------------------------
server <- function(input, output) {
   output$movement_choices <- renderUI({
       movements <- unique(str_log$movement)
       checkboxGroupInput(
           "movement_filter",
           label = "Select Movements",
           choices = movements,
           selected = movements
       )
   })
   
   output$progress_plot <- renderPlot({
       str_log %>% 
           filter(movement %in% input$movement_filter) %>%
           group_by(date, movement) %>% 
           summarise(e1rm = mean(e1rm)) %>% 
           # plot estimated 1-rep max over time (progress)
           ggplot(aes(date, e1rm, color = movement)) +
               geom_line() +
               theme_minimal() +
               labs(title = "Estimated 1 rep max", y = "pounds")
       
   })
   
   output$estimated_1rm <- renderPlot({
       # use the last three weeks to estimate current 1-rep max
       e1rm_3weeks <- str_log %>% 
           filter(date > today() - ddays(21),
                  variation == "conventional")
       e1rm_estimates <- e1rm_3weeks %>% 
           group_by(movement) %>% 
           summarise(estimate = mean(e1rm))
       
       # show estimated current 1-rep max
       ggplot(e1rm_3weeks, aes(x = fct_reorder(movement, e1rm), y = e1rm)) +
           geom_point(alpha = 0.5) +
           geom_label(data = e1rm_estimates, 
                      aes(x = fct_reorder(movement, estimate), 
                          y = estimate, 
                          label = round(estimate, 0)
                      ),
                      nudge_x = 0.2) +
           labs(x = "movement", y = "pounds", title = "3 week estimated 1 rep max") +
           theme_minimal() 
   })
   
   output$training_weight <- renderTable({
       # use the last three weeks to estimate current 1-rep max
       e1rm_3weeks <- str_log %>% 
           filter(date > today() - ddays(21),
                  variation == "conventional")
       e1rm_estimates <- e1rm_3weeks %>% 
           group_by(movement) %>% 
           summarise(estimate = mean(e1rm))
       reps <- as.integer(input$reps)
       e1rm_estimates %>% 
           mutate(rpe6 = rpe_weight(estimate, 6, reps, TRUE),
                  rpe7 = rpe_weight(estimate, 7, reps, TRUE),
                  rpe8 = rpe_weight(estimate, 8, reps, TRUE),
                  rpe9 = rpe_weight(estimate, 9, reps, TRUE))
   }, digits = 0)
}

# Run the application ----------------------------------------------------------
shinyApp(ui = ui, server = server)

