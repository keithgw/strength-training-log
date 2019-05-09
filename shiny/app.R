library(googlesheets)
library(lubridate)
library(shiny)
library(shinythemes)
library(tidyverse)

# constants --------------------------------------------------------------------
kg_lb <- 2.20462

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

rpe_weight <- function(e1rm, rpe, reps, weight_units=c("lb", "kg"), round=FALSE) {
    weight_units <- match.arg(weight_units)
    round_unit <- ifelse(weight_units == "lb", 5, 2)
    # convert 1-rep max to weight at a given RPE
    weight <- e1rm * rpe_to_prop(rpe, reps)
    if (round) {
        # round to the nearest 5 pounds
        round(weight / round_unit) * round_unit
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
        units = ifelse(is.na(units), "lb", units),
        wk = lubridate::week(date),
        volume = reps * sets,
        tonnage = volume * weight,
        pounds = ifelse(units == "lb", weight, weight * kg_lb),
        kilos = ifelse(units == "kg", weight, weight / kg_lb)
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
                10,
                plotOutput("progress_plot")
            ),
            column(
                2,
                uiOutput("movement_choices"),
                radioButtons(
                    inputId = "units",
                    label = "units",
                    choices = c("lb", "kg"),
                    selected = "kg"
                )
            )
        )
    ),
    tabPanel(
        "Training Weight",
        fluidRow(
            column(
                10,
                "Estimated Training Weight",
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
        ),
        fluidRow(
            "Last Week's Weights",
            tableOutput("last_week")
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
   
   progress_data <- reactive({
       if (input$units == "lb") {
           estimated_data <- str_log %>%
               mutate(
                   # calculate estimated 1-rep max for each session
                   e1rm = pmap_dbl(list(pounds, rpe, reps), estimate_1rm)
               )
       } else {
           estimated_data <- str_log %>%
               mutate(
                   # calculate estimated 1-rep max for each session
                   e1rm = pmap_dbl(list(kilos, rpe, reps), estimate_1rm)
               )
       }
       estimated_data %>%
           group_by(date, movement) %>%
           summarise(e1rm = mean(e1rm))
   })
   
   output$progress_plot <- renderPlot({
       progress_data() %>%
           filter(movement %in% input$movement_filter) %>%
           # plot estimated 1-rep max over time (progress)
           ggplot(aes(date, e1rm, color = movement)) +
               geom_line() +
               theme_minimal() +
               labs(title = "Estimated 1 rep max", y = input$units)
       
   })
   
   three_weeks <- reactive({
       # use the last three weeks to estimate current 1-rep max
       last3weeks <- str_log %>%
           filter(date > today() - ddays(21),
                  variation == "conventional")
       if (input$units == "lb") {
           e1rm_3weeks <- last3weeks %>%
               mutate(
                   # calculate estimated 1-rep max for each session
                   e1rm = pmap_dbl(list(pounds, rpe, reps), estimate_1rm)
               )
       } else {
           e1rm_3weeks <- last3weeks %>%
               mutate(
                   # calculate estimated 1-rep max for each session
                   e1rm = pmap_dbl(list(kilos, rpe, reps), estimate_1rm)
               )
       }
       return(e1rm_3weeks)
   })
   
   output$estimated_1rm <- renderPlot({
       # use the last three weeks to estimate current 1-rep max
       e1rm_3weeks <- three_weeks()
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
           labs(x = "movement", y = input$units, title = "3 week estimated 1 rep max") +
           theme_minimal()
   })
   
   output$training_weight <- renderTable({
       # use the last three weeks to estimate current 1-rep max
       e1rm_3weeks <- three_weeks()
       e1rm_estimates <- e1rm_3weeks %>%
           group_by(movement) %>%
           summarise(estimate = mean(e1rm))
       reps <- as.integer(input$reps)
       e1rm_estimates %>%
           mutate(rpe6 = rpe_weight(estimate, 6, reps, input$units, round = TRUE),
                  rpe7 = rpe_weight(estimate, 7, reps, input$units, round = TRUE),
                  rpe8 = rpe_weight(estimate, 8, reps, input$units, round = TRUE),
                  rpe9 = rpe_weight(estimate, 9, reps, input$units, round = TRUE))
   }, digits = 0)
   
   output$last_week <- renderTable({
       str_log %>%
           # filter to last week
           filter(wk == lubridate::week(today()) - 1) %>%
           group_by(movement, variation, reps, rpe) %>%
           summarise(wt = ifelse(input$units == "lb", max(pounds), max(kilos))) %>%
           spread(key = rpe, value = wt, sep = "")
   }, digits = 0, na = "")
}

# Run the application ----------------------------------------------------------
shinyApp(ui = ui, server = server)

