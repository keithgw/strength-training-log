#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(googlesheets)
library(lubridate)
library(shiny)
library(shinythemes)
library(tidyverse)

# helper functions
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

# load data from datbase
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

# Define UI for application that draws a histogram
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
    )
)
    
# Define server logic required to draw a histogram
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
           # plot estimated 1-rep max over time (progress)
           ggplot(aes(date, e1rm, color = movement)) +
               geom_line() +
               theme_minimal() +
               labs(title = "Estimated 1 rep max", y = "pounds")
       
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

