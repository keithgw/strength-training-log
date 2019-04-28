library(tidyverse)
library(googlesheets)
library(lubridate)

sheet_key <- "1AWA6Zq_BG2gYPwGFpUC-M-4YDoA58CMDANWvC4qoZ3A"
gs_auth()
sheet <- gs_key(sheet_key)

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

# get the data from the database
slog <- gs_read(sheet) %>% 
    mutate(
        # if no date is given, use the date of entry
        date = ifelse(is.na(date), as_date(mdy_hms(Timestamp)), mdy(date)) %>% as_date(), 
        # calculate estimated 1-rep max for each session
        e1rm = pmap_dbl(list(weight, rpe, reps), estimate_1rm)
    )

# plot estimated 1-rep max over time (progress)
ggplot(slog, aes(date, e1rm, color = movement)) +
    geom_line() +
    theme_minimal() +
    labs(title = "Estimate 1 rep max", y = "pounds")

# use the last three weeks to estimate current 1-rep max
e1rm_3weeks <- slog %>% 
    filter(date > today() - ddays(21),
           variation == "conventional")
e1rm_estimates <- e1rm_3weeks %>% 
    group_by(movement) %>% 
    summarise(estimate = mean(e1rm))

# show estimated current 1-rep max
ggplot(e1rm_3weeks, aes(x = fct_reorder(movement, e1rm), y = e1rm)) +
    geom_point(alpha = 0.5) +
    geom_label(data = e1rm_estimates, 
               aes(x = fct_reorder(movement, estimate), y = estimate, label = round(estimate, 0)),
               nudge_x = 0.2) +
    labs(x = "movement", y = "pounds", title = "3 week estimated 1 rep max") +
    theme_minimal() 

# use for deciding weights for a given session
reps_for_estimates <- 8 # this will be an input in shiny
e1rm_estimates %>% 
    mutate(rpe6 = rpe_weight(estimate, 6, reps_for_estimates, TRUE),
           rpe7 = rpe_weight(estimate, 7, reps_for_estimates, TRUE),
           rpe8 = rpe_weight(estimate, 8, reps_for_estimates, TRUE),
           rpe9 = rpe_weight(estimate, 9, reps_for_estimates, TRUE))

## average RPE should be a weighted average
# slog %>% 
#     group_by(date, movement) %>% 
#     summarise(mean_rpe = mean(rpe)) %>% 
#     ggplot(aes(date, mean_rpe, color = movement)) +
#         geom_line()

## Updates ---------------------------------------------------------------------

# volume by exercise
str_log <- str_log %>% 
    mutate(
        wk = lubridate::week(date)
        , volume = reps * sets
        , tonnage = volume * weight
    ) 

weekly_summary <- str_log %>% 
    group_by(wk, movement) %>% 
    summarise(
        vol = sum(volume)
        , ton = sum(tonnage)
        , max_rep = max(weight)
        , e1rm = mean(e1rm)
    )

# intensity, maybe?
weekly_tidy <- weekly_summary %>% 
    gather("metric", "value", vol:e1rm)

weekly_tidy %>% 
    filter(metric %in% c("max_rep", "e1rm")) %>% 
    ggplot(aes(wk, value, color = metric)) +
    geom_line() +
    facet_wrap(~movement)

# eh, don't love this
min_max <- weekly_tidy %>% 
    group_by(movement, metric) %>% 
    summarise(min_val = min(value), max_val = max(value))

weekly_tidy %>% 
    filter(metric %in% c("vol", "ton")) %>%
    inner_join(min_max) %>% 
    mutate(normalized = (value - min_val) / (max_val - min_val)) %>% 
    ggplot(aes(wk, normalized, color = metric)) + 
    geom_line() +
    facet_wrap(~movement)

# last week's weights  
str_log %>% 
    filter(wk == lubridate::week(today()) - 1) %>% 
    group_by(movement, variation, reps, rpe) %>% 
    summarise(wt = max(weight)) %>% 
    spread(key = rpe, value = wt, sep = "")
