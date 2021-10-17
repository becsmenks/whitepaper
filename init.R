# HEADER ------------------------------------------------------------------
# Project: Charging Optimization Whitepaper
# Date: May 2021
# Description: This script contains the libraries and functions needed to run
#              the widget.
# Sections: A - Import Libraries
#           B - Define Filepaths
#           C - Define Functions
#           D - Declare Variables

# A - Import Libraries ----------------------------------------------------

library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(shiny)
library(shinydashboard)
library(plotly)
library(ggimage)

# B - Define Filepaths ----------------------------------------------------

simulations_filepath <- 'data/simulations.csv'

# C - Define Functions ----------------------------------------------------

# Function to create the network grid
make_grid <- function(distance, step_size, shape) {
  g <- data.frame(x = seq(from = -distance,
                          to = distance,
                          by = step_size)) %>%
    merge(data.frame(y = seq(from = -distance,
                             to = distance,
                             by = step_size)))
  
  switch(shape,
         diamond = g %>%
           mutate(dist = abs(x) + abs(y)) %>%
           filter(dist <= distance),
         square = g %>%
           mutate(dist = pmax(abs(x), abs(y))) %>%
           filter(dist <= distance))
}

# Function to locate chargers on the grid based on how far a vehicle can drive
locate_chargers <- function(distance, batt_size) {
  locs <- seq(from = 0,
              to = distance,
              by = batt_size)
  unique(c(locs, distance))
}

# Function to count number of chargers
count_num_locations <- function(network, locations) {
  network %>%
    filter(dist %in% locations) %>%
    nrow()
}

# Function to count number of vehicles
count_num_vehicles <- function(num_locations, chr2loc_ratio, veh2chrg_ratio) {
  num_locations * chr2loc_ratio* veh2chrg_ratio
}

# Function to calculate total charger cost
calc_total_charger_cost <- function(num_locations, chr2loc_ratio, cost) {
  num_locations * chr2loc_ratio * cost # locs * chg/loc * $/chg = $
}

# Function to calculate total battery cost
calc_total_battery_cost <- function(num_vehicles, cost, total_range, 
                                    daily_range = 100) {
  num_vehicles * max(total_range - daily_range, 0) * cost # vehs * mi/veh * $/mi = $
}


# D - Declare Variables ---------------------------------------------------

values <- list(
  network_distance = list(
    min = 100,
    max = 900,
    by = 100,
    selected = 500
  ),
  battery_cost = list(
    min = 30,
    max = 70,
    by = 5,
    selected = 50
  ),
  charger_cost = list(
    min = 20000,
    max = 180000,
    by = 20000,
    selected = 100000
  ),
  battery_size = list(
    min = 50,
    max = 450,
    by = 50,
    selected = 250
  ),
  veh2chr_ratio = list(
    min = 200,
    max = 600,
    by = 50,
    selected = 400
  ),
  chr2loc_ratio = list(
    min = 4,
    max = 20,
    by = 2,
    selected = 12
  )
)

group_var_choices <- c('Network distance' = 'Distance', 
                       'Vehicle range' = 'Range',
                       "Vehicles per charger" = "Vehicles per Charger",              
                       "Chargers per site" = 'Chargers per Site',
                       "Battery unit cost" = 'Battery Cost',
                       "Charger install unit cost" = 'Charger Cost')

operator_choices = c('Median' = 'median',
                     'Mean' = 'mean')

