# HEADER ------------------------------------------------------------------
# Project: Charging Optimization Whitepaper
# Date: May 2021
# Description: This script looks a hypothetical network of specified size and
#              calculates the total cost to serve that network either by (1)
#              increasing vehicle range or (2) installing chargers. Which option
#              is more cost-effective depends on the unit costs for batteries
#              and chargers, the network distance, and the initially assumed
#              vehicle range. The script produces a widget that allows the user
#              to vary these parameters and calculate resulting costs.
# Sections: A - Setup Code
#           B - Make Shiny UI
#           C - Make Shiny Server
#           D - Run Shiny App

# A - Setup Code ----------------------------------------------------------

source('init.R')

sim_results_summarized <- read.csv(simulations_filepath)

# B - Make Shiny UI -------------------------------------------------------

# Define UI for charging optimization widget
ui <- fluidPage(
  
  # App title
  titlePanel("Charging Optimization Example"),
  
  # Sidebar layout with input and output definitions
  fluidRow(
    # Sidebar panel for inputs
    column(width = 3,
           # Slider styles go here
           tags$style(HTML(".js-irs-0 .irs-single, .js-irs-0 .irs-bar-edge, .js-irs-0 .irs-bar {background: #3f840b; border-top: #3f840b; border-bottom: #3f840b;}")),
           tags$style(HTML(".js-irs-1 .irs-single, .js-irs-1 .irs-bar-edge, .js-irs-1 .irs-bar {background: #3f840b; border-top: #3f840b; border-bottom: #3f840b;}")),
           tags$style(HTML(".js-irs-2 .irs-single, .js-irs-2 .irs-bar-edge, .js-irs-2 .irs-bar {background: #3f840b; border-top: #3f840b; border-bottom: #3f840b;}")),
           tags$style(HTML(".js-irs-3 .irs-single, .js-irs-3 .irs-bar-edge, .js-irs-3 .irs-bar {background: #3f840b; border-top: #3f840b; border-bottom: #3f840b;}")),
           tags$style(HTML(".js-irs-4 .irs-single, .js-irs-4 .irs-bar-edge, .js-irs-4 .irs-bar {background: #3f840b; border-top: #3f840b; border-bottom: #3f840b;}")),
           tags$style(HTML(".js-irs-5 .irs-single, .js-irs-5 .irs-bar-edge, .js-irs-5 .irs-bar {background: #3f840b; border-top: #3f840b; border-bottom: #3f840b;}")),
           
           h3("Ecosystem Inputs"),
           # Input: Slider for the network distance
           sliderInput(inputId = "network_distance",
                       label = "Network distance (miles):",
                       min = values$network_distance$min,
                       max = values$network_distance$max,
                       value = values$network_distance$selected,
                       step = values$network_distance$by),
           # Input: Slider for battery size
           sliderInput(inputId = "battery_size",
                       label = "Vehicle range (miles):",
                       min = values$battery_size$min,
                       max = values$battery_size$max,
                       value = values$battery_size$selected,
                       step = values$battery_size$by),
           # Input: Slider for vehicle to charger ratio
           sliderInput(inputId = "veh2chr_ratio",
                       label = "Vehicles per charger:",
                       min = values$veh2chr_ratio$min,
                       max = values$veh2chr_ratio$max,
                       value = values$veh2chr_ratio$selected,
                       step = values$veh2chr_ratio$by),
           # Input: Slider for vehicle to charger ratio
           sliderInput(inputId = "chr2loc_ratio",
                       label = "Chargers per site:",
                       min = values$chr2loc_ratio$min,
                       max = values$chr2loc_ratio$max,
                       value = values$chr2loc_ratio$selected,
                       step = values$chr2loc_ratio$by),
           
           h3("Market Inputs"),
           # Input: Slider for battery cost
           sliderInput(inputId = "battery_cost",
                       label = "Battery unit cost ($/mile):",
                       min = values$battery_cost$min,
                       max = values$battery_cost$max,
                       value = values$battery_cost$selected,
                       step = values$battery_cost$by),
           # Input: Slider for charger cost
           sliderInput(inputId = "charger_cost",
                       label = "Charger install unit cost ($/charger):",
                       min = values$charger_cost$min,
                       max = values$charger_cost$max,
                       value = values$charger_cost$selected,
                       step = values$charger_cost$by),
           
           h3("Chart Controls"),
           # Input: Chart Grouping Variable
           selectInput(inputId = 'select_group_var',
                       label = 'Summary dimension:',
                       choices = group_var_choices),
           selectInput(inputId = 'select_group_op',
                       label = "Summary operator:",
                       choices = operator_choices)
           ),
    # Second column for plots and results
    column(width = 9,
           # First results row with grid plot and KPIs
           fluidRow(
             # Grid plot
             column(
               width = 5,
               h3("Charging Ecosystem Visualization"),
               # Output: Grid plot
               plotlyOutput("grid_plot")
             ),
             # KPI boxes
             column(
               width = 3, offset = 1,
               h3("Total Cost (Millions)"),
               br(),
               br(),
               # Output: Battery cost
               fluidRow(infoBoxOutput("battery_cost_kpi")),
               # Output: Charger cost
               fluidRow(infoBoxOutput("charger_cost_kpi"))
             )
           ),
           # Second results row with the full simulation plot
           fluidRow(
             # Simulation plot
             column(width = 9,
                    h3("Cost Trade-off between Charger Installations and Vehicle Range"),
                    # Output: Simulation plot
                    plotlyOutput("sim_plot")
             )
           )
    )
  )
)

# C - Make Shiny Server ---------------------------------------------------

# Define server logic required to calculate costs
server <- function(input, output) {
  # Create the distance grid
  grid <- reactive({
    make_grid(input$network_distance,
              50,
              "diamond")
    })
  # Identify the required locations of chargers
  charger_locations <- reactive({
    locate_chargers(input$network_distance,
                    input$battery_size)
  })
  # Count the number of locations
  num_locations <- reactive({
    count_num_locations(grid(), charger_locations())
  })
  # Count the number of vehicles
  num_vehicles <- reactive({
    count_num_vehicles(num_locations(), input$chr2loc_ratio, input$veh2chr_ratio)
  })
  # Get the total charger installation cost to get to destination on outer edge
  # of the grid
  total_charger_cost <- reactive({
    cost1 = calc_total_charger_cost(num_locations(),
                                    input$chr2loc_ratio,
                                    input$charger_cost)
    
    battery_cost_range = max(input$network_distance - 100, 0)
    charger_cost_range = max(min(input$battery_size, input$network_distance) - 100, 0)
    charger_cost_frac_of_battery_cost = if_else(battery_cost_range == 0, 0, 
                                                charger_cost_range / battery_cost_range)
    cost2 = total_battery_cost() * charger_cost_frac_of_battery_cost
    
    cost1 + cost2
  })
  # Get the total incremental battery cost to get to destination on outer edge
  # of the grid
  total_battery_cost <- reactive({
    calc_total_battery_cost(num_vehicles(),
                            input$battery_cost,
                            input$network_distance)
  })
  # Plot the network
  output$grid_plot <- renderPlotly({
    # Visualize the grid
    ggplotly(
      grid() %>%
        mutate(
          `Location Type` = case_when(
            dist == input$network_distance ~ "Destination",
            dist == 0 ~ "Origin",
            dist %in% charger_locations() ~ "Charging Stop",
            TRUE ~ "Waypoint"),
          `Location Type` = factor(
            `Location Type`,
            levels = c("Origin",
                       "Waypoint",
                       "Charging Stop",
                       "Destination"))) %>%
        rename(Distance = dist) %>%
        ggplot() +
        aes(x = x, y = y,
            label = Distance,
            colour = `Location Type`,
            shape = `Location Type`) +
        geom_point() +
        xlab("") +
        ylab("") +
        theme_bw() +
        theme(axis.text = element_blank(),
              axis.ticks = element_blank(),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank()) +
        scale_shape_manual(values = c("Origin" = "15",
                                      "Waypoint" = "16",
                                      "Charging Stop" = "3",
                                      "Destination" = "15")) +
        scale_color_manual(values = c("Origin" = "black",
                                      "Waypoint" = "blue",
                                      "Charging Stop" = "red",
                                      "Destination" = "#3f840b")),
      tooltip = c("label")
    )
  })
  # Plot the full simulation
  output$sim_plot <- renderPlotly({
    group_var_name <- input$select_group_var
    ggplotly(
      sim_results_summarized %>%
        filter(group_var_name == input$select_group_var,
               operator_name == input$select_group_op) %>% 
        ggplot() +
        aes(x = battery_total_cost,
            y = charger_total_cost,
            color = group_var) +
        geom_point() +
        geom_point(data =
                     tibble(
                       Distance = paste(input$network_distance, "miles"),
                       Range = paste(input$battery_size, "miles"),
                       `Battery Cost` = paste0("$", input$battery_cost, "/mile"),
                       `Charger Cost` = paste0("$", input$charger_cost/1000, "k/charger"),
                       `Vehicles per Charger` = paste(input$veh2chr_ratio),
                       `Chargers per Site` = paste(input$chr2loc_ratio),
                       battery_total_cost = total_battery_cost() / 1000000,
                       charger_total_cost = total_charger_cost() / 1000000
                     ) %>% 
                     rename(c('group_var' = group_var_name)) %>% 
                     select(group_var, battery_total_cost, charger_total_cost),
                   shape = 18,
                   size = 5) +
        geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
        xlab(paste(str_to_title(input$select_group_op), 
                   "Total Battery Cost (Millions)")) +
        ylab(paste(str_to_title(input$select_group_op), 
                   "Total Charger Cost (Millions)")) +
        theme_bw() +
        scale_x_continuous(labels = scales::dollar_format()) +
        scale_y_continuous(labels = scales::dollar_format()) + 
        guides(color = guide_legend(title=group_var_name)) + 
        expand_limits(x = 0, y = 0),
      tooltip = c("Distance")
    )
  })
  # Calculate total battery cost
  output$battery_cost_kpi <- renderInfoBox({
    # Create the info box
    infoBox(
      HTML(paste0('<font color="#3f840b" size =6><b>$',
                  format(round(total_battery_cost() / 1000000),
                         big.mark = ",",
                         scientific = FALSE),
                  '</b></font>')),
      icon = icon("battery-full")
    )
  })
  # Calculate total charger cost
  output$charger_cost_kpi <- renderInfoBox({
    # Create the info box
    infoBox(
      HTML(paste0('<font color="#3f840b" size =6><b>$',
                  format(round(total_charger_cost() / 1000000),
                         big.mark = ",",
                         scientific = FALSE),
                  '</b></font>')),
      icon = icon("charging-station")
    )
  })
}



# D - Run Shiny App -------------------------------------------------------

shinyApp(ui, server)

