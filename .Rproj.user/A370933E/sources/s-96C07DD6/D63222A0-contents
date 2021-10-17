# HEADER ------------------------------------------------------------------
# Project: Charging Optimization Whitepaper
# Date: May 2021
# Description: This script pre-runs the simulations for the widget.
# Sections: A - Setup Code
#           B - Run Simulations
#           C - Summarize Results
#           D - Save Data

# A - Setup Code ----------------------------------------------------------

source('init.R')


# B - Run Simulation ------------------------------------------------------

sim_results_battery <- NULL
sim_results_charger <- NULL

# Loop through network distances
for (d in seq(from = values$network_distance$min,
              to = values$network_distance$max,
              by = values$network_distance$by)) {
  print(paste("Simulating network distance:", d, "miles"))
  grid <- make_grid(distance = d,
                    step_size = 50,
                    shape = "diamond")
  # Loop through battery sizes
  for (s in seq(from = values$battery_size$min,
                to = values$battery_size$max,
                by = values$battery_size$by)) {
    locations <- locate_chargers(d, s)
    num_locations <- count_num_locations(grid, locations)
    # Loop through chargers per site
    for (r1 in seq(from = values$chr2loc_ratio$min,
                   to = values$chr2loc_ratio$max,
                   by = values$chr2loc_ratio$by)) {
      # Loop through vehicles per charger
      for (r2 in seq(from = values$veh2chr_ratio$min,
                     to = values$veh2chr_ratio$max,
                     by = values$veh2chr_ratio$by)) {
        num_vehicles <- count_num_vehicles(num_locations, r1, r2)
        # Loop through battery costs
        for (bc in seq(from = values$battery_cost$min,
                       to = values$battery_cost$max,
                       by = values$battery_cost$by)) {
          total_battery_cost <- calc_total_battery_cost(num_vehicles, bc, d)
          # Make results dataframe
          sim_results_battery <- tibble(
            network_distance = d,
            battery_size = s,
            veh2chr_ratio = r2,
            chr2loc_ratio = r1,
            battery_unit_cost = bc,
            battery_total_cost = total_battery_cost
          ) %>%
            bind_rows(sim_results_battery)
        }
        # Loop through charger costs
        for (cc in seq(from = values$charger_cost$min,
                       to = values$charger_cost$max,
                       by = values$charger_cost$by)) {
          total_charger_cost <- calc_total_charger_cost(num_locations, r1, cc)
          # Make results dataframe
          sim_results_charger <- tibble(
            network_distance = d,
            battery_size = s,
            chr2loc_ratio = r1,
            charger_unit_cost = cc,
            charger_total_cost = total_charger_cost
          ) %>%
            bind_rows(sim_results_charger)
        }
      }
    }
  }
}


# C - Summarize Results ---------------------------------------------------

sim_results <- sim_results_battery %>%
  left_join(sim_results_charger, by = c("network_distance",
                                        "battery_size",
                                        "chr2loc_ratio")) %>% 
  rename(charger_total_cost_low = charger_total_cost) %>% 
  mutate(battery_cost_range = pmax(network_distance - 100, 0),
         charger_cost_range = pmax(pmin(battery_size, network_distance) - 100, 0),
         charger_cost_frac_of_battery_cost = if_else(
           battery_cost_range == 0, 0, charger_cost_range / battery_cost_range),
         charger_total_cost_adder = charger_cost_frac_of_battery_cost * battery_total_cost,
         charger_total_cost = charger_total_cost_low + charger_total_cost_adder) %>%
  mutate(Distance = paste(network_distance, "miles"),
         Range = paste(battery_size, "miles"),
         `Battery Cost` = paste0("$", battery_unit_cost, "/mile"),
         `Charger Cost` = paste0("$", charger_unit_cost/1000, "k/charger"),
         `Vehicles per Charger` = paste(veh2chr_ratio),
         `Chargers per Site` = paste(chr2loc_ratio))

sim_results_summarized <- NULL
for (group_var_name in group_var_choices) {
  for (operator_name in operator_choices) {
    sim_results_summarized <- sim_results %>% 
      rename(c('group_var' = group_var_name)) %>% 
      group_by(group_var) %>%
      summarise_at(c('battery_total_cost', 'charger_total_cost'),
                   operator_name) %>%
      ungroup() %>%
      mutate(battery_total_cost = battery_total_cost / 1000000,
             charger_total_cost = charger_total_cost / 1000000,
             group_var_name = group_var_name,
             operator_name = operator_name) %>% 
      bind_rows(sim_results_summarized)
  }
}

# D - Save Data -----------------------------------------------------------

write.csv(sim_results_summarized, simulations_filepath, row.names = F)
  