# Libraries and inputs ----
library(decisionSupport)
library(tidyverse)
library("readxl")


input_estimates <- read_excel("input_nuts-small.xlsx")

years <- 30


# 3 Szenarios: Trees without harvest, nuts+hay (70 trees) nuts only (200 trees)

# Model Function ----

model_function <- function() {
  
  # General ----
  
  nuts_frost <- chance_event(
    chance = 0.2,
    value_if = 0.2,
    value_if_not = 1,
    n = years
  )
  
  nut_price <- vv(var_mean = nut_price,
                    var_CV = 20,
                    n = years)
  
  days_irrigation <- chance_event(chance = 0.5,
                                  value_if = 1,
                                  value_if_not = 0,
                                  n = 60)
  
  general_investments_vec <- vv(var_mean = maintaining_fences_cost,
                           var_CV = 30,
                           n = years)
  ###
  general_investments_vec[1] <- general_investments_vec[1] + 
    grass_planting_cost + initial_fences_cost
  ###
  
  # Yield Nuts ----
  # Version 1
  
  nuts_1 <- gompertz_yield(
    max_harvest = nut_yield,
    time_to_first_yield_estimate = 6,
    first_yield_estimate_percent = 50,
    time_to_second_yield_estimate = 10,
    second_yield_estimate_percent = 100,
    n_years = years,
    var_CV = 10)
    
  nuts_yield_vec_1 <- Map("*", nuts_1, nuts_frost)
  
  harvest_count_1 <- ifelse(nuts_yield_vec_1 < 20, 0, 1)
  
  ###
  nut_income_vec_1 <- Map("*", nuts_yield_vec_1, nut_price_1)
  ###
    
  # Version 2
    
  nuts_2 <- gompertz_yield(
    max_harvest = nut_yield,
    time_to_first_yield_estimate = 6,
    first_yield_estimate_percent = 50,
    time_to_second_yield_estimate = 10,
    second_yield_estimate_percent = 100,
    n_years = years,
    var_CV = 10)
    
  nuts_yield_vec_2 <- Map("*", nuts_2, nuts_frost)
  
  harvest_count_2 <- ifelse(nuts_yield_vec_2 < 20, 0, 1)
  
  ###
  nut_income_vec_2 <- Map("*", nuts_yield_vec_2, nut_price)
  ###
    
  # Nuts Costs ----
  # Version 1
  
  maintaining_tree_h_vec_1 <- vv(var_mean = maintaining_trees_hours,
                               var_CV = 15,
                               n = years)
  
  nut_harvest_h_vec_1 <- vv(var_mean = nut_harvest_hours,
                            var_CV = 5,
                            n = years)
  
  nut_harvest_h_vec_1 <- Map("*", nut_harvest_h_vec_1, harvest_count_1)
  
  nut_h_vec_1 <- maintaining_tree_h_vec_1 + nut_harvest_h_vec_1
  
  nut_h_vec_1[1] <- nut_h_vec_1[1] + tree_planting_hours
  
  nut_workcosts_vec_1 <- nut_h_vec_1 * working_hours_costs
  
  nut_var_costs_vec_1 <- vv(var_mean = nut_var_costs,
                            var_CV = 20,
                            n = years)
  
  nut_var_costs_vec_1[1] <- nut_var_costs_vec_1[1] + tree_planting_cost
  ###
  nut_costs_vec_1 <- nut_workcosts_vec_1 + nut_var_costs_vec_1
  ###
  
  # Version 2
  
  maintaining_tree_h_vec_2 <- vv(var_mean = maintaining_trees_hours,
                                 var_CV = 15,
                                 n = years)
  
  nut_harvest_h_vec_2 <- vv(var_mean = nut_harvest_hours,
                            var_CV = 5,
                            n = years)
  
  nut_harvest_h_vec_2 <- Map("*", nut_harvest_h_vec_2, harvest_count_2)
  
  nut_h_vec_2 <- maintaining_tree_h_vec_2 + nut_harvest_h_vec_2
  
  nut_h_vec_2[1] <- nut_h_vec_2[1] + tree_planting_hours
  
  nut_workcosts_vec_2 <- nut_h_vec_2 * working_hours_costs
  
  nut_var_costs_vec_2 <- vv(var_mean = nut_var_costs,
                            var_CV = 20,
                            n = years)
  
  nut_var_costs_vec_2[1] <- nut_var_costs_vec_2[1] + tree_planting_cost
  ###
  nut_costs_vec_2 <- nut_workcosts_vec_2 + nut_var_costs_vec_2
  ###
  
  # Irrigation ----
  
  days_irrigation <- vv(var_mean = days_to_irrigate,
                        var_CV = 80,
                        n = years)
  
  days_irrigation[days_irrigation < 10] <- 10
  
  days_irrigation[days_irrigation > 55] <- 55
  
  irrigation_h_vec <- days_irrigation * work_per_irrigation
  
  irrigation_installation <- water_trailer + installation_irrigation
  
  # Version 1
  
  water_usage_vec_1 <- days_irrigation * water_per_day
  
  trailer_refills_vec_1 <- water_usage_vec_1 / trailer_capacity
  
  trailer_refills_vec_1 <- trailer_refills_vec_1 + 3
  
  refill_h_vec_1 <- trailer_refills_vec_1 * work_per_trailer
  
  irrigation_h_vec_1 <- refill_h_vec_1 + irrigation_h_vec
  
  irrigation_costs_1 <- irrigation_h_vec_1 * working_hours_costs
  
  ###
  irrigation_costs_1[1] <- irrigation_costs_1[1] + irrigation_installation
  ###
  
  # Version 2
  
  water_usage_vec_2 <- days_irrigation * water_per_day
  
  trailer_refills_vec_2 <- water_usage_vec_2 / trailer_capacity
  
  trailer_refills_vec_2 <- trailer_refills_vec_2 + 3
  
  refill_h_vec_2 <- trailer_refills_vec_2 * work_per_trailer
  
  irrigation_h_vec_2 <- refill_h_vec_2 + irrigation_h_vec
  
  irrigation_costs_2 <- irrigation_h_vec_2 * working_hours_costs
  
  ###
  irrigation_costs_2[1] <- irrigation_costs_2[1] + irrigation_installation
  ###
  
  # Nuts Final ----
  
  # Version 1
  
  nuts_costs_vec_1 <- nut_costs_vec_1 + irrigation_costs_1
  
  ###
  nut_profit_vec_1 <- nut_income_vec_1 - nuts_costs_vec_1
  ###
  
  # Version 2
  
  nuts_costs_vec_2 <- nut_costs_vec_2 + irrigation_costs_2
  
  ###
  nut_profit_vec_2 <- nut_income_vec_2 - nuts_costs_vec_2
  ###
  
}




