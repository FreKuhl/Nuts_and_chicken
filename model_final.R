# Libraries and inputs ----

library(decisionSupport)
library(tidyverse)
library(ggpubr)
library("readxl")

input_estimates <- read_excel("input_estimates.xlsx")
discount_rate = 3
years <- 30 # IMPORTANT! Select ONLY steps of 10
number_of_chicken = 50 # Select 50 or 200

# make variables ----
# Use only for test runs

make_variables <- function(est,n=1)
{ x<-random(rho=est, n=n)
for(i in colnames(x)) assign(i,
                             as.numeric(x[1,i]),envir=.GlobalEnv)
}

make_variables(as.estimate(input_estimates))



# Model Function ----

# 5 Scenarios: 
# 1: nuts+hay+chicken (70 trees)
# 2: nuts+chicken (200 trees)
# 3: Truffle Trees + chicken (200 trees)
# 4: nuts+hay+chicken+truffle (70 trees)
# 5: nuts+chicken+truffle (200 trees)


model_function <- function() {
  
  # General ----
  
  full_yield_period <- years - 8
  
  nuts_frost <- chance_event(
    chance = late_frost,
    value_if = 0.3,
    value_if_not = 1,
    n = years
  )
  
  nut_price <- vv(var_mean = nut_price,
                  var_CV = 20,
                  n = years)
  
  subsidies_vec <- vv(var_mean = subsidies,
                  var_CV = 1,
                  n = years)
  
  general_investments_vec <- vv(var_mean = maintaining_fences_cost,
                                var_CV = 30,
                                n = years)
  
  ###
  general_investments_vec[1] <- general_investments_vec[1] + 
    grass_planting_cost + initial_fences_cost
  ###
  
  # Baseline ----
  
  #Winterbarley
  winterbarley_yield_vec <- vv(var_mean = winterbarley_yield,
                               var_CV = 10,
                               n = years)
  winterbarley_costs_vec <- vv(var_mean = winterbarley_costs,
                               var_CV = 5,
                               n = years)
  winterbarley_work_vec <- vv(var_mean = winterbarley_work,
                              var_CV = 5,
                              n = years)
  winterbarley_price_vec <- vv(var_mean = winterbarley_price,
                               var_CV = 10,
                               n = years)
  
  winterbarley_income_vec <- (winterbarley_yield_vec * winterbarley_price_vec) -
    (winterbarley_costs_vec + (winterbarley_work_vec * working_hours_costs))
  
  # Rye
  rye_yield_vec <- vv(var_mean = rye_yield,
                               var_CV = 10,
                               n = years)
  rye_costs_vec <- vv(var_mean = rye_costs,
                               var_CV = 5,
                               n = years)
  rye_work_vec <- vv(var_mean = rye_work,
                              var_CV = 5,
                              n = years)
  rye_price_vec <- vv(var_mean = rye_price,
                               var_CV = 10,
                               n = years)
  
  rye_income_vec <- (rye_yield_vec * rye_price_vec) -
    (rye_costs_vec + (rye_work_vec * working_hours_costs))
  
  # Wheat
  wheat_yield_vec <- vv(var_mean = wheat_yield,
                               var_CV = 10,
                               n = years)
  wheat_costs_vec <- vv(var_mean = wheat_costs,
                               var_CV = 5,
                               n = years)
  wheat_work_vec <- vv(var_mean = wheat_work,
                              var_CV = 5,
                              n = years)
  wheat_price_vec <- vv(var_mean = wheat_price,
                               var_CV = 10,
                               n = years)
  
  wheat_income_vec <- (wheat_yield_vec * wheat_price_vec) -
    (wheat_costs_vec + (winterbarley_work_vec * wheat_work_vec))
  
  # Summerbarley
  summerbarley_yield_vec <- vv(var_mean = summerbarley_yield,
                               var_CV = 10,
                               n = years)
  summerbarley_costs_vec <- vv(var_mean = summerbarley_costs,
                               var_CV = 5,
                               n = years)
  summerbarley_work_vec <- vv(var_mean = summerbarley_work,
                              var_CV = 5,
                              n = years)
  summerbarley_price_vec <- vv(var_mean = summerbarley_price,
                               var_CV = 10,
                               n = years)
  
  summerbarley_income_vec <- (summerbarley_yield_vec * summerbarley_price_vec) -
    (summerbarley_costs_vec + (summerbarley_work_vec * working_hours_costs))
  
  
  baseline_vec <- winterbarley_income_vec
  
  baseline_vec[2] <- rye_income_vec[2]
  baseline_vec[6] <- rye_income_vec[6]
  baseline_vec[10] <- rye_income_vec[10]
  baseline_vec[14] <- rye_income_vec[14]
  baseline_vec[18] <- rye_income_vec[18]
  baseline_vec[22] <- rye_income_vec[22]
  baseline_vec[26] <- rye_income_vec[26]
  baseline_vec[30] <- rye_income_vec[30]
  
  baseline_vec[3] <- wheat_income_vec[3]
  baseline_vec[7] <- wheat_income_vec[7]
  baseline_vec[11] <- wheat_income_vec[11]
  baseline_vec[15] <- wheat_income_vec[15]
  baseline_vec[19] <- wheat_income_vec[19]
  baseline_vec[23] <- wheat_income_vec[23]
  baseline_vec[27] <- wheat_income_vec[27]
  
  baseline_vec[4] <- summerbarley_income_vec[4]
  baseline_vec[8] <- summerbarley_income_vec[8]
  baseline_vec[12] <- summerbarley_income_vec[12]
  baseline_vec[16] <- summerbarley_income_vec[16]
  baseline_vec[20] <- summerbarley_income_vec[20]
  baseline_vec[24] <- summerbarley_income_vec[24]
  baseline_vec[28] <- summerbarley_income_vec[28]
  
  ###
  baseline_vec <- baseline_vec + subsidies_vec
  baseline_vec <- discount(baseline_vec, discount_rate)
  baseline <- Reduce("+", baseline_vec)
  ###
  
  # Yield Nuts ----
  # For Versions 1 and 4
  
  nuts_1 <- gompertz_yield(
    max_harvest = nut_yield_1,
    time_to_first_yield_estimate = 6,
    first_yield_estimate_percent = 40,
    time_to_second_yield_estimate = 10,
    second_yield_estimate_percent = 100,
    n_years = years,
    var_CV = 10)
  
  nuts_yield_vec_1 <- nuts_1 * nuts_frost
  
  harvest_count_1 <- ifelse(nuts_yield_vec_1 < 20, 0, 1)
  
  amount_bales <- vv(var_mean = amount_bales_1,
                     var_CV = 10,
                     n = years)
  
  income_per_bale <- vv(var_mean = income_bale_1,
                        var_CV = 10,
                        n = years)
  
  income_hay <- amount_bales * income_per_bale
  
  ###
  nut_income_vec_1_4 <- (nuts_yield_vec_1 * nut_price) + income_hay
  ###
  
  # For Versions 2 and 5
  
  nuts_2 <- gompertz_yield(
    max_harvest = nut_yield_2,
    time_to_first_yield_estimate = 6,
    first_yield_estimate_percent = 40,
    time_to_second_yield_estimate = 10,
    second_yield_estimate_percent = 100,
    n_years = years,
    var_CV = 10)
  
  nuts_yield_vec_2 <-nuts_2 * nuts_frost
  
  harvest_count_2 <- ifelse(nuts_yield_vec_2 < 20, 0, 1)
  
  ###
  nut_income_vec_2_5 <- (nuts_yield_vec_2 * nut_price)
  ###
  
  # Nuts Costs ----
  # For Versions 1 and 4
  
  maintaining_tree_h_vec_1 <- vv(var_mean = maintaining_trees_h_1,
                                 var_CV = 15,
                                 n = years)
  
  maintaining_tree_h_vec_1[8:full_yield_period] <- 
    maintaining_tree_h_vec_1[8:full_yield_period] * maintaining_trees_factor
  
  nut_harvest_h_vec_1 <- vv(var_mean = nut_harvest_hours_1,
                            var_CV = 5,
                            n = years)
  
  nut_harvest_h_vec_1 <- nut_harvest_h_vec_1 * harvest_count_1
  
  nut_mulch_h_vec_1 <- vv(var_mean = mulch_h_1,
                          var_CV = 5,
                          n = years)
  
  other_nut_h_1 <- vv(var_mean = other_nut_h_1,
                      var_CV = 5,
                      n = years)
  
  nut_h_vec_1 <- maintaining_tree_h_vec_1 + nut_harvest_h_vec_1 + 
    nut_mulch_h_vec_1 + other_nut_h_1
  
  nut_h_vec_1[1] <- nut_h_vec_1[1] + tree_planting_hours_1
  
  nut_workcosts_vec_1 <- nut_h_vec_1 * working_hours_costs
  
  nut_other_costs_vec_1 <- vv(var_mean = nut_var_costs_1,
                              var_CV = 20,
                              n = years)
  
  nut_other_costs_vec_1[1] <- nut_other_costs_vec_1[1] + tree_planting_costs_1
  
  harvest_nets_1 <- vv(var_mean = harvest_nets_1,
                       var_CV = 1,
                       n = years)
  
  harvest_nets_1[] <- harvest_nets_1 *
    c(TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE)
  
  nut_other_costs_vec_1 <- nut_other_costs_vec_1 + harvest_nets_1
  
  nut_other_costs_vec_1[1] <- nut_other_costs_vec_1[1] + tree_planting_costs_1
  
  nut_other_costs_vec_4 <- nut_other_costs_vec_1
  
  nut_other_costs_vec_4[1] <- 
    nut_other_costs_vec_4[1] + truffle_tree_planting_costs_1
  
  hay_costs <- vv(var_mean = hay_costs_1,
                  var_CV = 5,
                  n = years)
  
  replace_trees_1 <- chance_event(chance = 0.1,
                                  value_if = 3,
                                  value_if_not = 0,
                                  n = years,
                                  CV_if = 30)
  
  replace_trees_1 <- replace_trees_1 * replacing_trees_cost
  
  replace_trees_4 <- replace_trees_1 * replacing_truffle_trees_cost
  
  ###
  nut_costs_vec_1 <- nut_workcosts_vec_1 + nut_other_costs_vec_1 + 
    hay_costs + soil_analysis +replace_trees_1
  
  nut_costs_vec_4 <- nut_workcosts_vec_1 + nut_other_costs_vec_4 + 
    hay_costs + soil_analysis +replace_trees_4
  ###
  
  # For Versions 2 and 5
  
  maintaining_tree_h_vec_2 <- vv(var_mean = maintaining_trees_h_2,
                                 var_CV = 15,
                                 n = years)
  
  maintaining_tree_h_vec_2[8:full_yield_period] <- 
    maintaining_tree_h_vec_2[8:full_yield_period] * maintaining_trees_factor
  
  nut_harvest_h_vec_2 <- vv(var_mean = nut_harvest_hours_2,
                            var_CV = 5,
                            n = years)
  
  nut_harvest_h_vec_2 <- nut_harvest_h_vec_2 * harvest_count_2
  
  nut_mulch_h_vec_2 <- vv(var_mean = mulch_h_2,
                          var_CV = 5,
                          n = years)
  
  other_nut_h_2 <- vv(var_mean = other_nut_h_2,
                      var_CV = 5,
                      n = years)
  
  nut_h_vec_2 <- maintaining_tree_h_vec_2 + nut_harvest_h_vec_2 + 
    nut_mulch_h_vec_2 + other_nut_h_2
  
  nut_h_vec_2[1] <- nut_h_vec_2[1] + tree_planting_hours_2
  
  nut_workcosts_vec_2 <- nut_h_vec_2 * working_hours_costs
  
  nut_other_costs_vec_2 <- vv(var_mean = nut_var_costs_2,
                              var_CV = 20,
                              n = years)
  
  harvest_nets_2 <- vv(var_mean = harvest_nets_2,
                       var_CV = 1,
                       n = years)
  
  harvest_nets_2[] <- harvest_nets_2 *
    c(TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE)
  
  nut_other_costs_vec_2 <- nut_other_costs_vec_2 + harvest_nets_2
  
  nut_other_costs_vec_2[1] <- nut_other_costs_vec_2[1] + tree_planting_costs_2
  
  nut_other_costs_vec_5 <- nut_other_costs_vec_2
  
  nut_other_costs_vec_5[1] <- 
    nut_other_costs_vec_5[1] + truffle_tree_planting_costs_2
  
  replace_trees_2 <- chance_event(chance = 0.1,
                                  value_if = 9,
                                  value_if_not = 0,
                                  n = years,
                                  CV_if = 30)
  
  replace_trees_2 <- replace_trees_2 * replacing_trees_cost
  
  replace_trees_5 <- replace_trees_2 * replacing_truffle_trees_cost
  
  ###
  nut_costs_vec_2 <- nut_workcosts_vec_2 + nut_other_costs_vec_2 + 
    soil_analysis + replace_trees_2
  
  nut_costs_vec_5 <- nut_workcosts_vec_2 + nut_other_costs_vec_5 + 
    soil_analysis + replace_trees_5
  ###
  
  # Irrigation ----
  
  days_irrigation <- vv(var_mean = days_to_irrigate,
                        var_CV = 60,
                        n = years)
  
  days_irrigation[days_irrigation < 10] <- 10
  
  days_irrigation[days_irrigation > 55] <- 55
  
  irrigation_h_vec <- days_irrigation * work_per_irrigation
  
  # For Versions 1 and 4
  
  irrigation_installation_1 <- water_trailer + installation_irrigation_1
  
  water_usage_vec_1 <- days_irrigation * water_per_day_1
  
  trailer_refills_vec_1 <- water_usage_vec_1 / trailer_capacity
  
  trailer_refills_vec_1 <- trailer_refills_vec_1 + 3
  
  refill_h_vec_1 <- trailer_refills_vec_1 * work_per_trailer
  
  irrigation_h_vec_1 <- refill_h_vec_1 + irrigation_h_vec
  
  irrigation_costs_1 <- irrigation_h_vec_1 * working_hours_costs
  
  maintaining_irrigation_1 <- vv(var_mean = maintaining_irrigation_1,
                                 var_CV = 10,
                                 n = years)
  
  water_costs_1 <- water_usage_vec_1 * water_price
  
  irrigation_costs_1_4 <- 
    irrigation_costs_1 + maintaining_irrigation_1 + water_costs_1
  
  ###
  irrigation_costs_1_4[1] <- irrigation_costs_1_4[1] + irrigation_installation_1
  ###
  
  # For Versions 2 and 5
  
  irrigation_installation_2 <- water_trailer + installation_irrigation_2
  
  water_usage_vec_2 <- days_irrigation * water_per_day_2
  
  trailer_refills_vec_2 <- water_usage_vec_2 / trailer_capacity
  
  trailer_refills_vec_2 <- trailer_refills_vec_2 + 3
  
  refill_h_vec_2 <- trailer_refills_vec_2 * work_per_trailer
  
  irrigation_h_vec_2 <- refill_h_vec_2 + irrigation_h_vec
  
  irrigation_costs_2 <- irrigation_h_vec_2 * working_hours_costs
  
  maintaining_irrigation_2 <- vv(var_mean = maintaining_irrigation_2,
                                 var_CV = 10,
                                 n = years)
  
  water_costs_2 <- water_usage_vec_2 * water_price
  
  irrigation_costs_2_5 <- 
    irrigation_costs_2 + maintaining_irrigation_2 + water_costs_2
  
  ###
  irrigation_costs_2_5[1] <- irrigation_costs_2_5[1] + irrigation_installation_2
  ###
  
  # Nuts Final ----
  
  # For Versions 1 and 4
  
  nuts_costs_vec_1 <- nut_costs_vec_1 + irrigation_costs_1_4
  
  nut_costs_vec_4 <- nut_costs_vec_4 + irrigation_costs_1_4
  
  ###
  nut_profit_vec_1 <- nut_income_vec_1_4 - nuts_costs_vec_1
  
  nut_profit_vec_4 <- nut_income_vec_1_4 - nut_costs_vec_4
  ###
  
  # For Versions 2 and 5
  
  nuts_costs_vec_2 <- nut_costs_vec_2 + irrigation_costs_2_5
  
  nut_costs_vec_5 <- nut_costs_vec_5 + irrigation_costs_2_5
  
  ###
  nut_profit_vec_2 <- nut_income_vec_2_5 - nuts_costs_vec_2
  
  nut_profit_vec_5 <- nut_income_vec_2_5 - nut_costs_vec_5
  ###
  
  # Truffle only Trees ----
  # Version 3
  
  maintaining_tree_h_vec_3 <- vv(var_mean = maintaining_trees_h_3,
                                 var_CV = 15,
                                 n = years)
  
  nut_mulch_h_vec_3 <- vv(var_mean = mulch_h_3,
                          var_CV = 5,
                          n = years)
  
  tree_h_vec <- maintaining_tree_h_vec_3 + nut_mulch_h_vec_3
  
  tree_h_vec[1] <- tree_h_vec[1] + tree_planting_hours_3
  
  tree_other_costs_vec <- vv(var_mean = tree_var_costs,
                             var_CV = 20,
                             n = years)
  
  tree_other_costs_vec[1] <- tree_other_costs_vec[1] + tree_planting_costs_3
  
  ###
  truffle_tree_costs <- (tree_h_vec * working_hours_costs) + tree_other_costs_vec
  ###
  
  # Truffle ----
  # For Version 3, 4 and 5
  
  truffle <- gompertz_yield(
    max_harvest = truffle_yield,
    time_to_first_yield_estimate = 5,
    first_yield_estimate_percent = 10,
    time_to_second_yield_estimate = 15,
    second_yield_estimate_percent = 100,
    n_years = years,
    var_CV = 60,
  )
  
  truffle_price <- vv(var_mean = truffle_price,
                      var_CV = 10,
                      n = years)
  
  truffle_income <- truffle * truffle_price
  
  truffle_harvest_costs <- vv(var_mean = truffle_harvest_costs,
                              var_CV = 5,
                              n = years)
  
  truffle_harvest_costs[1:5] <- 0
  
  ###
  truffle_final_vec_4_5 <- truffle_income - truffle_harvest_costs
  ###
  
  # Truffle irrigation for 3
  
  days_irrigation_truffle <- vv(var_mean = truffle_days_to_irrigate,
                        var_CV = 60,
                        n = years)
  
  days_irrigation_truffle[days_irrigation_truffle < 20] <- 20
  
  days_irrigation_truffle[days_irrigation_truffle > 70] <- 70
  
  
  truffle_irrigation_h_vec <- days_irrigation_truffle * work_per_irrigation
  
  water_usage_vec_truffle <- days_irrigation_truffle * truffle_water_per_day
  
  trailer_refills_vec_truffle <- water_usage_vec_truffle / trailer_capacity
  
  trailer_refills_vec_truffle <- trailer_refills_vec_truffle + 3
  
  refill_h_vec_truffle <- trailer_refills_vec_truffle * work_per_trailer
  
  irrigation_h_vec_truffle <- refill_h_vec_truffle + truffle_irrigation_h_vec
  
  irrigation_costs_truffle <- irrigation_h_vec_truffle * working_hours_costs
  
  maintaining_irrigation_truffle <- vv(var_mean = maintaining_irrigation_2,
                                 var_CV = 10,
                                 n = years)
  
  water_costs_truffle <- water_usage_vec_truffle * water_price
  
  irrigation_costs_truffle <- 
    irrigation_costs_truffle + maintaining_irrigation_truffle + water_costs_truffle
  
  irrigation_costs_truffle[1] <- 
    irrigation_costs_truffle[1] + irrigation_installation_2
  
  ###
  truffle_final_vec_3 <- 
    truffle_income - truffle_harvest_costs - irrigation_costs_truffle
  ###
  
  # Chicken ----
  # For Version 1, 2, 3, 4 and 5
  
  if (number_of_chicken == 50){
    
    initial_chicken_costs_final_2 <-
      (number_of_chicken * chicken_replacement_cost) + initial_chicken_mobile_cost_2
    
    maintaining_chicken_mobile_vec <- vv(var_mean = maintaining_chicken_mobile_2,
                                           var_CV = 15,
                                           n = years)
    
    maintaining_chicken_mobile_vec[1] <-
      maintaining_chicken_mobile_vec[1] + initial_chicken_costs_final_2
    
    working_hours_chicken <- vv(var_mean = working_hours_chicken_2,
                                var_CV = 10,
                                n = years)
    
  } else if (number_of_chicken == 200) {
    
    initial_chicken_costs_final_1 <-
      (number_of_chicken * chicken_replacement_cost) + initial_chicken_mobile_cost_1
    
    maintaining_chicken_mobile_vec <- vv(var_mean = maintaining_chicken_mobile_1,
                                           var_CV = 15,
                                           n = years)
    
    maintaining_chicken_mobile_vec[1] <-
      maintaining_chicken_mobile_vec[1] + initial_chicken_costs_final_1
    
    working_hours_chicken <- vv(var_mean = working_hours_chicken_1,
                                var_CV = 10,
                                n = years)
    
  } else {
    print("Number of chicken wrong")
  }
  
  
  chicken_feed_vec <- vv(var_mean = feed_per_hen,
                     var_CV = 5,
                     n = years)
  
  feed_cost_vec <- vv(var_mean = feed_cost,
                  var_CV = 10,
                  n = years)
  
  feed_cost_final_vec <- number_of_chicken * (chicken_feed_vec * feed_cost_vec)
  
  working_costs_chicken_final <-
    working_hours_chicken * working_hours_costs
  
  chicken_replacement <- vv(var_mean = number_of_chicken,
                            var_CV = 2,
                            n = years)
  
  # setting every second year to zero starting with the first 
  chicken_replacement[] <- chicken_replacement * c(FALSE, TRUE)
  
  chicken_replacement_cost_final <-
    chicken_replacement * chicken_replacement_cost
  
  # Income
  
  # Number of eggs per Year
  eggs_per_year <- vv(var_mean = egg_per_hen,
                      var_CV = 2,
                      n = years)
  
  eggs_per_year <- eggs_per_year * number_of_chicken
  
  #
  eggs_price <- vv(
    var_mean = eggs_price,
    var_CV = 5,
    n = years,
    lower_limit = 0.2
  )
  
  eggs_income <- eggs_per_year * eggs_price
  
  chicken_income <-
    eggs_income - (
      maintaining_chicken_mobile_vec + feed_cost_final_vec +
        working_costs_chicken_final + chicken_replacement_cost_final
    )
  
  
  # CO² Certificates ----
  
  # Eggs
  
  co2_emmiter_eggs <- (number_of_chicken * egg_per_hen) * co2_per_egg
  
  co2_emmiter_eggs <- co2_emmiter_eggs / kg_per_certificate
  
  # Version 1 and 4
  
  co2_per_year_1_4 <- 70 * kg_per_bush
  
  certifikates_1_4 <- co2_per_year_1_4 / kg_per_certificate
  
  certifikates_1_4 <- certifikates_1_4 - co2_emmiter_eggs
  
  certifikates_1_4_vec <- rep(c(certifikates_1_4), times = years)
  
  income_certifikates_1_4_vec <- certifikates_1_4_vec * co2_price_per_ton
  
  certifikates_1_4_final <- Reduce("+", certifikates_1_4_vec)
  
  # Version 2 and 5
  
  co2_per_year_2_5 <- 200 * kg_per_bush
  
  certifikates_2_5 <- co2_per_year_2_5 / kg_per_certificate
  
  certifikates_2_5 <- certifikates_2_5 - co2_emmiter_eggs
  
  certifikates_2_5_vec <- rep(c(certifikates_2_5), times = years)
  
  income_certifikates_2_5_vec <- certifikates_1_4_vec * co2_price_per_ton
  
  certifikates_2_5_final <- Reduce("+", certifikates_2_5_vec)
  
  # Version 3
  
  co2_per_year_3 <- 200 * kg_per_tree
  
  certifikates_3 <- co2_per_year_3 / kg_per_certificate
  
  certifikates_3 <- certifikates_3 - co2_emmiter_eggs
  
  certifikates_3_vec <- rep(c(certifikates_3), times = years)
  
  income_certifikates_3_vec <- certifikates_1_4_vec * co2_price_per_ton
  
  certifikates_3_final <- Reduce("+", certifikates_3_vec)
  
  # Decision / Final ----
  # discounting for inflation
  
  # Version 1
  small_nut_chicken_profit_vec_1 <-
    nut_profit_vec_1 + chicken_income - general_investments_vec + subsidies_vec
  
  small_nut_chicken_profit_vec_1 <- 
    discount(small_nut_chicken_profit_vec_1, discount_rate)
  
  small_nut_chicken_profit_1 <- Reduce("+", small_nut_chicken_profit_vec_1)
  
  
  #Version 2
  big_nut_chicken_profit_vec_2 <-
    nut_profit_vec_2 + chicken_income - general_investments_vec + subsidies_vec
  
  big_nut_chicken_profit_vec_2 <- 
    discount(big_nut_chicken_profit_vec_2, discount_rate)
  
  big_nut_chicken_profit_2 <- Reduce("+", big_nut_chicken_profit_vec_2)
  
  # Version 3
  truffle_chicken_profit_vec_3 <-
    truffle_final_vec_3 - truffle_tree_costs + chicken_income - 
    general_investments_vec + subsidies_vec
  
  truffle_chicken_profit_vec_3 <-
    discount(truffle_chicken_profit_vec_3, discount_rate)
  
  truffle_chicken_profit_3 <- Reduce("+", truffle_chicken_profit_vec_3)
  
  # Version 4
  small_nut_chicken_truffle_profit_vec_4 <-
    nut_profit_vec_1 + chicken_income + (truffle_final_vec_4_5/2.8) - 
    general_investments_vec + subsidies_vec
  
  small_nut_chicken_truffle_profit_vec_4 <- 
    discount(small_nut_chicken_truffle_profit_vec_4, discount_rate)
  
  small_nut_chicken_truffle_profit_4 <- 
    Reduce("+", small_nut_chicken_truffle_profit_vec_4)
  
  # Version 5
  big_nut_chicken_truffle_profit_vec_5 <-
    nut_profit_vec_2 + chicken_income + truffle_final_vec_4_5 - 
    general_investments_vec + subsidies_vec
  
  big_nut_chicken_truffle_profit_vec_5 <- 
    discount(big_nut_chicken_truffle_profit_vec_5, discount_rate)
  
  big_nut_chicken_truffle_profit_5 <- 
    Reduce("+", big_nut_chicken_truffle_profit_vec_5)
  
  # Version 6 Chicken only for better visualization
  
  chicken_profit_vec <- 
    chicken_income - general_investments_vec + subsidies_vec
  
  chicken_profit_vec <- discount(chicken_profit_vec, discount_rate)
  
  chicken_profit_6 <- Reduce("+", chicken_profit_vec)
  
  
  # Decisions
  
  ###
  
  decision_1 <- small_nut_chicken_profit_1 - baseline
  
  decision_2 <- big_nut_chicken_profit_2 - baseline
  
  decision_3 <- truffle_chicken_profit_3 - baseline
  
  decision_4 <- small_nut_chicken_truffle_profit_4 - baseline
  
  decision_5 <- big_nut_chicken_truffle_profit_5 - baseline
  
  d_2_inst_1 <- big_nut_chicken_profit_2 - small_nut_chicken_profit_1
  
  d_5_inst_4 <- 
    big_nut_chicken_truffle_profit_5 - small_nut_chicken_truffle_profit_4
  
  d_4_inst_1 <- small_nut_chicken_truffle_profit_4 - small_nut_chicken_profit_1
  
  d_5_inst_2 <- big_nut_chicken_truffle_profit_5 -big_nut_chicken_profit_2
  
  d_3_inst_1 <- truffle_chicken_profit_3 - small_nut_chicken_profit_1
  
  d_3_inst_4 <- truffle_chicken_profit_3 - small_nut_chicken_truffle_profit_4
  
  d_3_inst_2 <- truffle_chicken_profit_3 - big_nut_chicken_profit_2
  
  d_3_inst_5 <- truffle_chicken_profit_3 - big_nut_chicken_truffle_profit_5
  
  d_1_inst_6 <- small_nut_chicken_profit_1 - chicken_profit_6
    
  d_2_inst_6 <- big_nut_chicken_profit_2 - chicken_profit_6
    
  d_3_inst_6 <- truffle_chicken_profit_3 - chicken_profit_6
    
  d_4_inst_6 <- small_nut_chicken_truffle_profit_4 - chicken_profit_6
    
  d_5_inst_6 <- big_nut_chicken_truffle_profit_5 - chicken_profit_6
  
  d_5_inst_3 <- big_nut_chicken_truffle_profit_5 - truffle_chicken_profit_3
  
  d_4_inst_3 <- small_nut_chicken_truffle_profit_4 - truffle_chicken_profit_3
  ###
  
  
  return(list(outcome_1 = small_nut_chicken_profit_1,
              outcome_2 = big_nut_chicken_profit_2,
              outcome_3 = truffle_chicken_profit_3,
              outcome_4 = small_nut_chicken_truffle_profit_4,
              outcome_5 = big_nut_chicken_truffle_profit_5,
              outcome_6 = chicken_profit_6,
              baseline = baseline,
              decision_1 = decision_1,
              decision_2 = decision_2,
              decision_3 = decision_3,
              decision_4 = decision_4,
              decision_5 = decision_5,
              d_5_inst_3 = d_5_inst_3,
              certifikates_1_4 = certifikates_1_4_final,
              certifikates_2_5 = certifikates_2_5_final,
              certifikates_3 = certifikates_3_final,
              vec_outcome_1 = small_nut_chicken_profit_vec_1,
              vec_outcome_2 = big_nut_chicken_profit_vec_2,
              vec_outcome_3 = truffle_chicken_profit_vec_3,
              vec_outcome_4 = small_nut_chicken_truffle_profit_vec_4,
              vec_outcome_5 = big_nut_chicken_truffle_profit_vec_5,
              vec_outcome_baseline = baseline_vec,
              co2_egg = co2_emmiter_eggs))
  
}


# Monte Carlo ----
# Run the Monte Carlo simulation using the model function
simulation <- mcSimulation(
  estimate = as.estimate(input_estimates),
  model_function = model_function,
  numberOfModelRuns = 10000,
  functionSyntax = "plainNames"
)
