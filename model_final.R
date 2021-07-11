# Libraries and inputs ----

library(decisionSupport)
library(tidyverse)
library(ggpubr)
library("readxl")

input_estimates <- read_excel("input_estimates.xlsx")
discount_rate = 3
years <- 30 # IMPORTANT! Select ONLY steps of 10

# make variables ----
make_variables <- function(est,n=1)
{ x<-random(rho=est, n=n)
for(i in colnames(x)) assign(i,
                             as.numeric(x[1,i]),envir=.GlobalEnv)
}

make_variables(as.estimate(input_estimates))



# Model Function ----

# 5 Szenarios: 
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
  
  general_investments_vec <- vv(var_mean = maintaining_fences_cost,
                                var_CV = 30,
                                n = years)
  ###
  general_investments_vec[1] <- general_investments_vec[1] + 
    grass_planting_cost + initial_fences_cost
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
  nut_income_vec_1_4 <- (nuts_yield_vec_1 * nut_price) + income_hay + subsidies
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
  nut_income_vec_2_5 <- (nuts_yield_vec_2 * nut_price) + subsidies
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
  
  nut_other_costs_vec_4[1] <- nut_other_costs_vec_4[1] + truffle_tree_planting_costs_1
  
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
  
  nut_other_costs_vec_5[1] <- nut_other_costs_vec_5[1] + truffle_tree_planting_costs_2
  
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
  
  irrigation_costs_1_4 <- irrigation_costs_1 + maintaining_irrigation_1 + water_costs_1
  
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
  
  irrigation_costs_2_5 <- irrigation_costs_2 + maintaining_irrigation_2 + water_costs_2
  
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
    first_yield_estimate_percent = 50,
    time_to_second_yield_estimate = 10,
    second_yield_estimate_percent = 100,
    n_years = years,
    var_CV = 40,
  )
  
  truffle_price <- vv(var_mean = truffle_price,
                      var_CV = 10,
                      n = years)
  
  truffle_income <- truffle * truffle_price
  
  truffle_harvest_costs <- vv(var_mean = truffle_harvest_costs,
                              var_CV = 5,
                              n = years)
  
  truffle_harvest_costs[1:9] <- 0
  
  truffle_final_vec_3_4_5 <- truffle_income - truffle_harvest_costs
  
  # Chicken ----
  # For Version 1, 2, 3, 4 and 5
  
  initial_chicken_costs_final <-
    (number_of_chicken * chicken_replacement_cost) + initial_chicken_mobile_cost
  
  maintaining_chicken_mobile_vec <- vv(var_mean = maintaining_chicken_mobile,
                                       var_CV = 15,
                                       n = years)
  
  maintaining_chicken_mobile_vec[1] <-
    maintaining_chicken_mobile_vec[1] + initial_chicken_costs_final
  
  chicken_feed <- vv(var_mean = chicken_feed,
                     var_CV = 5,
                     n = years)
  
  feed_cost <- vv(var_mean = feed_cost,
                  var_CV = 10,
                  n = years)
  
  feed_cost_final <- chicken_feed * feed_cost
  
  working_hours_chicken <- vv(var_mean = working_hours_chicken,
                              var_CV = 10,
                              n = years)
  
  working_costs_chicken_final <-
    working_hours_chicken * working_hours_costs
  
  chicken_replacement <- vv(var_mean = number_of_chicken,
                            var_CV = 10,
                            n = years,
                            lower_limit = 225)
  
  # setting every second year to zero starting with the first 
  chicken_replacement[] <- chicken_replacement * c(FALSE, TRUE)
  
  chicken_replacement_cost_final <-
    chicken_replacement * chicken_replacement_cost
  
  # Income
  
  # Number of eggs per Year
  eggs_per_year <- vv(var_mean = eggs,
                      var_CV = 7,
                      n = years)
  
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
      maintaining_chicken_mobile_vec + feed_cost_final +
        working_costs_chicken_final + chicken_replacement_cost_final
    )
  
  # Decision / Final ----
  # discounting for inflation
  
  # Version 1
  small_nut_chicken_profit_vec_1 <-
    nut_profit_vec_1 + chicken_income
  
  small_nut_chicken_profit_vec_1 <- 
    discount(small_nut_chicken_profit_vec_1, discount_rate)
  
  small_nut_chicken_profit_1 <- Reduce("+", small_nut_chicken_profit_vec_1)
  
  
  #Version 2
  big_nut_chicken_profit_vec_2 <-
    nut_profit_vec_2 + chicken_income
  
  big_nut_chicken_profit_vec_2 <- 
    discount(big_nut_chicken_profit_vec_2, discount_rate)
  
  big_nut_chicken_profit_2 <- Reduce("+", big_nut_chicken_profit_vec_2)
  
  # Version 3
  truffle_chicken_profit_vec_3 <-
    truffle_final_vec_3_4_5 - truffle_tree_costs + chicken_income
  
  truffle_chicken_profit_vec_3 <-
    discount(truffle_chicken_profit_vec_3, discount_rate)
  
  truffle_chicken_profit_3 <- Reduce("+", truffle_chicken_profit_vec_3)
  
  # Version 4
  small_nut_chicken_truffle_profit_vec_4 <-
    nut_profit_vec_1 + chicken_income + (truffle_final_vec_3_4_5/2.8)
  
  small_nut_chicken_truffle_profit_vec_4 <- 
    discount(small_nut_chicken_truffle_profit_vec_4, discount_rate)
  
  small_nut_chicken_truffle_profit_4 <- Reduce("+", small_nut_chicken_truffle_profit_vec_4)
  
  # Version 5
  big_nut_chicken_truffle_profit_vec_5 <-
    nut_profit_vec_2 + chicken_income + truffle_final_vec_3_4_5
  
  big_nut_chicken_truffle_profit_vec_5 <- 
    discount(big_nut_chicken_truffle_profit_vec_5, discount_rate)
  
  big_nut_chicken_truffle_profit_5 <- Reduce("+", big_nut_chicken_truffle_profit_vec_5)
  
  
  # Decisions
  
  ###
  
  d_2_inst_1 <- big_nut_chicken_profit_2 - small_nut_chicken_profit_1
  
  d_5_inst_4 <- big_nut_chicken_truffle_profit_5 - small_nut_chicken_truffle_profit_4
  
  d_4_inst_1 <- small_nut_chicken_truffle_profit_4 - small_nut_chicken_profit_1
  
  d_5_inst_2 <- big_nut_chicken_truffle_profit_5 -big_nut_chicken_profit_2
  
  d_1_inst_3 <- small_nut_chicken_profit_1 - truffle_chicken_profit_3
  
  d_4_inst_3 <-  small_nut_chicken_truffle_profit_4 - truffle_chicken_profit_3
  
  d_2_inst_3 <- big_nut_chicken_profit_2 - truffle_chicken_profit_3
  
  d_5_inst_3 <- big_nut_chicken_truffle_profit_5 - truffle_chicken_profit_3
  
  ###
  
  
  return(list(outcome_1 = small_nut_chicken_profit_1,
              outcome_2 = big_nut_chicken_profit_2,
              outcome_3 = truffle_chicken_profit_3,
              outcome_4 = small_nut_chicken_truffle_profit_4,
              outcome_5 = big_nut_chicken_truffle_profit_5,
              d_2_inst_1 = d_2_inst_1,
              d_5_inst_4 = d_5_inst_4,
              d_4_inst_1 = d_4_inst_1,
              d_5_inst_2 = d_5_inst_2,
              d_1_inst_3 = d_1_inst_3,
              d_4_inst_3 = d_4_inst_3,
              d_2_inst_3 = d_2_inst_3,
              d_5_inst_3 = d_5_inst_3,
              vec_outcome_1 = small_nut_chicken_profit_vec_1,
              vec_outcome_2 = big_nut_chicken_profit_vec_2,
              vec_outcome_3 = truffle_chicken_profit_vec_3,
              vec_outcome_4 = small_nut_chicken_truffle_profit_vec_4,
              vec_outcome_5 = big_nut_chicken_truffle_profit_vec_5))
  
}


# Monte Carlo ----
# Run the Monte Carlo simulation using the model function
simulation <- mcSimulation(
  estimate = as.estimate(input_estimates),
  model_function = model_function,
  numberOfModelRuns = 10000,
  functionSyntax = "plainNames"
)


# Plot distributions ----

plot_distributions(
  mcSimulation_object = simulation,
  vars = c("outcome_1", "outcome_2", "outcome_3", "outcome_4", "outcome_5"),
  method = "smooth_simple_overlay"
) +
  labs(title = "Distribution of income for five different interventions",
       subtitle = "Accumulated values for 30 years - 10000 model runs") +
  scale_fill_manual(
    labels = c("outcome_1", "outcome_2", "outcome_3", "outcome_4", "outcome_5"),
    values = c("red", "blue", "green", "orange", "purple"),
    name = "Decision Options:"
  ) +
  theme(legend.position = "bottom")

# v2
plot_distributions(
  mcSimulation_object = simulation,
  vars = c("d_2_inst_1", "d_2_inst_3", "d_4_inst_1", "d_5_inst_3", "d_5_inst_4"),
  method = "smooth_simple_overlay"
) +
  labs(title = "Differences between the sceanrios",
       subtitle = "Accumulated values for 30 years - 10000 model runs") +
 scale_fill_manual(
  labels = c("d_2_inst_1", "d_2_inst_3", "d_4_inst_1", "d_5_inst_3", "d_5_inst_4"),
  values = c("red", "blue", "green", "orange", "purple", "grey"),
  name = "Decision Options:"
  ) +
  theme(legend.position = "bottom")


# Plot the cashflow distribution over time
# This seems weird...
plot_cashflow(
  mcSimulation_object = simulation,
  cashflow_var_name = c("vec_outcome_1", "vec_outcome_2", "vec_outcome_3", "vec_outcome_4", "vec_outcome_5"),
  x_axis_name = "Years with intervention",
  y_axis_name = "Annual cashflow in €",
  color_25_75 = "green4",
  color_5_95 = "green1",
  color_median = "red",
  facet_labels = c("vec_outcome_1", "vec_outcome_2", "vec_outcome_3", "vec_outcome_4", "vec_outcome_5")
) +
  labs(title = "Cashflow in three different interventions",
       subtitle = "Values for the first 10 years - 10000 model runs")

# PLS ----
#Projection to Latent Structures analysis

# outcome_1
pls_result <- plsr.mcSimulation(
  object = simulation,
  resultName = names(simulation$y)[1],
  ncomp = 1
)

Pls_1 <- plot_pls(pls_result, input_table = input_estimates, threshold = 0.8) + 
  ggtitle(label= "outcome_1")

# outcome_2
pls_result2 <- plsr.mcSimulation(
  object = simulation,
  resultName = names(simulation$y)[2],
  ncomp = 1
)

Pls_2 <- plot_pls(pls_result2, input_table = input_estimates, threshold = 0.8) + 
  ggtitle(label= "outcome_2")

# outcome_3
pls_result3 <- plsr.mcSimulation(
  object = simulation,
  resultName = names(simulation$y)[3],
  ncomp = 1
)

Pls_3 <- plot_pls(pls_result3, input_table = input_estimates, threshold = 0.8) + 
  ggtitle(label= "outcome_2")

# outcome_4
pls_result4 <- plsr.mcSimulation(
  object = simulation,
  resultName = names(simulation$y)[4],
  ncomp = 1
)

Pls_4 <- plot_pls(pls_result4, input_table = input_estimates, threshold = 0.8) + 
  ggtitle(label= "outcome_4")

# outcome_5
pls_result5 <- plsr.mcSimulation(
  object = simulation,
  resultName = names(simulation$y)[5],
  ncomp = 1
)

Pls_5 <- plot_pls(pls_result5, input_table = input_estimates, threshold = 0.8) + 
  ggtitle(label= "outcome_5")

# final plot
Pls_combined <- ggarrange(Pls_2, Pls_1, Pls_3, Pls_4, Pls_5 + rremove("x.text"), 
          labels = c(" ", " ", " ", " ", " "),
          ncol = 5, nrow = 1)
annotate_figure(Pls_combined,
                top = text_grob("Projection to Latent Structures analysis", face = "bold", size = 14),
)
# EVPI ----
# Use with caution!!! takes really long time to calculate!!!

mcSimulation_table_1 <- data.frame(simulation$x, simulation$y[6])
mcSimulation_table_2 <- data.frame(simulation$x, simulation$y[7])
mcSimulation_table_3 <- data.frame(simulation$x, simulation$y[8])
mcSimulation_table_4 <- data.frame(simulation$x, simulation$y[9])
mcSimulation_table_5 <- data.frame(simulation$x, simulation$y[10])
mcSimulation_table_6 <- data.frame(simulation$x, simulation$y[11])
mcSimulation_table_7 <- data.frame(simulation$x, simulation$y[12])
mcSimulation_table_8 <- data.frame(simulation$x, simulation$y[13])

evpi_1 <- multi_EVPI(mc = mcSimulation_table, write_table = T, first_out_var = "d_2_inst_1")
evpi_2 <- multi_EVPI(mc = mcSimulation_table, write_table = T, first_out_var = "d_5_inst_4")
evpi_3 <- multi_EVPI(mc = mcSimulation_table, write_table = T, first_out_var = "d_4_inst_1")
evpi_4 <- multi_EVPI(mc = mcSimulation_table, write_table = T, first_out_var = "d_5_inst_2")
evpi_5 <- multi_EVPI(mc = mcSimulation_table, write_table = T, first_out_var = "d_1_inst_3")
evpi_6 <- multi_EVPI(mc = mcSimulation_table, write_table = T, first_out_var = "d_4_inst_3")
evpi_7 <- multi_EVPI(mc = mcSimulation_table, write_table = T, first_out_var = "d_2_inst_3")
evpi_8 <- multi_EVPI(mc = mcSimulation_table, write_table = T, first_out_var = "d_5_inst_3")

plot_evpi(evpi_1, decision_vars = "d_2_inst_1")
plot_evpi(evpi_2, decision_vars = "d_5_inst_4")
plot_evpi(evpi_3, decision_vars = "d_4_inst_1")
plot_evpi(evpi_4, decision_vars = "d_5_inst_2")
plot_evpi(evpi_5, decision_vars = "d_1_inst_3")
plot_evpi(evpi_6, decision_vars = "d_4_inst_3")
plot_evpi(evpi_7, decision_vars = "d_2_inst_3")
plot_evpi(evpi_8, decision_vars = "d_5_inst_3")
