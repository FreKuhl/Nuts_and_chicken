library(decisionSupport)
library(tidyverse)
library("readxl")

input_estimates <- read_excel("input_estimates_v2.xlsx")
years <- 20

# Model Function ----

model_function <- function(){
  
  # General investments ----
  
  initial_investments <- grass_planting_cost + initial_fences_cost + machinery
  
  maintaining_fences <- vv(var_mean = maintaining_fences_cost,
                           var_CV = 30,
                           n = years)
  
  general_investments <- initial_investments + Reduce("+", maintaining_fences) # Final value of this chapter
  
  # Hazelnuts ----
  
  # Frost chance_event
  
  nuts_frost <- chance_event(chance = 0.1,
                             value_if = 0,
                             value_if_not = 1,
                             n = years)
  
  # Yield
  
  nuts <- gompertz_yield(max_harvest = nut_yield,
                         time_to_first_yield_estimate = 4,
                         first_yield_estimate_percent = 50,
                         time_to_second_yield_estimate = 10,
                         second_yield_estimate_percent = 100,
                         n_years = years,
                         var_CV = 20)
  
  nuts_frost_yield <- Map("*", nuts, nuts_frost)
  
  nut_price <- vv(var_mean = nut_price,
                  var_CV = 20,
                  n = years)
  
  nut_income <- Map("*", nuts_frost_yield, nut_price)
  
  nut_income <- Reduce("+", nut_income) # Final value income nuts
  
  
  # Costs
  
  initial_haselnut_costs <- tree_planting_cost + ((years / 10) * harvest_nets)
  
  maintaining_trees <- sum(vv(var_mean = maintaining_trees_cost,
                              var_CV = 20,
                              n = years))
  
  harvest_count <- sum(nuts_frost_yield > 20)
  
  nut_harvest_cost <- sum(vv(var_mean = nut_harvest_cost,
                             var_CV = 20,
                             n = harvest_count))
  
  # Replacing Trees
  
  replacing_trees <- chance_event(chance = 0.3,
                                  value_if = 10,
                                  value_if_not = 0,
                                  n = years,
                                  CV_if = 50)
  
  trees_to_replace <- Reduce("+", replacing_trees)
  
  replacing_trees_cost <- trees_to_replace * replacing_trees_cost
  
  nut_fertilizer_cost <- sum(vv(var_mean = nut_fertilizer,
                                var_CV = 30,
                                n = years))
  
  nut_costs <- initial_haselnut_costs + 
    maintaining_trees + 
    nut_harvest_cost +
    replacing_trees_cost +
    nut_fertilizer_cost # Finale Hazelnut costs
  
  hazelnuts <- nut_income - nut_costs
  
  # Irrigation ----
  
  
  days_irrigation <- vv(var_mean = days_to_irrigate,
                        var_CV = 80,
                        n = years)

  
  for (i in 1:length(days_irrigation)){
    if ((days_irrigation[i]) < 10){
      days_irrigation[i] <- 10
    }
    if (days_irrigation[i] > 60){
      days_irrigation[i] <- 60
    }
  }
  
  days_to_irrigate <- sum(days_irrigation)
  
  irrigation_work_costs <- (work_per_irrigation * working_hours_costs) * days_to_irrigate
  
  water <- days_to_irrigate * water_per_day
  
  water_costs <- (water / 1000) * water_price
  
  irrigation_maintanence <- sum(vv(var_mean = maintaining_irrigation,
                                   var_CV = 10,
                                   n = years))
  
  irrigation_costs <- water_costs + 
    irrigation_work_costs + 
    installation_irrigation +
    irrigation_maintanence
  # Final irrigation Value
  
  # Truffle ----
  
  # Yield
  
  truffle <- gompertz_yield(max_harvest = truffle_yield,
                            time_to_first_yield_estimate = 9,
                            first_yield_estimate_percent = 20,
                            time_to_second_yield_estimate = 17,
                            second_yield_estimate_percent = 100,
                            n_years = years,
                            var_CV = 20)
  
  truffle_price <- vv(var_mean = truffle_price,
                      var_CV = 10,
                      n = years)
  
  truffle_income <- Map("*", truffle, truffle_price)
  
  truffle_income <- Reduce("+", truffle_income)
  
  truffle_harvest_cost <- (years - 8) * truffle_harvest_cost
  
  truffle_final <- truffle_income - truffle_harvest_cost - truffle_planting_cost
  
  # Chicken ----
  
  initial_chicken_costs <- initial_chicken_cost + initial_chicken_mobile_cost
  
  maintaining_chicken_mobile <- sum(vv(var_mean = maintaining_chicken_mobile,
                                       var_CV = 40,
                                       n = years))
  
  chicken_feed <- vv(var_mean = chicken_feed,
                     var_CV = 15,
                     n = years)
  
  chicken_feed_cost <- vv(var_mean = feed_costs,
                          var_CV = 20,
                          n = years)
  
  feed_cost_total <- Map("*", chicken_feed, chicken_feed_cost)
  
  feed_cost_total <- Reduce("+", feed_cost_total)
  
  working_hours_chicken <- sum(vv(var_mean = working_hours_chicken,
                          var_CV = 5,
                          n = years))
  
  working_costs_chicken <- working_hours_chicken * working_hours_costs
  
  chicken_replacement <- (years * 133) * chicken_replacement_cost
  
  eggs <- sum(vv(var_mean = eggs,
                 var_CV = 5,
                 n = years))
  
  eggs_income <- eggs * eggs_price
  
  chicken_income <- eggs_income - 
    chicken_replacement - 
    working_costs_chicken - 
    feed_cost_total - 
    initial_chicken_costs
  
  # Final Results
  
  nuts_final <- hazelnuts - general_investments - irrigation_costs
  
  nuts_chicken_final <- hazelnuts + chicken_income - general_investments - irrigation_costs
  
  nuts_truffle_chicken_final <- hazelnuts - general_investments - irrigation_costs + truffle_final + chicken_income
  
  final_chicken_income <- chicken_income - general_investments
  
  crop <- years * deckungsbeitrag
  
  return(list(nuts = nuts_final,
              nuts_chicken = nuts_chicken_final,
              nuts_chicken_truffle = nuts_truffle_chicken_final,
              chicken = final_chicken_income,
              crop = crop))
}



# Run the Monte Carlo simulation using the model function ----
simulation <- mcSimulation(estimate = as.estimate(input_estimates),
                           model_function = model_function,
                           numberOfModelRuns = 10000,
                           functionSyntax = "plainNames")


# plot_distributions ----
plot_distributions(mcSimulation_object = simulation,
                   vars = c("nuts_chicken", "nuts_chicken_truffle", "nuts"),
                   method = "smooth_simple_overlay",)




# Plot the cashflow distribution over time

plot_cashflow(mcSimulation_object = simulation, cashflow_var_name = "nuts_chicken",
              x_axis_name = "Years with intervention",
              y_axis_name = "Annual cashflow in USD",
              color_25_75 = "green4", color_5_95 = "green1",
              color_median = "red")


# (PLS) Projection to Latent Structures analysis ----

# nuts_final 
pls_result <- plsr.mcSimulation(object = simulation,
                                resultName = names(simulation$y)[1], ncomp = 1)


plot_pls(pls_result, input_table = input_estimates, threshold = 0) + ggtitle("Nuts only")

#nuts_chicken_final
pls_result <- plsr.mcSimulation(object = simulation,
                                resultName = names(simulation$y)[2], ncomp = 1)


plot_pls(pls_result, input_table = input_estimates, threshold = 0) + ggtitle("Nuts and chicken")

#nuts_truffle_chicken_final
pls_result <- plsr.mcSimulation(object = simulation,
                                resultName = names(simulation$y)[3], ncomp = 1)


plot_pls(pls_result, input_table = input_estimates, threshold = 0) + ggtitle("Nuts, chicken & truffle")

#final_chicken_income
pls_result <- plsr.mcSimulation(object = simulation,
                                resultName = names(simulation$y)[4], ncomp = 1)


plot_pls(pls_result, input_table = input_estimates, threshold = 0) + ggtitle("Chicken only")

#crop
pls_result <- plsr.mcSimulation(object = simulation,
                                resultName = names(simulation$y)[5], ncomp = 1)


plot_pls(pls_result, input_table = input_estimates, threshold = 0) + ggtitle("Baseline")



# EVPI

mcSimulation_table <- data.frame(simulation$x, simulation$y[1:5])

evpi <- multi_EVPI(mc = mcSimulation_table, first_out_var = "nuts")

plot_evpi(evpi, decision_vars = "NPV_decision_do")

compound_figure(mcSimulation_object = mcSimulation_results, 
                input_table = input_table, plsrResults = pls_result, 
                EVPIresults = evpi, decision_var_name = "NPV_decision_do", 
                cashflow_var_name = "Cashflow_decision_do", 
                base_size = 7)







