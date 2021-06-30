library(decisionSupport)
library (DiagrammeR)
library("readxl")

input_estimates <- read_excel("input_estimates_v2.xlsx")
years <- 40

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
  
  days_irrigation <- chance_event(chance = 0.99,
                                  value_if = 30,
                                  value_if_not = 0,
                                  n = years,
                                  CV_if = 80)
  
  for (i in 1:length(days_irrigation)){
    if ((days_irrigation[i]) < 10){
      days_irrigation[i] <- 10
    }
    if (days_irrigation[i] > 55){
      days_irrigation[i] <- 55
    }
  }
  
  days_to_irrigate <- sum(days_irrigation)
  
  irrigation_work_costs <- work_per_irrigation * days_to_irrigate
  
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
                            time_to_first_yield_estimate = 10,
                            first_yield_estimate_percent = 50,
                            time_to_second_yield_estimate = 17,
                            second_yield_estimate_percent = 100,
                            n_years = years,
                            var_CV = 20)
  
  truffle_price <- vv(var_mean = truffle_price,
                      var_CV = 10,
                      n = years)
  
  truffle_income <- Map("*", truffle, truffle_price)
  
  truffle_income <- Reduce("+", truffle_income)
  
  truffle_harvest_cost <- (years - 10) * truffle_harvest_cost
  
  truffle_final <- truffle_income - truffle_harvest_cost - truffle_planting_cost
  
  
  
  
  
  
  
  
  
  
  
  
  
  final_result <- hazelnuts - general_investments - irrigation_costs
  
  final_result_w_truffle <- hazelnuts - general_investments - irrigation_costs + truffle_final
  
  return(list(final_result = final_result, 
              irrigation = irrigation_costs, 
              w_truffle = final_result_w_truffle))
}











# Run the Monte Carlo simulation using the model function ----
simulation <- mcSimulation(estimate = as.estimate(input_estimates),
                           model_function = model_function,
                           numberOfModelRuns = 3000,
                           functionSyntax = "plainNames")


# plot_distributions ----
plot_distributions(mcSimulation_object = simulation,
                   vars = c("final_result", "w_truffle"),
                   method = "smooth_simple_overlay",)















