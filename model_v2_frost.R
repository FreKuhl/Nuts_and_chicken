library(decisionSupport)
library (DiagrammeR)
library("readxl")

input_estimates <- read_excel("input_estimates_lukas.xlsx")
years <- 40

# Model Function

model_function <- function(){
  
  # Initial investment costs ----
  investment_cost <- grass_planting_cost + protective_animal_cost + initial_fences_cost + 
    initial_truffle_cost + planting_tree_cost + initial_chicken_cost + chicken_mobile_cost
  
  
  # Maintenance ----
  maintaining_trees <- vv(var_mean = maintaining_trees_cost,
                          var_CV = 15,
                          n = years)
  maintaining_trees_final <- Reduce("+", maintaining_trees)
  
  
  maintaining_fences <- vv(var_mean = maintaining_fences_cost,
                           var_CV = 15,
                           n = years)
  maintaining_fences_final <- Reduce("+", maintaining_fences)
  
  
  maintaining_chicken_mobile <- vv(var_mean = maintaining_chicken_mobile_cost,
                                   var_CV = 15,
                                   n = years)
  maintaining_chicken_mobile_final <- Reduce("+", maintaining_chicken_mobile)
  
  
  replacing_chicken_final <- 39 * (133 * replacing_chicken_cost)
  
  
  trees_to_replace_var <- vv(var_mean = trees_to_replace,
                             var_CV = 100,
                             n = years)
  replacing_trees <- vv(var_mean = replacing_trees_cost,
                        var_CV = 20,
                        n = years)
  replacing_trees_sum <- Map("*", trees_to_replace_var, replacing_trees)
  replacing_trees_final <- Reduce("+", replacing_trees_sum)
  
  
  feed <- vv(var_mean = feed_cost,
             var_CV = 20,
             n = years)
  feed_final <- Reduce("+", feed)
  
  
  # Income ----
  
  truffle <- gompertz_yield(max_harvest = 10,
                            time_to_first_yield_estimate = 10,
                            first_yield_estimate_percent = 50,
                            time_to_second_yield_estimate = 17,
                            second_yield_estimate_percent = 100,
                            n_years = years,
                            var_CV = 20)
  for (value in truffle){
    truffle_income =+ value * truffle_price
  }
  
  
  eggs <- vv(var_mean = eggs_yield,
             var_CV = 5, 
             n = years)
  for (value in eggs){
    egg_income =+ value * eggs_price
  }
  
  #wood_income <- 2 * wood_yield
  
  
  nuts <- gompertz_yield(max_harvest = nut_yield,
                         time_to_first_yield_estimate = 4,
                         first_yield_estimate_percent = 50,
                         time_to_second_yield_estimate = 10,
                         second_yield_estimate_percent = 100,
                         n_years = years,
                         var_CV = 20)
  
  
  # Frost chance ----
  
  nuts_frost <- chance_event(chance = 0.1,
                             value_if = 0,
                             value_if_not = 1,
                             n = years)
  
  nuts_frost_yield <- Map("*", nuts, nuts_frost)
  
  for (value in nuts_frost_yield){
    nut_income =+ value * nut_price 
    if (value != 0){
      harvest_count =+ 1
    }
  }
  
  nut_harvest_cost_final <- harvest_count * nut_harvest_cost
  
  
  # Final----
  
  final_income <- nut_income + truffle_income + egg_income + subsidies
  
  final_maintenance <- maintaining_trees_final + maintaining_fences_final + maintaining_chicken_mobile_final +
    replacing_chicken_final + replacing_trees_final + feed_final + nut_harvest_cost_final
  
  final_result <- final_income - final_maintenance - investment_cost
  
  return(list(final_result = final_result))
}

# Run the Monte Carlo simulation using the model function ----
simulation <- mcSimulation(estimate = as.estimate(input_estimates),
                           model_function = model_function,
                           numberOfModelRuns = 3000,
                           functionSyntax = "plainNames")


# plot_distributions ----
plot_distributions(mcSimulation_object = simulation,
                   vars = "final_result",
                   method = "smooth_simple_overlay",
                   old_names = "final_result",
                   new_names = "Profits after 20 Years")

