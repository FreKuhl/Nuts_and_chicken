library(decisionSupport)
library (DiagrammeR)
library("readxl")

input_estimates <- read_excel("input_estimates_lukas.xlsx")
years <- 40

# Model Function ----

model_function <- function(){
  
  # Initial investment costs ----
  investment_cost <- initial_fences_cost + planting_tree_cost
  
  
  # Maintenance ----
  maintaining_trees <- vv(var_mean = maintaining_trees_cost,
                          var_CV = 15,
                          n = years)
  maintaining_trees_final <- Reduce("+", maintaining_trees)
  
  
  maintaining_fences <- vv(var_mean = maintaining_fences_cost,
                           var_CV = 15,
                           n = years)
  maintaining_fences_final <- Reduce("+", maintaining_fences)
  
  trees_to_replace_var <- vv(var_mean = trees_to_replace,
                             var_CV = 100,
                             n = years)
  replacing_trees <- vv(var_mean = replacing_trees_cost,
                        var_CV = 20,
                        n = years)
  replacing_trees_sum <- Map("*", trees_to_replace_var, replacing_trees)
  replacing_trees_final <- Reduce("+", replacing_trees_sum)

  
  
  # Income ----

  
  nuts <- gompertz_yield(max_harvest = 1000,
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
    nut_income =+ value * 20 
    if (value != 0){
      harvest_count =+ 1
    }
  }
  
  nut_harvest_cost_final <- harvest_count * nut_harvest_cost
  
  
  # Final----
  
  final_income <- nut_income + subsidies
  
  final_maintenance <- maintaining_trees_final + maintaining_fences_final + 
    replacing_trees_final + nut_harvest_cost_final
  
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

