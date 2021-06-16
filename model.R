library(decisionSupport)
library (DiagrammeR)
library("readxl")

input_estimates <- read_excel("input_estimates_lukas.xlsx")


# Model Function ####

model_function <- function(){
  
  # Still missing, likelyhood with which something occurs, for example dead trees
  
  # Estimate the initial investment costs
  investment_cost <- grass_planting_cost + protective_animal_cost + initial_fences_cost + 
    initial_truffle_cost + planting_tree_cost + initial_chicken_cost + chicken_mobile_cost
  
  # Anual Maintaining costs
  maintaining <- maintaining_trees_cost + maintaining_fences_cost + maintaining_chicken_mobile_cost +
    replacing_chicken_cost + replacing_trees_cost + feed_cost + crop_cost + working_cost

  # Harvest costs
  harvest_costs <- nut_harvest_cost + tree_harvest_cost
  
  # Income
  income <- subsidies + (truffle_yield * truffle_price) + (nut_yield * nut_price) + 
    (eggs_yield * eggs_price) + (wood_yield * wood_price) + (crop_yield * crop_price)
  
  # Estimate the final results from the model
  final_result <- (10 * income) - investment_cost - (10 * (maintaining + harvest_costs))
  
  # Generate the list of outputs from the Monte Carlo simulation
  return(list(final_result = final_result))
}



# Run the Monte Carlo simulation using the model function
simulation <- mcSimulation(estimate = as.estimate(input_estimates),
                                    model_function = model_function,
                                    numberOfModelRuns = 10000,
                                    functionSyntax = "plainNames")

#simulation


# plot_distributions ####
plot_distributions(mcSimulation_object = simulation,
                   vars = "final_result",
                   method = "smooth_simple_overlay",
                   old_names = "final_result",
                   new_names = "Outcome distribution for profits")

