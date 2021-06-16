library(decisionSupport)
library (DiagrammeR)
library(tidyverse)
librarreadxl

# Impact Pathway ####
mermaid("graph LR
        E(Establishment Cost)-->T(Total Cost); style T fill: red; 
        linkStyle 0 stroke:red, stroke-width:2px
        M(Management Cost)-->T; style M fill:blue;
        linkStyle 1 stroke: red, stroke-width:2px
        
        T-->NPV(Net Present Value); style T fill:red;
        linkStyle 2 stroke: red, stroke-width:2px
        R(Revenue)-->NPV; style R fill:green;
        linkStyle 3 stroke: green, stroke-width:2px
        
        RT(Regional Truffle)--> R(Revenue);
        linkStyle 4 stroke: green, stroke-width:2px
        RN(Regional Nuts)--> R;
        linkStyle 5 stroke: green, stroke-width:2px
        FRE(Free Range Eggs)--> R;
        linkStyle 6 stroke: green, stroke-width:2px
        HQW(High Quality Wood)--> R;
        linkStyle 7 stroke: green, stroke-width:2px
        CR(Crop Rotation)-->R; style CR fill:blue
        linkStyle 8 stroke:green, stroke-width:2px
        ")

input_estimates <- read.csv("input_estimates.csv", sep=";", dec=".")



# Just for testing ####

# make_variables <- function(est,n=1)
# { x<-random(rho=est, n=n)
# for(i in colnames(x)) assign(i,
#                              as.numeric(x[1,i]),envir=.GlobalEnv)
# }
# # 
# # make_variables(as.estimate(input_estimates))


# Model Function ####

model_function <- function(){
  # Estimate the baseline income in a normal season
    baseline_profit <- read_excel("input_estimates")
  
  # Estimate the total revenue
    truffle_profit <-
    nuts_profit <-
    eggs_profit <-
    wood_profit <-
      total_profit <-
    
  # Estimate the total costs
    establishment_cost <- 
    management_costs <- 
    work_cost <- 
      total_cost <-
 
    
  # Estimate the final results from the model
  final_result <- total_profit - total_cost - baseline_profit
  
  # Generate the list of outputs from the Monte Carlo simulation
  return(list(final_result = final_result))
}

# Run the Monte Carlo simulation using the model function
chile_mc_simulation <- mcSimulation(estimate = as.estimate(input_estimates),
                                    model_function = model_function,
                                    numberOfModelRuns = 800,
                                    functionSyntax = "plainNames")

chile_mc_simulation


# plot_distributions ####
plot_distributions(mcSimulation_object = chile_mc_simulation,
                   vars = "final_result",
                   method = "smooth_simple_overlay",
                   old_names = "final_result",
                   new_names = "Outcome distribution for profits")

