# Libraries and inputs ----
library(decisionSupport)
library(tidyverse)
library("readxl")



input_estimates <- read_excel("input_nuts-small.xlsx")
years <- 40
number_of_chicken <- 225
replacement <- c(0, 225, 0, 225, 0, 225, 0, 225, 0, 225, 0, 225, 0, 225, 0, 225, 0, 225, 0, 225, 0, 225, 0, 225, 0, 225, 0, 225, 0, 225, 0, 225, 0, 225, 0, 225, 0, 225, 0, 225)

# Make variables----
make_variables <- function(est,n=1)
{ x<-random(rho=est, n=n)
for(i in colnames(x)) assign(i,
                             as.numeric(x[1,i]),envir=.GlobalEnv)
}

make_variables(as.estimate(input_estimates))


# Chicken ----
# *Costs----
# * * Initial costs ----
# initial Investment for setting up the chicken mobile
initial_chicken_costs_final <-
  number_of_chicken * chicken_replacement_cost + initial_chicken_mobile_cost

# Maintenance costs for the chicken mobile
maintaining_chicken_mobile <- vv(
  var_mean = maintaining_chicken_mobile,
  var_CV = 20,
  n = years
)

# adding the initial cost to the first year of maintenance
maintaining_chicken_mobile[1] <- maintaining_chicken_mobile[1] + initial_chicken_costs_final

# * * Operating costs  ----
# Amount of feed needed for the number_of_chicken each year
chicken_feed <- vv(var_mean = chicken_feed,
                   var_CV = 5,
                   n = years)

# Yearly costs for feed
feed_cost <- vv(var_mean = feed_costs,
                        var_CV = 5,
                        n = years)

#
feed_cost_final <- chicken_feed* feed_cost

#
working_hours_chicken <- vv(
  var_mean = working_hours_chicken,
  var_CV = 10,
  n = years
)

#
working_costs_chicken_final <- working_hours_chicken * working_hours_costs

#
chicken_replacement <- vv(var_mean = replacement,
                          var_CV = 10,
                          n = years
)

#
chicken_replacement_cost_final <- chicken_replacement * chicken_replacement_cost

# * Income ----
# Number of eggs per year

eggs <- vv(
  var_mean = eggs,
  var_CV = 5,
  n = years
)
eggs_per_year <- eggs * number_of_chicken

eggs_price <- vv(
  var_mean = eggs_price,
  var_CV = 10,
  n = years,
  lower_limit = 0.25
)

eggs_income <- eggs_per_year * eggs_price

# Adding Values ----
# 
chicken_income <- sum(eggs_income - maintaining_chicken_mobile - feed_cost_final - working_costs_chicken_final - chicken_replacement_cost_final)
