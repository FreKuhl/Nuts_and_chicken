# Libraries and inputs ----

library(decisionSupport)
library(tidyverse)
library(ggpubr)
library("readxl")
library(bayesplot)

# Plot distributions ----

# Plot income of 5 scenarios + baseline
plot_distributions(
  mcSimulation_object = simulation,
  vars = c("baseline","outcome_1", "outcome_2", "outcome_3", "outcome_4", "outcome_5"),
  method = "smooth_simple_overlay"
) +
  labs(title = "Distribution of income for five different interventions and Baseline",
       subtitle = "Accumulated values for 30 years - 10000 model runs") +
  scale_fill_manual(
    labels = c("Baseline: Crop Land", "nuts+hay+chicken (70)", "nuts+chicken (200)",
               "Truffle Trees + chicken (200)", "nuts+hay+chicken+truffle (70)",
               "nuts+chicken+truffle (200)"),
    values = c("red", "blue", "green", "orange", "purple", "grey"),
    name = "Decision Options:
    (# of Trees)"
  ) +
  coord_cartesian(ylim = c(0, 0.000015)) +
  xlab("Outcome distribution in €") +
  ylab("Probability density")

# Plot Decision 1-5 against baseline
plot_distributions(
  mcSimulation_object = simulation,
  vars = c("decision_1", "decision_2", "decision_3", "decision_4", "decision_5"),
  method = "smooth_simple_overlay"
) +
  labs(title = "Worth of decision of switching from baseline to scenarios",
       subtitle = "Accumulated values for 30 years - 10000 model runs") +
  scale_fill_manual(
    labels = c("decision_1", "decision_2", "decision_3", "decision_4", "decision_5"),
    values = c("red", "blue", "green", "orange", "purple", "grey"),
    name = "Decision Options:"
  ) +
  theme(legend.position = "bottom") +
  coord_cartesian(ylim = c(0, 0.000015)) +
  xlab("Outcome distribution in €") +
  ylab("Probability density")


# Decisions 3
plot_distributions(
  mcSimulation_object = simulation,
  vars = c("d_5_inst_3"),
  method = "smooth_simple_overlay") +
  labs(title = "Decisions Scenario 5 instead of 3",
       subtitle = "Accumulated values for 30 years - 10000 model runs") +
  scale_fill_manual(
    labels = c("Scenario 5 instead of 3"),
    values = c("red", "blue", "green", "orange", "purple", "grey"),
    name = "Decision Options:"
  ) +
  theme(legend.position = "bottom") +
  xlab("Outcome distribution in €") +
  ylab("Probability density")

# Plot CO² Certificates ----
plot_distributions(
  mcSimulation_object = simulation,
  vars = c("certifikates_1_4", "certifikates_2_5", "certifikates_3"),
  method = "smooth_simple_overlay") +
  labs(title = "CO² Certificates",
       subtitle = "Accumulated values for 30 years - 10000 model runs") +
  scale_fill_manual(
    labels = c("Scenario 1 and 4", "Scenario 2 and 5", "Scenario 3"),
    values = c("red", "blue", "green", "orange", "purple", "grey"),
    name = "Decision Options:"
  ) +
  theme(legend.position = "bottom")

# Plot the cashflow distribution over time ----
plot_cashflow(
  mcSimulation_object = simulation,
  cashflow_var_name = c(
    "vec_outcome_1",
    "vec_outcome_2",
    "vec_outcome_3",
    "vec_outcome_4",
    "vec_outcome_5",
    "vec_outcome_baseline"
  ),
  x_axis_name = "Years with intervention",
  y_axis_name = "Annual cashflow in €",
  color_25_75 = "green4",
  color_5_95 = "green1",
  color_median = "red",
  facet_labels = c(
    "Scenario 1",
    "Scenario 2",
    "Scenario 3",
    "Scenario 4",
    "Scenario 5",
    "Scenario baseline"
  ),
) +
  labs(title = "Cashflow",
       subtitle = "Values for the first 30 years - 10000 model runs")

# Cahsflow just baseline
plot_cashflow(
  mcSimulation_object = simulation,
  cashflow_var_name = c("vec_outcome_baseline"),
  x_axis_name = "Years with intervention",
  y_axis_name = "Annual cashflow in €",
  color_25_75 = "green4",
  color_5_95 = "green1",
  color_median = "red",
  facet_labels = c("Scenario baseline"
  ),
) +
  labs(title = "Cashflow",
       subtitle = "Values for the first 30 years - 10000 model runs")

# Other plotting options ----

results <- data.frame("Scenario 1" = simulation$y$outcome_1,
                   "Scenario 2" = simulation$y$outcome_2,
                   "Scenario 3" = simulation$y$outcome_3,
                   "Scenario 4" = simulation$y$outcome_4,
                   "Scenario 5" = simulation$y$outcome_5)

decisions <- data.frame("Scenario 1" = simulation$y$decision_1,
                      "Scenario 2" = simulation$y$decision_2,
                      "Scenario 3" = simulation$y$decision_3,
                      "Scenario 4" = simulation$y$decision_4,
                      "Scenario 5" = simulation$y$decision_5)

results_1 <- data.frame("Scenario 1" = simulation$y$outcome_1,
                        "Scenario 2" = simulation$y$outcome_2,
                        "Scenario 3" = simulation$y$outcome_3)

results_2 <- data.frame("Scenario 4" = simulation$y$outcome_4,
                        "Scenario 5" = simulation$y$outcome_5)

baseline_1 <- data.frame("Baseline" = simulation$y$baseline)

# results
mcmc_areas(results,prob = 0.5, point_est = "median") +
  xlab("Outcome Distribution in €") +
  labs(title = "Distribution of income for five different interventions",
       subtitle = "Accumulated values for 30 years - 10000 model runs")

#Decisions
mcmc_areas(decisions,prob = 0.5, point_est = "median") +
  xlab("Outcome Distribution in €") +
  labs(title = "Worth of decision of switching from baseline to scenarios",
       subtitle = "Accumulated values for 30 years - 10000 model runs")

mcmc_areas(baseline_1,prob = 0.5, point_est = "median") +
  xlab("Outcome Distribution in €")



mcmc_intervals(results,prob = 0.5,prob_outer = 0.95,point_est = "median")



# 1: nuts+hay+chicken (70 trees)
# 2: nuts+chicken (200 trees)
# 3: Truffle Trees + chicken (200 trees)
# 4: nuts+hay+chicken+truffle (70 trees)
# 5: nuts+chicken+truffle (200 trees)

# PLS ----
#Projection to Latent Structures analysis

# outcome_1
pls_result <- plsr.mcSimulation(
  object = simulation,
  resultName = names(simulation$y)[1],
  ncomp = 1
)

Pls_1 <- plot_pls(pls_result, input_table = input_estimates, threshold = 0.8) + 
  ggtitle(label= "nuts+hay+chicken (70 trees)")

# outcome_2
pls_result2 <- plsr.mcSimulation(
  object = simulation,
  resultName = names(simulation$y)[2],
  ncomp = 1
)

Pls_2 <- plot_pls(pls_result2, input_table = input_estimates, threshold = 0.8) + 
  ggtitle(label= "nuts+chicken (200 trees)")


# outcome_3
pls_result3 <- plsr.mcSimulation(
  object = simulation,
  resultName = names(simulation$y)[3],
  ncomp = 1
)

Pls_3 <- plot_pls(pls_result3, input_table = input_estimates, threshold = 0.8) + 
  ggtitle(label= "Truffle Trees + chicken (200 trees)")

# outcome 4
pls_result4<- plsr.mcSimulation(
  object = simulation,
  resultName = names(simulation$y)[4],
  ncomp = 1
)

Pls_4<- plot_pls(pls_result4, input_table = input_estimates, threshold = 0.8) + 
  ggtitle(label= "nuts+hay+chicken+truffle (70 trees)")

# outcome 5
pls_result5<- plsr.mcSimulation(
  object = simulation,
  resultName = names(simulation$y)[5],
  ncomp = 1
)

Pls_5<- plot_pls(pls_result5, input_table = input_estimates, threshold = 0.8) + 
  ggtitle(label= "nuts+chicken+truffle (200 trees)")

# baseline
pls_result6<- plsr.mcSimulation(
  object = simulation,
  resultName = names(simulation$y)[7],
  ncomp = 1
)

Pls_6<- plot_pls(pls_result6, input_table = input_estimates, threshold = 0.8) + 
  ggtitle(label= "nuts+chicken+truffle (200 trees)")



# final plot
Pls_combined <- ggarrange(Pls_1, Pls_2, Pls_3, Pls_4, Pls_5, Pls_6 + rremove("x.text"), 
                          labels = c(" ", " ", " ", " ", " ", " "),
                          ncol = 5, nrow = 1)
annotate_figure(Pls_combined,
                top = text_grob("Projection to Latent Structures analysis",
                                face = "bold", size = 14),
)

# Plot results together ----

compound_figure(model = model_function,
                input_table = input_estimates,
                decision_var_name = "baseline",
                cashflow_var_name = "vec_outcome_baseline",
                model_runs = 10000,
                distribution_method = 'smooth_simple_overlay')


# EVPI ----
# Use with caution!!! takes really long time to calculate!!!

mcSimulation_table_1 <- data.frame(simulation$x, simulation$y[8])
mcSimulation_table_2 <- data.frame(simulation$x, simulation$y[9])
mcSimulation_table_3 <- data.frame(simulation$x, simulation$y[10])
mcSimulation_table_4 <- data.frame(simulation$x, simulation$y[11])
mcSimulation_table_5 <- data.frame(simulation$x, simulation$y[12])
mcSimulation_table_6 <- data.frame(simulation$x, simulation$y[13])

evpi_1 <- multi_EVPI(mc = mcSimulation_table_1, write_table = F, first_out_var = "decision_1")
evpi_2 <- multi_EVPI(mc = mcSimulation_table_2, write_table = F, first_out_var = "decision_2")
evpi_3 <- multi_EVPI(mc = mcSimulation_table_3, write_table = F, first_out_var = "decision_3")
evpi_4 <- multi_EVPI(mc = mcSimulation_table_4, write_table = F, first_out_var = "decision_4")
evpi_5 <- multi_EVPI(mc = mcSimulation_table_5, write_table = F, first_out_var = "decision_5")
evpi_6 <- multi_EVPI(mc = mcSimulation_table_6, write_table = F, first_out_var = "d_5_inst_3")

plot_evpi(evpi_1, decision_vars = "decision_1")
plot_evpi(evpi_2, decision_vars = "decision_2")
plot_evpi(evpi_3, decision_vars = "decision_3")
plot_evpi(evpi_4, decision_vars = "decision_4")
plot_evpi(evpi_5, decision_vars = "decision_5")
plot_evpi(evpi_6, decision_vars = "d_5_inst_3")
