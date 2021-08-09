# Libraries and inputs ----

library(decisionSupport)
library(tidyverse)
library(ggpubr)
library("readxl")

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

# Decisions 2
plot_distributions(
  mcSimulation_object = simulation,
  vars = c("d_1_inst_6","d_2_inst_6", "d_3_inst_6", "d_4_inst_6", "d_5_inst_6"),
  method = "smooth_simple_overlay"
) +
  labs(title = "Scenarios compared to `only chickens`",
       subtitle = "Accumulated values for 30 years - 10000 model runs") +
  scale_fill_manual(
    labels = c("Intervention 1...", "Intervention 2...",
               "Intervention 3...", "Intervention 4...", "Intervention 5 instead of `only chickens`"),
    values = c("red", "blue", "green", "orange", "purple", "grey"),
    name = "Decision Options:"
  ) +
  theme(legend.position = "bottom")

# Decisions 3
plot_distributions(
  mcSimulation_object = simulation,
  vars = c("d_4_inst_3", "d_5_inst_3", "d_5_inst_4"),
  method = "smooth_simple_overlay") +
  labs(title = "Decisions",
       subtitle = "Accumulated values for 30 years - 10000 model runs") +
  scale_fill_manual(
    labels = c("Scenario 5 - 3", "Scenario 5 - 3", "Scenario 5 - 4"),
    values = c("red", "blue", "green", "orange", "purple", "grey"),
    name = "Decision Options:"
  ) +
  theme(legend.position = "bottom")

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
  cashflow_var_name = c("vec_outcome_baseline",
                        "vec_outcome_1",
                        "vec_outcome_2",
                        "vec_outcome_3",
                        "vec_outcome_4",
                        "vec_outcome_5"),
  x_axis_name = "Years with intervention",
  y_axis_name = "Annual cashflow in €",
  color_25_75 = "green4",
  color_5_95 = "green1",
  color_median = "red",
  facet_labels = c(
    "vec_outcome_baseline",
    "vec_outcome_1",
    "vec_outcome_2",
    "vec_outcome_3",
    "vec_outcome_4",
    "vec_outcome_5"
  ),
) +
  labs(title = "Cashflow",
       subtitle = "Values for the first 30 years - 10000 model runs")



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

pls_result4<- plsr.mcSimulation(
  object = simulation,
  resultName = names(simulation$y)[3],
  ncomp = 1
)

Pls_4<- plot_pls(pls_result3, input_table = input_estimates, threshold = 0.8) + 
  ggtitle(label= "nuts+hay+chicken+truffle (70 trees)")

pls_result5<- plsr.mcSimulation(
  object = simulation,
  resultName = names(simulation$y)[3],
  ncomp = 1
)

Pls_5<- plot_pls(pls_result3, input_table = input_estimates, threshold = 0.8) + 
  ggtitle(label= "nuts+chicken+truffle (200 trees)")



# final plot
Pls_combined <- ggarrange(Pls_2, Pls_1, Pls_3, Pls_4, Pls_5 + rremove("x.text"), 
                          labels = c(" ", " ", " "," ", " "),
                          ncol = 3, nrow = 1) +
  ggtitle(label= "outcome_2")

# outcome_4
pls_result4 <- plsr.mcSimulation(
  object = simulation,
  resultName = names(simulation$y)[4],
  ncomp = 1
)

Pls_4 <- plot_pls(pls_result4, input_table = input_estimates, threshold = 0.8) + 
  ggtitle(label= "4: Few Nuttrees + Truffle + Chicken")

# outcome_5
pls_result5 <- plsr.mcSimulation(
  object = simulation,
  resultName = names(simulation$y)[5],
  ncomp = 1
)

Pls_5 <- plot_pls(pls_result5, input_table = input_estimates, threshold = 0.8) + 
  ggtitle(label= "5: Many Nuttrees + Truffle + Chicken")

# final plot
Pls_combined <- ggarrange(Pls_1, Pls_2, Pls_3, Pls_4, Pls_5 + rremove("x.text"), 
                          labels = c(" ", " ", " ", " ", " "),
                          ncol = 5, nrow = 1)
annotate_figure(Pls_combined,
                top = text_grob("Projection to Latent Structures analysis",
                                face = "bold", size = 14),
)
# EVPI ----
# Use with caution!!! takes really long time to calculate!!!

mcSimulation_table_1 <- data.frame(simulation$x, simulation$y[19])
mcSimulation_table_2 <- data.frame(simulation$x, simulation$y[20])
mcSimulation_table_3 <- data.frame(simulation$x, simulation$y[7])
mcSimulation_table_4 <- data.frame(simulation$x, simulation$y[9])
mcSimulation_table_5 <- data.frame(simulation$x, simulation$y[10])
mcSimulation_table_6 <- data.frame(simulation$x, simulation$y[11])
mcSimulation_table_7 <- data.frame(simulation$x, simulation$y[12])
mcSimulation_table_8 <- data.frame(simulation$x, simulation$y[13])

evpi_1 <- multi_EVPI(mc = mcSimulation_table_1, write_table = T, first_out_var = "d_5_inst_3")
evpi_2 <- multi_EVPI(mc = mcSimulation_table_2, write_table = T, first_out_var = "d_4_inst_3")
evpi_3 <- multi_EVPI(mc = mcSimulation_table_3, write_table = T, first_out_var = "d_5_inst_4")
evpi_4 <- multi_EVPI(mc = mcSimulation_table_4, write_table = T, first_out_var = "d_5_inst_2")
evpi_5 <- multi_EVPI(mc = mcSimulation_table_5, write_table = T, first_out_var = "d_3_inst_1")
evpi_6 <- multi_EVPI(mc = mcSimulation_table_6, write_table = T, first_out_var = "d_3_inst_4")
evpi_7 <- multi_EVPI(mc = mcSimulation_table_7, write_table = T, first_out_var = "d_3_inst_2")
evpi_8 <- multi_EVPI(mc = mcSimulation_table_8, write_table = T, first_out_var = "d_3_inst_5")

plot_evpi(evpi_1, decision_vars = "d_5_inst_3")
plot_evpi(evpi_2, decision_vars = "d_4_inst_3")
plot_evpi(evpi_3, decision_vars = "d_5_inst_4")
plot_evpi(evpi_4, decision_vars = "d_5_inst_2")
plot_evpi(evpi_5, decision_vars = "d_1_inst_3")
plot_evpi(evpi_6, decision_vars = "d_4_inst_3")
plot_evpi(evpi_7, decision_vars = "d_2_inst_3")
plot_evpi(evpi_8, decision_vars = "d_5_inst_3")
