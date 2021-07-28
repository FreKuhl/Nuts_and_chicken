function (mcSimulation_object, cashflow_var_name, x_axis_name = "Timeline of intervention", 
          y_axis_name = "Cashflow", legend_name = "Quantiles (%)", 
          legend_labels = c("5 to 95", "25 to 75", "median"), 
          color_25_75 = "grey40", color_5_95 = "grey70", 
          color_median = "blue", facet_labels = cashflow_var_name, 
          base_size = 11, ...) 
{
  assertthat::assert_that(class(mcSimulation_object)[[1]] == 
                            "mcSimulation", msg = "mcSimulation_object is not class 'mcSimulation', please provide a valid object. This does not appear to have been generated with 'mcSimulation' function.")
  data <- data.frame(mcSimulation_object$y, mcSimulation_object$x)
  assertthat::assert_that(is.character(cashflow_var_name), 
                          msg = "cashflow_var_name is not a character string.")
  assertthat::assert_that(unique(mcSimulation_object$x$n_years) > 
                            1, msg = "n_years are not more than '1'. Consider adding more years to the model.")
  assertthat::assert_that(is.character(x_axis_name), msg = "x_axis_name is not a character string.")
  assertthat::assert_that(is.character(y_axis_name), msg = "y_axis_name is not a character string.")
  assertthat::assert_that(is.character(color_25_75), msg = "color_25_75 is not a character string.")
  assertthat::assert_that(is.character(color_5_95), msg = "color_5_95 is not a character string.")
  assertthat::assert_that(is.character(color_median), msg = "color_median is not a character string.")
  assertthat::assert_that(color_25_75 %in% grDevices::colors(), 
                          msg = "Please choose a color name for color_25_75 from the grDevices colors.")
  assertthat::assert_that(color_5_95 %in% grDevices::colors(), 
                          msg = "Please choose a color name for color_5_95 from the grDevices colors.")
  assertthat::assert_that(color_median %in% grDevices::colors(), 
                          msg = "Please choose a color name for color_median from the grDevices colors.")
  assertthat::assert_that(any(substr(names(data), 1, nchar(cashflow_var_name)) %in% 
                                cashflow_var_name), msg = "There are no variables that start with the same name as cashflow_var_name in your data.")
  if (any(is.na(data))) {
    warning(paste("Some data included \"NA\" and", 
                  table(is.na(data))[2], "rows were removed."))
    data <- data[stats::complete.cases(data), ]
  }
  subset_data <- data %>% dplyr::select(dplyr::starts_with(cashflow_var_name)) %>% 
    tidyr::pivot_longer(dplyr::starts_with(cashflow_var_name))
  subset_list <- list()
  for (i in 1:length(cashflow_var_name)) {
    subset_list[[i]] <- subset_data %>% tidyr::separate(name, 
                                                        c("decision_option", "x_scale"), sep = nchar(cashflow_var_name[i])) %>% 
      dplyr::filter(decision_option == cashflow_var_name[i])
  }
  subset_data <- dplyr::bind_rows(subset_list)
  summary_subset_data <- suppressMessages(subset_data %>% dplyr::group_by(decision_option, 
                                                                          x_scale) %>% dplyr::summarize(p5 = quantile(value, 0.05), 
                                                                                                        p25 = quantile(value, 0.25), p50 = quantile(value, 0.5), 
                                                                                                        p75 = quantile(value, 0.75), p95 = quantile(value, 0.95)))
  ggplot2::ggplot(summary_subset_data, ggplot2::aes(as.numeric(x_scale))) + 
    ggplot2::geom_ribbon(ggplot2::aes(ymin = p5, ymax = p95, 
                                      fill = legend_labels[1])) + ggplot2::geom_ribbon(ggplot2::aes(ymin = p25, 
                                                                                                    ymax = p75, fill = legend_labels[2])) + ggplot2::geom_line(ggplot2::aes(y = p50, 
                                                                                                                                                                            color = legend_labels[3])) + ggplot2::geom_line(ggplot2::aes(y = 0)) + 
    ggplot2::scale_x_continuous(expand = c(0, 0)) + ggplot2::scale_y_continuous(labels = scales::comma) + 
    ggplot2::scale_fill_manual(values = c(color_25_75, color_5_95)) + 
    ggplot2::scale_color_manual(values = color_median) + 
    ggplot2::guides(fill = ggplot2::guide_legend(reverse = T, 
                                                 order = 1)) + ggplot2::labs(x = x_axis_name, y = y_axis_name, 
                                                                             fill = legend_name, color = "") + ggplot2::facet_wrap(~factor(decision_option, 
                                                                                                                                           labels = facet_labels)) + ggplot2::theme_bw(base_size = base_size) + 
    ggplot2::theme(legend.margin = ggplot2::margin(-0.75, 
                                                   0, 0, 0, unit = "cm"), strip.background = ggplot2::element_blank(), 
                   ...)
}