# Explicit breaks identify the intended lower and upper plotting limits.
# This example labels proportions as percentages without changing the data.
example_data <- data.frame(visit = 1:3, response = c(0, 0.5, 1))

ggplot2::ggplot(
  example_data,
  ggplot2::aes(x = visit, y = response)
) +
  ggplot2::geom_point() +
  ggplot2::scale_y_continuous(
    breaks = c(0, 0.5, 1),
    labels = VISCfunctions::label_with_bounds(
      formatter = scales::label_percent(accuracy = 1),
      lower = TRUE,
      upper = TRUE
    )
  ) +
  ggplot2::labs(x = "Visit", y = "Response")

# Expected y-axis labels: <= 0%, 50%, >= 100% (with inequality symbols).
# Labels alone do not set scale limits or squish out-of-range observations.
