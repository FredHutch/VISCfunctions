test_that("bounds labels integrate with a built ggplot2 continuous scale", {
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("scales")

  plot_data <- data.frame(visit = 1:3, response = c(0, 0.5, 1))
  plot <- ggplot2::ggplot(
    plot_data,
    ggplot2::aes(x = visit, y = response)
  ) +
    ggplot2::geom_point() +
    ggplot2::scale_y_continuous(
      breaks = c(0, 0.5, 1),
      labels = label_with_bounds(
        formatter = scales::label_percent(accuracy = 1),
        lower = TRUE,
        upper = TRUE
      )
    )

  built <- ggplot2::ggplot_build(plot)
  y_scale <- built$layout$panel_scales_y[[1]]

  expect_equal(y_scale$get_breaks(), c(0, 0.5, 1))
  expect_identical(
    y_scale$get_labels(),
    c("\u2264 0%", "50%", "\u2265 100%")
  )
  expect_equal(built$data[[1]]$y, plot_data$response)
})
