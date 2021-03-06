context("create_step_curve and mb_results")



test_that("create_step_curve testing", {
  get_output <- function(xx, flip = FALSE) {
    if (flip) {
      data.frame(time = c(0, as.double(xx$time), Inf),
                 surv = c(1, xx$surv, 0),
                 n.risk = c(max(xx$n.risk), xx$n.risk, NA),
                 n.event = c(NA, xx$n.event, NA),
                 n.censor = c(NA, xx$n.censor, NA),
                 surv.flipped = 1 -  c(1, xx$surv, 0))
    } else {
      data.frame(time = c(0, as.double(xx$time)),
                 surv = c(1, xx$surv),
                 n.risk = c(max(xx$n.risk), xx$n.risk),
                 n.event = c(NA, xx$n.event),
                 n.censor = c(NA, xx$n.censor))
    }
  }

  # Simple testing
  x = 1:10
  event = rep(0:1, 5)
  expect_identical(
    object = create_step_curve(x),
    expected = get_output(
      survival::survfit(survival::Surv(x) ~ 1)
    )
  )
  expect_identical(
    object = create_step_curve(x, factor(event)),
    expected = get_output(
      survival::survfit(survival::Surv(x, event) ~ 1)
    )
  )

  # testing flip_surv
  expect_identical(
    object = create_step_curve(x, flip_surv = TRUE),
    expected = get_output(
      survival::survfit(survival::Surv(x) ~ 1), flip = TRUE
    )
  )
  expect_identical(
    object = create_step_curve(x, event, flip_surv = TRUE),
    expected = get_output(
      survival::survfit(survival::Surv(x, event) ~ 1), flip = TRUE
    )
  )


  #testing with dplyr
  library(dplyr)
  dat = data.frame(x = c(1:10),
                   event = c(1,1,0,1,1,0,0,1,1,1),
                   ptid = c(1,1,2,2,3,3,3,3,3,3))
  expected_output <-
    tibble(bind_rows(
      lapply(1:3, function(xx) {
        cbind(ptid = as.numeric(xx),
              get_output(
                survival::survfit(survival::Surv(dat$x[dat$ptid == xx],
                                                 dat$event[dat$ptid == xx]) ~ 1)
              )
        )
      })
    ))

  expect_identical(
    object = dat %>%
      dplyr::group_by(ptid) %>%
      dplyr::group_modify(~ create_step_curve(x = .x$x, event = .x$event)) %>%
      dplyr::ungroup(),
    expected = expected_output
  )


  # Error checking
  expect_error(
    object = create_step_curve(1:3, c(1,'a','a')),
    regexp = '"event" must be a numeric or factor vector containing only 0/1 values'
  )

})




test_that("mb_results testing", {


  data(exampleData_BAMA)

  # testing AUC
  data_here <- exampleData_BAMA[exampleData_BAMA$visitno == 2, ]

  # Testing raw no truncation
  expect_equal(
    object = mb_results(data_here$magnitude, lower_trunc = 0,
                        upper_trunc = Inf, x_transform = 'raw')$aucMB[1],
    expected = mean(data_here$magnitude),
    tolerance = 1e-8
  )
  # Testing log10 no truncation
  expect_equal(
    object = mb_results(data_here$magnitude, lower_trunc = 1,
                        upper_trunc = Inf, x_transform = 'log10')$aucMB[1],
    expected = 10^mean(log10(data_here$magnitude)),
    tolerance = 1e-8
  )

  # Testing raw truncation
  expect_equal(
    object = mb_results(data_here$magnitude, lower_trunc = 100,
                        upper_trunc = 22000, x_transform = 'raw')$aucMB[1],
    expected = mean(pmin(pmax(100, data_here$magnitude), 22000)),
    tolerance = 1e-8
  )
  # Testing log10 truncation
  expect_equal(
    object = mb_results(data_here$magnitude, lower_trunc = 100,
                        upper_trunc = 22000, x_transform = 'log10')$aucMB[1],
    expected = 10^mean(log10(pmin(pmax(100, data_here$magnitude), 22000))),
    tolerance = 1e-8
  )


  # Testing raw no truncation with response
  expect_equal(
    object = mb_results(data_here$magnitude, data_here$response, lower_trunc = 0,
                        upper_trunc = Inf, x_transform = 'raw')$aucMB[1],
    expected = mean(data_here$magnitude * data_here$response),
    tolerance = 1e-8
  )
  # Testing log10 no truncation with response
  expect_equal(
    object = mb_results(data_here$magnitude, data_here$response, lower_trunc = 1,
                        upper_trunc = Inf, x_transform = 'log10')$aucMB[1],
    expected = 10^mean(log10(pmax(data_here$magnitude * data_here$response,1))),
    tolerance = 1e-8
  )

  # Testing raw truncation with response
  expect_equal(
    object = mb_results(data_here$magnitude, data_here$response, lower_trunc = 100,
                        upper_trunc = 22000, x_transform = 'raw')$aucMB[1],
    expected = mean(
      pmin(pmax(100, data_here$magnitude * data_here$response), 22000)),
    tolerance = 1e-8
  )
  # Testing log10 truncation with response
  expect_equal(
    object = mb_results(data_here$magnitude, data_here$response, lower_trunc = 100,
                        upper_trunc = 22000, x_transform = 'log10')$aucMB[1],
    expected = 10^mean(
      log10(pmin(pmax(100, data_here$magnitude * data_here$response), 22000))),
    tolerance = 1e-8
  )

  # Full output test
  auc_here = 10^mean(
    log10(pmin(pmax(100, data_here$magnitude * data_here$response), 22000))
    )
  step_info_here <- create_step_curve(pmin(pmax(
    100, data_here$magnitude * data_here$response), 22000))[,-5]
  step_info_here$time[step_info_here$time == 0] <- 1
  names(step_info_here) <- c('magnitude', 'breadth', 'n_remaining', 'n_here')

  expect_equal(
    object = mb_results(data_here$magnitude, data_here$response, lower_trunc = 100,
                        upper_trunc = 22000, x_transform = 'log10'),
    expected = data.frame(step_info_here, aucMB = auc_here),
    tolerance = 1e-8
  )

  #Error checking
  expect_error(
    object = mb_results(data_here$magnitude, lower_trunc = Inf),
    regexp = '"lower_trunc" can not be set at Inf'
  )
  expect_error(
    object = mb_results(data_here$magnitude[-1], data_here$response),
    regexp = '"magnitude" and "response" must be same length'
  )


})
