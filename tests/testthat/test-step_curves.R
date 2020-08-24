context("create_step_info and create_mb_results")



test_that("create_step_info and create_mb_results testing", {
  get_output <- function(xx) {
    data.frame(time = c(0, as.double(xx$time)),
               surv = c(1, xx$surv),
               n.risk = c(NA, xx$n.risk),
               n.event = c(NA, xx$n.event),
               n.censor = c(NA, xx$n.censor))
  }

  # Simple testing
  x = 1:10
  event = rep(0:1, 5)
  expect_equal(
    object = create_step_info(x),
    expected = get_output(
      survival::survfit(survival::Surv(x) ~ 1)
    )
  )
  expect_equal(
    object = create_step_info(x, event),
    expected = get_output(
      survival::survfit(survival::Surv(x, event) ~ 1)
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
        cbind(ptid = xx,
              get_output(
                survival::survfit(survival::Surv(dat$x[dat$ptid == xx],
                                                 dat$event[dat$ptid == xx]) ~ 1)
              )
        )
      })
    ))

  expect_equal(
    object = dat %>%
      dplyr::group_by(ptid) %>%
      dplyr::group_modify(~ create_step_info(x = .x$x, event = .x$event)) %>%
      dplyr::ungroup(),
    expected = expected_output
  )

})
