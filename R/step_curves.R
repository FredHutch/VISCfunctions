#' Creating Points for Step Lines
#'
#' Creates step curve info for survival KM plots and magnitude breath (MB) plots
#'
#' @param x values to create step curve for (numeric vector)
#' @param event event status, 0=censor and 1=event (numeric vector).
#'   If NULL assumes no censoring
#' @return Returns a data frame with time, \code{surv}, \code{n.risk},
#'   \code{n.event}, and \code{n.censor} (\code{survival::summary.survfit}
#'   output format)
#'
#' @details
#'   The output can be used for plotting to create area under the curve AUC
#'   values, with \code{time} on the x axis, \code{surv} on the y axis, and
#'   \code{n.censor == 1} subset can be used for a \code{ggplot2::geom_point()}
#'   layer
#'
#' @examples
#'
#' create_step_info(x = 1:10)
#' create_step_info(x = 1:10, event = rep(0:1, 5))
#'
#' library(dplyr)
#' dat = data.frame(x = c(1:10),
#'                  event = c(1,1,0,1,1,0,0,1,1,1),
#'                  ptid = c(1,1,2,2,3,3,3,3,3,3))
#' plot_data <-
#'  dat %>%
#'   dplyr::group_by(ptid) %>%
#'   dplyr::group_modify(~ create_step_info(x = .x$x, event = .x$event))
#'
#' ggplot2::ggplot(data = plot_data,
#'                 ggplot2::aes(x = time, y = surv, color = factor(ptid))) +
#'  ggplot2::geom_step(linetype = "dashed", direction = 'hv', lwd = .35) +
#'  ggplot2::geom_point(data = plot_data %>% filter(n.censor == 1),
#'                      shape = 3, size = 6, show.legend = FALSE)
#'
#' @export

create_step_info <- function(x, event = NULL){
  .check_numeric_input(x, lower_bound = 0)
  if (is.null(event)) {
    surv_obj_here <- survival::Surv(x)
  } else {
    .check_binary_input(event)
    if (!all(event %in% c(NA,0,1))) stop('"event" must be a numeric or factor vector containing only 0/1 values')
    if (is.factor(event)) event <- as.numeric(as.vector(event))
    surv_obj_here <- survival::Surv(x, event)
  }
  surv_fit_obj_here <- survival::survfit(surv_obj_here ~ 1)

  # Need to add 1,0 point for plots and AUC
  data.frame(time = c(0, as.double(surv_fit_obj_here$time)),
             surv = c(1, surv_fit_obj_here$surv),
             n.risk = c(NA, surv_fit_obj_here$n.risk),
             n.event = c(NA, surv_fit_obj_here$n.event),
             n.censor = c(NA, surv_fit_obj_here$n.censor))
}



