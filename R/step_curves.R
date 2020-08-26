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
#'   The output can be used for plotting step function curves,
#'   with \code{time} on the x axis, \code{surv} on the y axis, and
#'   \code{n.censor == 1} subset can be used for a \code{ggplot2::geom_point()}
#'   layer.
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




#' Magnitude Breath (MB) Curves and Area Under the Curve (AUC)
#'
#' Creates step curve info for magnitude breath (MB) plots, with option to
#'  include response status and have logged transformation for AUC calculation.
#'
#' @param x values to create step curve and AUC for (numeric vector)
#' @param response response status, vector of type integer (0/1) or logical
#'   (TRUE/FALSE). If NULL assumes no response information considered.
#' @param lower_trunc the lower truncation value (numeric scalar that must
#'   be 0 or higher).
#' @param upper_trunc the upper truncation value (numeric scalar that must be
#'   higher than \code{lower_trunc}). Set to Inf for no upper truncation
#' @param x_transform a character vector specifying the transformation for AUC
#'   calculation, if any. Must be one of "log10" (default) or "raw".
#' @return Returns a data frame with \code{time}, \code{surv}, \code{n.risk},
#'   \code{n.event}, and \code{n.censor} (\code{survival::summary.survfit}
#'   output format), and \code{aucMB}.
#'
#' @details
#'
#' AUC is calculated from 0 (or 1 if \code{x_transform = "log10"}) to the
#'  \code{x} values (after \code{lower_trunc}/\code{upper_trunc} \code{x}
#'  value truncation).
#'
#' If \code{response} is given, non-responding values (\code{response} = 0 or
#'  FALSE) will have their values set to the \code{lower_trunc} for MB curves
#'  and AUC calculations.
#'
#'
#' The output can be used for plotting step function curves,
#'   with \code{time} on the x axis, \code{surv} on the y axis, and
#'   \code{n.censor == 1} subset can be used for a \code{ggplot2::geom_point()}
#'   layer. Note if \code{x_transform = 'log10'} the resulting plot is best
#'   displayed on the log10 scale (\code{ggplot2::scale_x_log10}).
#'
#' \code{aucMB} can be used for boxplots and group comparisons. Note
#'  \code{aucMB} is repeated for values of \code{time} and \code{surv}.
#'
#' @examples
#'
#'
#' mb_results(96:105, c(rep(0,5), rep(1,5)), x_transform = 'log10')
#' mb_results(96:105, c(rep(0,5), rep(1,5)), x_transform = 'raw')
#'
#' # Simple Example
#' library(dplyr)
#' dat = data.frame(x = c(500,800,20,150,30000,10,1,2000,10000,900),
#'                  response = c(1,1,0,1,1,0,0,1,1,1),
#'                  ptid = c(1,1,2,2,3,3,3,3,3,3))
#' ind_results <-
#'  dat %>%
#'   dplyr::group_by(ptid) %>%
#'   dplyr::group_modify(~ mb_results(x = .x$x, response = .x$response))
#'
#' overall_results <-
#'  mb_results(x = dat$x, response = dat$response)
#'
#' ggplot2::ggplot(data = overall_results, ggplot2::aes(x = time, y = surv)) +
#'   ggplot2::geom_step(data = ind_results, ggplot2::aes(group = ptid),
#'           linetype = "dashed", direction = 'hv', lwd = .35, alpha = .7) +
#'   ggplot2::geom_step(direction = 'hv', lwd = .65) +
#'   ggplot2::scale_x_log10()
#'
#'
#' # BAMA Assay Example comparing MB Across Antigens
#'
#' data(exampleData_BAMA)
#'
#' data_here <-
#'  exampleData_BAMA %>%
#'  filter(visitno == 2)
#'
#' group_results <-
#'   data_here %>%
#'   dplyr::group_by(group) %>%
#'   dplyr::group_modify(~ mb_results(x = .x$magnitude , response = .x$response))
#'
#' ind_results <-
#'   data_here %>%
#'   dplyr::group_by(group, pubID) %>%
#'   dplyr::group_modify(~ mb_results(x = .x$magnitude , response = .x$response))
#'
#' ggplot2::ggplot(data = group_results,
#'                 ggplot2::aes(x = time, y = surv, color = factor(group))) +
#'   ggplot2::geom_step(data = ind_results, ggplot2::aes(group = pubID),
#'           linetype = "dashed", direction = 'hv', lwd = .35, alpha = .7) +
#'   ggplot2::geom_step(direction = 'hv', lwd = .65) +
#'   ggplot2::scale_x_log10('Response Magnitude',
#'            breaks = c(100,1000,10000, 22000),
#'            labels = c(expression(""<=100),1000,10000, expression("">=22000))) +
#'   ggplot2::ylab('Magnitude Breadth (%)') +
#'   ggplot2::scale_color_discrete('Group') +
#'   ggplot2::coord_cartesian(xlim = c(95, 23000)) +
#'   ggplot2::theme_bw()
#'
#'
#'  # AUC-MB plot
#'
#' AUC_MB <- dplyr::distinct(ind_results, group, pubID, aucMB)
#'
#' ggplot2::ggplot(AUC_MB, ggplot2::aes(x = factor(group), y = aucMB,
#'                                      color = factor(group))) +
#'   ggplot2::geom_boxplot(outlier.color = NA, show.legend = FALSE) +
#'   ggplot2::geom_point(position = ggplot2::position_jitter(width = 0.25,
#'            height = 0, seed = 1), size = 1.5, show.legend = FALSE) +
#'   ggplot2::scale_y_log10('AUC-MB') +
#'   ggplot2::xlab('Group') +
#'   ggplot2::theme_bw()
#'
#' @export


mb_results <- function(x,
                       response = NULL,
                       lower_trunc = 100,
                       upper_trunc = 22000,
                       x_transform= c('log10','raw')){

  .check_numeric_input(x, lower_bound = 0)
  x_transform <- match.arg(x_transform)
  .check_numeric_input(lower_trunc,
                       scalar = TRUE,
                       lower_bound = ifelse(x_transform == 'log10', 1, 0)

  )
  .check_numeric_input(upper_trunc,
                       scalar = TRUE,
                       lower_bound = lower_trunc + 1e-14
  )

  if (!is.null(response)) {
    .check_response_input(response)
    if (length(x) != length(response)) {
      stop('"x" and "response" must be same length')
    }
    # #When response = 0 x should be set to 0
    x <- x * response
  }
  #Truncation
  x <- pmin(pmax(lower_trunc, x), upper_trunc)

  surv_results <- create_step_info(x)

  if (x_transform == 'raw') {
    auc_results <- sum(diff(surv_results$time) *
                         utils::head(surv_results$surv, -1))
  } else {
    #Need to truncate at 1 since log transform
    surv_results$time[surv_results$time == 0] <- 1
    auc_results <- 10 ^ (sum(diff(log10(surv_results$time)) *
                               utils::head(surv_results$surv, -1)))
  }
  surv_results$aucMB <- auc_results
  return(surv_results)
}


