#' Add inequality signs to axis limit labels
#'
#' Label the smallest or largest finite break with an inequality sign.
#' These functions can be supplied directly to the `labels` argument of a
#' ggplot2 continuous scale.
#'
#' @param breaks A numeric vector of axis breaks.
#' @return A character vector with the same length and order as `breaks`.
#'   Missing and non-finite breaks are preserved without an inequality sign.
#' @details These helpers change labels only, not data or scale limits.
#'   Set breaks explicitly so the extreme break equals the intended assay
#'   or plotting limit. To display out-of-range observations at a boundary,
#'   configure out-of-bounds handling separately (for example,
#'   `oob = scales::squish`). With a single finite break, that break is labelled.
#'   All occurrences of a tied extreme receive the sign.
#' @examples
#' add_gte_to_upper_limit(c(0, 10, 20, 30))
#' add_lte_to_lower_limit(c(100, 1000, 10000))
#'
#' if (requireNamespace("ggplot2", quietly = TRUE)) {
#'   ggplot2::ggplot(mtcars, ggplot2::aes(wt, mpg)) +
#'     ggplot2::geom_point() +
#'     ggplot2::scale_y_continuous(
#'       limits = c(0, 30), breaks = c(0, 10, 20, 30),
#'       labels = add_gte_to_upper_limit
#'     )
#' }
#' @export
add_gte_to_upper_limit <- function(breaks) {
  axis_limit_labels(breaks, upper = TRUE)
}

#' @rdname add_gte_to_upper_limit
#' @export
add_lte_to_lower_limit <- function(breaks) {
  axis_limit_labels(breaks, upper = FALSE)
}

axis_limit_labels <- function(breaks, upper) {
  if (!is.numeric(breaks) || !is.null(dim(breaks))) {
    stop("`breaks` must be a numeric vector.", call. = FALSE)
  }
  labels <- as.character(breaks)
  finite <- is.finite(breaks)
  if (!any(finite)) {
    return(labels)
  }
  limit <- if (upper) max(breaks[finite]) else min(breaks[finite])
  at_limit <- which(finite & breaks == limit)
  sign <- if (upper) "\u2265 " else "\u2264 "
  labels[at_limit] <- paste0(sign, labels[at_limit])
  labels
}
