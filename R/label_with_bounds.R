#' Format axis labels with optional boundary signs
#'
#' @param formatter A function taking numeric breaks and returning a character
#'   vector of the same length, such as `scales::label_percent()`.
#' @param lower,upper Single non-missing logical values indicating whether to
#'   prefix the smallest and largest finite breaks with inequality signs.
#' @return A function accepting numeric breaks and returning character labels.
#' @details Boundaries are identified from the original numeric breaks before
#'   formatting. All tied extrema are labelled. Non-finite breaks and missing
#'   formatted labels receive no sign. If both bounds are requested with only
#'   one distinct finite break, an error is raised. Empty input returns an empty
#'   character vector without calling the formatter.
#'
#'   This function changes labels only. Set breaks explicitly to match intended
#'   limits, and configure scale limits and out-of-bounds handling separately.
#'   Expression-returning formatters are not supported.
#' @examples
#' label_with_bounds(lower = TRUE, upper = TRUE)(c(0, 0.5, 1))
#' label_with_bounds(function(x) paste0(100 * x, "%"),
#'                   lower = TRUE, upper = TRUE)(c(0, 0.5, 1))
#' @export
label_with_bounds <- function(formatter = as.character,
                              lower = FALSE,
                              upper = TRUE) {
  if (!is.function(formatter)) {
    stop("`formatter` must be a function.", call. = FALSE)
  }
  check_flag <- function(x) {
    is.logical(x) && length(x) == 1L && !is.na(x)
  }
  if (!check_flag(lower) || !check_flag(upper)) {
    stop("`lower` and `upper` must be TRUE or FALSE.", call. = FALSE)
  }
  force(formatter)
  force(lower)
  force(upper)

  function(breaks) {
    if (!is.numeric(breaks) || !is.null(dim(breaks))) {
      stop("`breaks` must be a numeric vector.", call. = FALSE)
    }
    if (length(breaks) == 0L) {
      return(character())
    }
    labels <- formatter(breaks)
    if (!is.character(labels) || !is.null(dim(labels)) ||
        length(labels) != length(breaks)) {
      stop("`formatter` must return one character label per break.",
           call. = FALSE)
    }
    finite <- is.finite(breaks)
    if (!any(finite) || (!lower && !upper)) {
      return(labels)
    }
    lower_value <- min(breaks[finite])
    upper_value <- max(breaks[finite])
    if (lower && upper && lower_value == upper_value) {
      stop("Cannot label both bounds with only one distinct finite break.",
           call. = FALSE)
    }
    if (lower) {
      i <- which(finite & breaks == lower_value & !is.na(labels))
      labels[i] <- paste0("\u2264 ", labels[i])
    }
    if (upper) {
      i <- which(finite & breaks == upper_value & !is.na(labels))
      labels[i] <- paste0("\u2265 ", labels[i])
    }
    labels
  }
}
