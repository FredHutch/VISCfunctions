#' Sorted Trapezoidal Integration
#'
#' This function is a wrapper for the `trapz` function. It sorts the x and y values
#' so that it is monotonically increasing along the x values.  Then computes the
#' area of a function with values `y` and points `x`. There is an optional
#' parameter to remove any NA values so that integration can proceed by
#' omitting the `x` and `y` values where the NA occurred.
#'
#' @param x A vector of numeric values representing the x-axis over which to integrate.
#' @param y A vector of numeric values representing the y-axis.
#' @param na.rm A logical indicating whether to remove NA values from both x and y values.
#' @return Approximated integral of the function, discretized through the points
#' x, y, from \code{min(x)} to \code{max(x)}.
#'
#' @details
#'The points (x, 0) and (x, y) are taken as vertices of a polygon. The area is computed using the trapezoid rule for approximation the definite integral of the function.
#'
#' @examples
#' set.seed(93)
#'
#' # Calculate the area under the sine curve from 0 to pi:
#' n <- 101
#' x <- seq(0, pi, len = n)
#' y <- sin(x)
#' trapz_sorted(x, y)
#'
#' # Calculate area under the curve for unsorted data:
#' x <- sample(1:n, n, replace = F)
#' y <- runif(n, 0, 33)
#' trapz_sorted(x, y)
#'
#' # Calculate the area without NA removal
#' y[3] <- NA
#' # Will fail to produce a result
#' trapz_sorted(x, y, na.rm=FALSE)
#' trapz_sorted(x, y, na.rm=TRUE)
#'
#'
#'
#' @export
#'
trapz_sorted <- function(x, y, na.rm = TRUE){
  # Input Checking
  .check_numeric_input(x)
  .check_numeric_input(y)
  m <- length(x)
  if (length(y) != m) stop("Arguments 'x', 'y' must be vectors of the same length.")
  if (na.rm == TRUE){
    # Sort by increasing values, NA are removed from x
    x <- x[order(x, decreasing = FALSE, method = "auto", na.last = NA)]
    y <- y[order(x, decreasing = FALSE, method = "auto", na.last = NA)]
    # NA removed from y
    x <- x[!is.na(y)]
    y <- y[!is.na(y)]
  } else{
    # Warn that data points will be removed if NA
    if(anyNA(x)) warning("NA values are present in the 'x' values. Returns NA.")
    if(anyNA(y)) warning("NA values are present in the 'y' values. Returns NA.")
    # Sort by increasing values, NA are not removed
    x <- x[order(x, decreasing = FALSE, method = "auto")]
    y <- y[order(x, decreasing = FALSE, method = "auto")]
  }
  # Check for length of x and y after NA removal
  m <- length(x)
  if (m < 2) {
    warning("There are less than 2 observations with non-missing values of both 'x' and 'y', so NA returned.")
    return(NA)
  } else {
    # Integrate
    xp <- c(x, x[m:1])
    yp <- c(numeric(m), y[m:1])
    n <- 2 * m
    p1 <- sum(xp[1:(n - 1)] * yp[2:n]) + xp[n] * yp[1]
    p2 <- sum(xp[2:n] * yp[1:(n - 1)]) + xp[1] * yp[n]
    return(0.5 * (p1 - p2))
    }
}




