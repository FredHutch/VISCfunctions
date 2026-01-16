# Sorted Trapezoidal Integration

This function is a wrapper for the `trapz` function. It sorts the x and
y values so that it is monotonically increasing along the x values. Then
computes the area of a function with values `y` and points `x`. There is
an optional parameter to remove any NA values so that integration can
proceed by omitting the `x` and `y` values where the NA occurred.

## Usage

``` r
trapz_sorted(x, y, na.rm = TRUE)
```

## Arguments

- x:

  A vector of numeric values representing the x-axis over which to
  integrate.

- y:

  A vector of numeric values representing the y-axis.

- na.rm:

  A logical indicating whether to remove NA values from both x and y
  values.

## Value

Approximated integral of the function, discretized through the points x,
y, from `min(x)` to `max(x)`.

## Details

The points (x, 0) and (x, y) are taken as vertices of a polygon. The
area is computed using the trapezoid rule for approximation the definite
integral of the function.

## Examples

``` r
set.seed(93)

# Calculate the area under the sine curve from 0 to pi:
n <- 101
x <- seq(0, pi, len = n)
y <- sin(x)
trapz_sorted(x, y)
#> [1] 1.999836

# Calculate area under the curve for unsorted data:
x <- sample(1:n, n, replace = FALSE)
y <- runif(n, 0, 33)
trapz_sorted(x, y)
#> [1] 1820.19

# Calculate the area without NA removal
x[10] <- NA
y[3] <- NA
# Will fail to produce a result
trapz_sorted(x, y, na.rm=FALSE)
#> Warning: NA values are present in the 'x' values. Returns NA.
#> Warning: NA values are present in the 'y' values. Returns NA.
#> [1] NA
trapz_sorted(x, y, na.rm=TRUE)
#> [1] 1795.662


```
