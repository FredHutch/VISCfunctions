# Protect control characters in a string for use in a latex table or caption

escape takes a vector of chracter values, and puts "\\ in front of them
to make them allowable in the latex table output

## Usage

``` r
escape(x)
```

## Arguments

- x:

  character vector of test containing values that need to be latex
  escaped

## Value

character Vector of transformed values for table output

## Examples

``` r
value_example <- c("testvalue", "test_value", "ampersand&")
escape(value_example)
#> [1] "testvalue"    "test\\_value" "ampersand\\&"
escape("String_Entry %")
#> [1] "String\\_Entry \\%"
```
