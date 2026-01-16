# Shorten git hash

Internal function for Reproducibility Tables. find '@' followed by 40
hex digits, and substitute with the '@' and the first 7 hex digits in
()-captured group.

## Usage

``` r
shorten_git_hash(x)
```

## Arguments

- x:

  String containing `@` followed by long git hash

## Value

String containing `@` followed by short git hash
