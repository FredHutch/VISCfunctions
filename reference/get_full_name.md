# Get Full Username from ID

For a given ID looks up user name

## Usage

``` r
get_full_name(id = NULL)
```

## Arguments

- id:

  ID to look full name up. If null (default) looks up ID of current user

## Value

First and Last name associated with ID

## Details

If `id` null, uses system "USERNAME" variable for Windows and "USER"
variable for Linux and MACs. Full Name is found in Windows via the `net`
command, and via ldap search in Linux and MACs. The ldap search will
only work on SCHARPs network at Fred Hutching Cancer Research Center.

## Examples

``` r
get_full_name()
#> [1] "runner"
```
