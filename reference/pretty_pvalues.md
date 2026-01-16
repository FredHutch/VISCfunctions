# Round and format a vector of p-values

pretty_pvalues() takes a vector of p-values, rounds them to a specified
digit amount, allows options for emphasizing p-values \< the defined
significance level, and returns a character for missing.

## Usage

``` r
pretty_pvalues(
  pvalues,
  digits = 3,
  bold = FALSE,
  italic = FALSE,
  background = NULL,
  sig_alpha = 0.05,
  missing_char = "---",
  include_p = FALSE,
  trailing_zeros = TRUE,
  output_type = c("latex", "html", "pandoc", "no_markup")
)
```

## Arguments

- pvalues:

  numeric vector of raw p-values to be formatted

- digits:

  number of digits to round to; values with zeros past this number of
  digits are truncated

- bold:

  TRUE or FALSE: set to TRUE to bold p-values \< the defined
  significance level

- italic:

  TRUE or FALSE: set to TRUE to italicize p-values \< the defined
  significance level

- background:

  highlight color for p-values \< the defined significance level.
  Default = NULL (no highlighting)

- sig_alpha:

  the defined significance level. Default = 0.05

- missing_char:

  character string that will replace missing values from the p-value
  vector. Default = "—"

- include_p:

  TRUE or FALSE: set to TRUE to print "p = " before each p-value

- trailing_zeros:

  TRUE or FALSE: default = TRUE, p-values are formatted with trailing
  zeros to the defined number of digits (i.e. 0.100 instead of 0.1 if
  digits = 3)

- output_type:

  output type, "latex" (default), "html" , "pandoc" (for Word document
  output), or "no_markup"

## Value

Vector of transformed p-values for table output

## Details

With this function, there are two things to be noted: Since the p-value
vector formatting uses `cell_spec`, which generates raw HTML or LaTeX,
or uses custom markdown syntax for the pandoc option, make sure you
remember to put `escape = FALSE` into your kable code when generating
your table. At the same time, you will need to escape special symbols
manually. Additionally, `cell_spec` needs a way to know whether you want
HTML, LaTeX, or Markdown (pandoc) output. You can specify it locally in
the function or globally using `options(knitr.table.format = "latex")`.
If you don't provide anything, this function will output as HTML by
default.

For pandoc markup only bold and italic can be specified. Both can
jointly be specified for bold italics.

## Examples

``` r
pvalue_example = c(1, 0.06, 0.0005, NA, 1e-6)

pretty_pvalues(pvalue_example, background = "pink")
#> [1] "1.000"                     "0.060"                    
#> [3] "\\cellcolor{pink}{<0.001}" "---"                      
#> [5] "\\cellcolor{pink}{<0.001}"

pretty_pvalues(pvalue_example, digits = 4, missing_char = "missing",
               bold = TRUE)
#> [1] "1.0000"            "0.0600"            "\\textbf{0.0005}" 
#> [4] "missing"           "\\textbf{<0.0001}"

# How to use pretty_pvalues in reports
raw_pvals <- c(0.00000007, .05000001, NaN, NA, 0.783)
pretty_pvals <- pretty_pvalues(raw_pvals , digits = 3, background = "green",
                               italic = TRUE, bold = TRUE)
kableExtra::kable(pretty_pvals , format = "latex", escape = FALSE,
                  col.names = c("P-values"))
#> 
#> \begin{tabular}{l}
#> \hline
#> P-values\\
#> \hline
#> \cellcolor{green}{\em{\textbf{<0.001}}}\\
#> \hline
#> 0.050\\
#> \hline
#> ---\\
#> \hline
#> ---\\
#> \hline
#> 0.783\\
#> \hline
#> \end{tabular}
```
