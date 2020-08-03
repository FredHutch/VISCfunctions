#' Replace repeated rows in a data.frame with NA
#'
#' This function is for replacing repeated rows in a data.frame into NA for nice printing. This
#' is not intended for use during processing.
#'
#' @param .data a data.frame
#' @param ... Columns to use to identify which to rows to replace with NA's
#'
#' @return a data.frame where repeated rows in the columns identified by ... are replaced with NA
#
#' @examples
#' sample_df <- data.frame(
#'   x = c(1, 1, 1, 2, 2, 2, 2),
#'   y = c('test1', 'test1', 'test2', 'test1', 'test2', 'test2', 'test1'),
#'   z = c(1, 2, 3, 4, 5, 6, 7),
#'   outputVal = runif(7)
#' )
#'
#' library(dplyr)
#' options(knitr.kable.NA = '')
#' collapse_group_row(sample_df, x, y, z) %>%
#'  kableExtra::kable() %>%
#'  kableExtra::kable_styling()
#' @export
collapse_group_row <- function(.data, ...) {
    stopifnot(is.data.frame(.data))

    # This bit allows for non-standard evaluation. We can choose to not use this and
    # have a vector passed with the fields to be using.
    fields <- as.character(as.list(substitute(substitute(...)))[-1])
    # Remove ' from the beginning and end of the field names
    fields <- gsub("[`]$", "", gsub("^[`]", "", fields))

    alt_fields <- setdiff(colnames(.data), fields)

    # make sure all column names passed exist
    stopifnot(all(fields %in% names(.data)))

    # sort row order of fields
    nice_fields <- ifelse(grepl(" ", fields), paste0("`", fields, "`"), fields)
    .data <- .data[eval(parse(text = paste0("with(.data,order(", paste(nice_fields,
        collapse = ","), "))"))), ]

    # create 'new' fields containing the result of collapsing rows
    for (i in 0:(length(fields) - 1)) {
        field_of_interest <- fields[length(fields) - i]
        duplicated_rows <- duplicated(.data[, fields[1:(length(fields) - i)]])
        .data[duplicated_rows, field_of_interest] <- NA
    }

    .data[, c(paste0(fields), alt_fields)]

}
