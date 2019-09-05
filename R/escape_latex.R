#' Protect control characters in a string for use in a latex table or caption
#'
#' escape takes a vector of chracter values, and puts "\\" in front of them to make them
#' allowable in the latex table output
#'
#' @param x character vector of test containing values that need to be latex escaped
#'
#' @return character Vector of transformed values for table output
#
#' @examples
#' value_example <- c("testvalue", "test_value", "ampersand&")
#' escape(value_example)
#' escape("String_Entry %")
#'
#' @export
escape<-function(x){
  gsub("([&%$#_{}~^\\])","\\\\\\1",x)
}
