# FUNCTION: dataProfile()
# CONTEXT: BAMA, NAB
# describe data (variable list, # rows, # unique non-missing values, # missing values)
dataProfile <- function(df) {
  dat <- data.frame(Var = names(df), stringsAsFactors = FALSE)
  dat$varClass <- lapply(X = df, function(x)
    class(x))
  dat$NumRows <- lapply(X = df, function(x)
    length(x))
  dat$uniqVals <-
    lapply(X = df, function(x)
      length(which(!is.na(unique((x)
      )))))
  dat$NumNAs <- lapply(X = df, function(x)
    length(which(is.na(x))))
  dat$NumBlankStr <- lapply(X = df, function(x) {
    length(which(x == ""))
  })
  return(dat)
}
