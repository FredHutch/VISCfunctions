#################
# NAb
#################


# FUNCTION:  nabTiter()
# CONTEXT: NAb
# Description: Convert text titer strings to numeric, handling > and <
nabTiter <-
  function(titer,
           method = c("dilution", "concentration")) {
    method <- match.arg(method)
    switch(
      method,
      dilution = ifelse(
        grepl("<", titer),
        as.numeric(gsub("[<>]", "", titer)) / 2,
        ifelse(grepl(">", titer), as.numeric(gsub("[<>]", "", titer)),
               as.numeric(titer))
      ),
      concentration = ifelse(
        grepl("<", titer),
        as.numeric(gsub("[<>]", "", titer)) / 2,
        ifelse(grepl(">", titer), as.numeric(gsub("[<>]", "", titer)) *
                 2,
               as.numeric(titer))
      )
    )
  }

# FUNCTION:  nabResponse()
# CONTEXT: NAb
# Description: NAb response call
nabResponse <- function(titer) {
  response <- ifelse(grepl("<", titer), 0, 1)
  return(response)
}
