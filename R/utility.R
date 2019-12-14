# FUNCTION getUsername
# gets user's real name from either Windows local or LDAP (for Mac and linux)
# Arguments:
#    id (default arg works except when Mac login id is different from LDAP id)
# Returns: User's first and last names as a string
getUsername <- function(id = Sys.getenv("USER")) {
  switch(Sys.info()[['sysname']],
         Windows = {
           myargs <- paste("user /domain", Sys.getenv("USERNAME"))
           user <- system2(command = "net",
                           args = myargs,
                           stdout = TRUE)
           user <-
             gsub("Full Name\ *", "", user[grepl("FULL NAME", toupper(user))])
           user <-
             paste(strsplit(gsub(",", "", user), " ")[[1]][c(2, 1)], collapse = " ")
         },
         Linux   = {
           myargs <-
             paste0("-x -h ldapint.pc.scharp.org -b dc=scharp,dc=org uid=", id)
           user <-
             system2("ldapsearch", args = myargs, stdout = TRUE)[11]
           user <- gsub("[a-z]+: ", "", user)
         },
         Darwin  = {
           myargs <-
             paste0("-x -h ldapint.pc.scharp.org -b dc=scharp,dc=org uid=", id)
           user <-
             system2("ldapsearch", args = myargs, stdout = TRUE)[11]
           user <- gsub("[a-z]+: ", "", user)
         })

  return(user)
}


# FUNCTION: kableit
# wrapper for kable() function
kableit <- function(obj,
                    format = "html",
                    full_width = FALSE,
                    ...) {
  kable(setDT(lapply(obj, function(x)
    x <-
      ifelse(is.na(
        x
      ), "", x))), format = format, ...) %>% kable_styling(full_width = full_width)
}

# FUNCTION: pubid()
# CONTEXT: Any
# Calculate pubids
pubid <- function(x,
                  prefix = "",
                  range = 1000:9999,
                  seed = 8675309)
{
  # add a hyphen to prefix if it's not empty and doesn't already have one
  prefix <-
    ifelse(prefix != "" &
             substr(prefix, nchar(prefix), nchar(prefix)) != "-",
           paste0(prefix, "-"),
           prefix)

  # handle case when input is non-unique or has NA values
  if (anyNA(x) | length(x) != length(unique(x))) {
    warning(
      "Input vector x has duplicate or missing values. Output values are based on unique(x[!is.na(x)])."
    )
  }
  x1 <- sort(unique(x[!is.na(x)]))

  # generate PubIDs
  set.seed(seed)
  out <-
    as.list(paste0(prefix, sample(
      x = range,
      size = length(x1),
      replace = FALSE
    )))
  names(out) <- x1
  return(out)
}

# FUNCTION: saveObj()
# CONTEXT: package building
# Description: Takes object inobj and assigns it to [pkgName]_[suffix], e.g. Pantaleo.RV.498_ICS_stats
#              Provide suffix as an expression rather than a string
saveObj <-
  function(inobj,
           suffix,
           prefix = pkgName,
           env = parent.frame()) {
    outname <- paste0(prefix, "_", suffix)
    assign(outname, get(inobj, envir = env), envir = env)
    message(paste0("Saving ", inobj, " as ", outname, "."))
    invisible(inobj)
  }
