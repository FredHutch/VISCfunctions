# FUNCTION WilsonScoreLimit
# computes upper or lower CI limit
# Arguments:
#    n (# in cohort)
#    npos (number of positive responders)
#    ci (confidence interval as integer, e.g., 95 for 95% CI)
# Returns: a length-2 vector representing Wilson's score confidence interval limits as c(lower, upper)
WilsonScoreLim <- function(n, npos, ci=95){
  z <- qnorm(1 - (100-ci)/100/2)
  p <- npos/n
  denom <- 1+z*z/n
  t1 <- p + z*z/2/n
  t2 <- z*sqrt(p*(1-p)/n + z*z/4/n/n)
  return(c((t1 - t2)/denom, (t1 + t2)/denom))
}


# FUNCTION fmtPCT
# format percent string for output tables
# Arguments:
#    n (# in cohort)
#    npos (number of positive responders)
# Returns: Percent positive string, e.g., "8/14 = 57.14%"
fmtPCT <- function(n, npos){paste0(npos, "/", n, " = ", round(npos*100/n,2),"%")}

# FUNCTION fmtCI
# format CI string for output tables
# Arguments:
#    lcl (lower confidence interval limit)
#    ucl (upper confidence interval limit)
# Returns: CI string, e.g., "(32.6%, 78.6%)"
fmtCI <- function(lcl, ucl){paste0("(", round(lcl*100,1), "%, ", round(ucl*100,1), "%)")}


# FUNCTION  prtDT
# data.table implementation of point_response_table macro to compute response rates and CIs, formatted for report PDFs
# Arguments:
#    dt (name of input data.table that includes keyVars and responseVar variable)
#    keyVars (summary ("by") variables, provided as a character vector, e.g. c("tcellsub", "antigen", "cytoskine", "week"))
# Returns: data.table that includes keyVars, string of response counts, rates, and confidence intervals
prtDT <- function(dt, keyVars = c("T-cell Subset", "Peptide Pool", "Cytokine", "Week"), resp="response"){
  dt[, {n = sum(!is.na(get(resp)))
        npos = sum(get(resp), na.rm = TRUE)
        lcl = WilsonScoreLim(n, npos)[1] # could use alternate CI method function here
        lcl[lcl<0] <- 0                  # truncate at 0
        ucl = WilsonScoreLim(n, npos)[2] # could use alternate CI method function here
        percent = fmtPCT(n, npos)
        CI = fmtCI(lcl, ucl)
        list(percent=percent, CI=CI)}, # this list determines what is returned (plus keyVars)
     by = keyVars]
}


# FUNCTION englist()
# Looks at the unique values of some variable, lists them as text, and inserts "and" before the last one
# inputs:
#  df (name of ics data.table),
#  var (name of var to summarize),
#  varnice (optional text to prepend to numeric vars, e.g. "Week" to turn 4 into "Week 4")
englist <- function(df, var, varnice="", excl="NEG", oxford=TRUE){
  pars <- as.list(match.call()[-1])
	lst <- unique(df[[eval(quote(var))]])
  lst <- paste0(varnice, lst)
  lst <- lst[lst != excl]
  len <- length(lst)
	if(len>1){
		lst[len] <- paste("and", lst[len])
	}
	if(oxford==TRUE){
	  return(paste(lst, collapse=", "))
	} else {
		return(paste(c(paste(lst[1:len-1], collapse=", "), lst[len]), collapse=" "))
	}
}


# FUNCTION color()
# Capitalize the first letter of each word in a string (to nice-ify variable names for LaTeX tables)
color <- function(x, cond, color){
  ifelse(eval(cond), x, paste0("\\textcolor{", color, "}{", x, "}"))
}


# FUNCTION simpleCap()
# Capitalize the first letter of each word in a string (to nice-ify variable names for LaTeX tables)
simpleCap <- function(x){
  s <- strsplit(x, " ")[[1]]
  paste(toupper(substring(s, 1,1)), substring(s, 2),
      sep="", collapse=" ")
}


# FUNCTION bold.allcolheads
# by Bhavesh Borate
# supplied as function for xtable.sanitize.colnames.function option
bold.allcolheads <- function(x){
  paste('\\textbf{',x,'}', sep ='')
}


# FUNCTION qe()
# qe = Quadruple Escape
# shorthand for gsub("_", "\\\\_", x)
qe <- function(x){
  gsub("_", "\\\\_", x, fixed=FALSE)
}


# FUNCTION spellNum()
# spell out numerals 1-9, use cap argument to capitalize it.
spellNum <- function(num, cap=FALSE){
	m <- data.table(n = c(1, 2, 3, 4, 5, 6, 7, 8, 9),
									word = c("one", "two", "three", "four", "five", "six", "seven", "eight", "nine"),
									Word = c("One", "Two", "Three", "Four", "Five", "Six", "Seven", "Eight", "Nine"))
	if(num %in% m$n){return(m[num==n, ifelse(cap==FALSE, word, Word)])} else {return(num)}
}

# makes formatC friendly with *apply functions
formatCwrap <- function(arg, digits=2, format="f"){
	formatC(arg, digits=digits, format=format, big.mark=",")
}


# FUNCTIONs NAto0() and NAtoBlank*()
# use with *apply, and maybe with .SD, to convert all NA character values to 0s or empty characters
NAto0 <- function(x){ifelse(is.na(x), "0", x)}
NAtoBlank <- function(x){ifelse(is.na(x), "", x)}
