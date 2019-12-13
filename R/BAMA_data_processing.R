# --------------------------------------------------#
#  UTILITY FUNCTIONS FOR BAMA ASSAY DATA PROCESSING #
# --------------------------------------------------#

# FUNCTION getUsername
# gets user's real name from either Windows local or LDAP (for Mac and linux)
# Arguments:
#    id (default arg works except when Mac login id is different from LDAP id)
# Returns: User's first and last names as a string
getUsername <- function(id=Sys.getenv("USER")){
  switch(Sys.info()[['sysname']],
         Windows = {
           myargs <- paste("user /domain", Sys.getenv("USERNAME"))
           user <- system2(command="net", args=myargs, stdout=TRUE)
           user <- gsub("Full Name\ *", "", user[grepl("FULL NAME", toupper(user))])
           user <- paste(strsplit(gsub(",", "", user), " ")[[1]][c(2, 1)], collapse=" ")
         },
         Linux   = {
           myargs <- paste0("-x -h ldapint.pc.scharp.org -b dc=scharp,dc=org uid=", id)
           user <- system2("ldapsearch", args = myargs, stdout=TRUE)[11]
           user <- gsub("[a-z]+: ", "", user)
         },
         Darwin  = {
           myargs <- paste0("-x -h ldapint.pc.scharp.org -b dc=scharp,dc=org uid=", id)
           user <- system2("ldapsearch", args = myargs, stdout=TRUE)[11]
           user <- gsub("[a-z]+: ", "", user)
         })

  return(user)
}




# FUNCTION: BAMAresponseCall()
# CONTEXT: BAMA
# compute response call by standard 3-part definition
# force NA if any one of the conditions is NA. Otherwise, a FALSE leads to response=0 even if one condition is NA
# Arguments:
#      dat: data.frame with N rows for response calls to be applied
#      def: vector with N rows indicating for each whether delta or ratio is to be used
#      idea for future improvement: allow a single value or a vector. Function tests for length of vector and proceeds accordingly
# accepts def values of deltaX (standard 3-part definition) or ratioXXX where X is the ratio cutoff (2.5 by default),
#   so e.g. provide "delta3" or "ratio2.5" or "ratio5" for second argument.
#   def can be a vector of length 1 or of length nrow(dat).
BAMAresponseCall <- function(dat, def="delta3", sermuc="ser"){
  if(length(def)==1){def <- rep(def, nrow(dat))}
  rc <- as.integer(rep(NA, nrow(dat)))
  fold <- as.numeric(substr(def,6,10))
  deltaInx <- which(substr(tolower(def),1,5)=="delta")
  ratioInx <- which(substr(tolower(def),1,5)=="ratio")
  if(tolower(sermuc)=="ser"){
    cond1 <- dat$delta >= dat$pos_threshold
    cond2 <- dat$delta > pmax(0, fold * dat$delta_baseline)
    cond3 <- dat$fi_bkgd > pmax(0, fold * dat$fi_bkgd_baseline)
    rc[deltaInx] <- ifelse(apply(cbind(cond1, cond2, cond3)[deltaInx,], 1, function(x) sum(is.na(x))) > 0, NA, as.integer(cond1 & cond2 & cond3)[deltaInx])
    cond1 <- dat$fi_bkgd >= dat$pos_threshold
    cond2 <- dat$fi_bkgd / dat$fi_bkgd_blank > fold
    cond3 <- dat$fi_bkgd > 3 * dat$fi_bkgd_baseline
    rc[ratioInx] <- ifelse(apply(cbind(cond1, cond2, cond3)[ratioInx,], 1, function(x) sum(is.na(x))) > 0, NA, as.integer(cond1 & cond2 & cond3)[ratioInx])
  } else if(tolower(sermuc)=="muc"){
    cond1 <- dat$delta >= 100
    cond2 <- dat$spec_actvy > fold * dat$spec_actvy_baseline
    rc <- ifelse(apply(cbind(cond1, cond2), 1, function(x) sum(is.na(x))) > 0, NA, as.integer(cond1 & cond2))
  }
  return(rc)
}




# FUNCTION: GenerateAUC()
# Description : Compute the area (above the x-axis) of a function with values y at the points x,
#               where the area under the x-axis is set to 0.
# Arguments   :
#           x : x-coordinates of points on the x-axis.
#           y : y-coordinates of function values.
# Arguments for BAMA data :
#           x : log10(dilution). Note, dilution levels must be in increasing order
#           y : Corresponding MFI*(background-subtracted fi - blank). Note,
#               negative background-subtracted blank values should be replaced by 0
# Details     :
#         If sum of the two adjacent y is negative, set the area of the corresponding trapezoid to 0;
#         If sum of the two adjacent y is positive, the area of the corresponding trapezoid is equal
#         to the average of two y times the two x distance.
# Author      : Lily Zhang
# Date        : 11-13-2014
GenerateAUC <- function(x, y){
  m <- length(x)
  #average y
  y1 <- y[1:(m-1)]
  y2 <- y[2:m]
  y_m <- (y1+y2)/2
  #set negative values to 0
  y_m <- ifelse(y_m<0, 0, y_m)
  #calculate x distance
  x1 <- x[1:(m-1)]
  x2 <- x[2:m]
  x_w <- x2-x1
  #each trapezoid area
  tArea <- x_w*y_m
  return(sum(tArea))
}




# FUNCTION: dataProfile()
# CONTEXT: BAMA, NAB
# describe data (variable list, # rows, # unique non-missing values, # missing values)
dataProfile <- function(df){
  dat <- data.frame(Var = names(df), stringsAsFactors=FALSE)
  dat$varClass <- lapply(X=df, function(x) class(x))
  dat$NumRows <- lapply(X=df, function(x) length(x))
  dat$uniqVals <- lapply(X=df, function(x) length(which(!is.na(unique((x))))))
  dat$NumNAs <- lapply(X=df, function(x) length(which(is.na(x))))
  dat$NumBlankStr <- lapply(X=df, function(x){length(which(x==""))})
  return(dat)
}



# FUNCTION: kableit
# wrapper for kable() function
kableit <- function(obj, format="html", full_width = FALSE, ...){
  kable(setDT(lapply(obj, function(x) x <- ifelse(is.na(x), "", x))), format = format, ...) %>% kable_styling(full_width = full_width)
}


# FUNCTION: addQCind
# CONTEXT: BAMA
# determine observations that should get flagged for filtering (missing baseline, missing FI, missing visit, etc.)
addQCind <- function(dat, bl){

	# keep only needed columns to save memory/time
	N <- nrow(dat)

	f1 <- f2 <- f3 <- f4 <- f5 <- rep(NA, N)

  # identify samples with missing FI
  missingfi <- which(is.na(dat$delta) | is.na(dat$fi_bkgd) | is.na(dat$fi_bkgd_blank))
	f1[missingfi] <- "Missing MFI"

	# missing baseline
	missingbl <- which(is.na(dat$delta_baseline) | is.na(dat$fi_bkgd_baseline) | is.na(dat$fi_bkgd_blank_baseline))
	f2[missingbl] <- "Missing baseline MFI"

	# identify samples with high sample-specific background
  highbkgd <- which(dat$fi_bkgd_blank > 5000)
	f3[highbkgd] <- "Blank MFI > 5000"

	# identify samples with high sample-specific background at baseline
	highbkgdbl <- which(dat$fi_bkgd_blank_baseline > 5000)
	f4[highbkgdbl] <- "Baseline Blank MFI* > 5000"

	# identify samples with high baseline MFI*
	highMFIbl <- which(dat$delta_baseline > 5000)
	f5[highMFIbl] <- "Baseline MFI* > 5000"

	# create single filter flag with reason (high baseline is programmed above but not implemented here)
	filter_reason <- apply(cbind(f1, f2, f3, f4), 1, function(x){paste0(x[!is.na(x)], collapse=", ")})
	filter_reason[filter_reason==""] <- "Not Filtered"
	return(filter_reason)
}





# FUNCTION: writePerm()
# CONTEXT: BAMA
# write rda file for data package, or write qc csv file for Tomaras lab (depending on qc argument)
writePerm <- function(df, suffix, excl = NULL, qc = FALSE,env){
  qckeepVars <- c("notebook", "instrument_type", "filename", "testdt", "detection", "isotype", "spectype", "spec_primary", "ptid", "visitno",
                  "antigen", "antigen_lot", "beadsetno", "dilution", "pctcv", "fi", "fi_bkgd", "blank", "fi_bkgd_blank",
                  "fi_bkgd_baseline", "delta", "pos_threshold", "response", "filter_reason", "tab", "spec_actvy", "auc_atlas")
                  # NOTE: group should never be in here, for lab blinding.
  if(!is.null(excl)){qckeepVars <- setdiff(qckeepVars, excl)}

  outname <- paste(pkgName, suffix,sep="_")
  if(qc==TRUE){
    write.csv(df[, qckeepVars], file.path(DataPackageR::project_data_path(),paste0("QC_", outname, ".csv")), row.names=FALSE)
    message("Writing ", paste0("QC_", outname, ".csv"))
  } else {
    return(df[, qckeepVars])
  }
}


# FUNCTION: get_titer_fun()
# CONTEXT: ADCC LUC
# compute ADCC Luciferase titer
get_titer_fun <- function(cutoff_in, activity_in, dilution_in){
  data_here <- data.table(activity_in, dilution_in)
  
  if (any(data_here$activity_in > cutoff_in, na.rm=TRUE)) {
    
    #sorting data by dilution (lowest to highest)
    setkey(data_here, dilution_in)
    for (i in 1:(nrow(data_here) - 1)) {
      
      #Checking if cutoff is between the next two points
      if (data_here$activity_in[i] >= cutoff_in & data_here$activity_in[i + 1] < cutoff_in) {
        slope_here <- (data_here$activity_in[i] - data_here$activity_in[i + 1]) / (log10(data_here$dilution_in[i]) - log10(data_here$dilution_in[i + 1]))
        int_here <- data_here$activity_in[i] - (slope_here * log10(data_here$dilution_in[i]))
        titer_here <- 10 ^ ((cutoff_in - int_here) / slope_here)
        # Adding break because just want most conservative titer value
        break
      }
    }
    
    #Setting Maximum dilution if last dilution above cutoff
    if (data_here$activity_in[nrow(data_here)] > cutoff_in) {
      titer_here <- as.double(data_here$dilution_in[nrow(data_here)])
    }
  } else if (length(which(is.na(data_here$activity_in))) == nrow(data_here)) {
    titer_here <- as.double(NA)
  } else {
    #Non-Responder
    titer_here <- as.double(min(data_here$dilution_in))
  }
  titer_here
}

# FUNCTION: pubid()
# CONTEXT: Any
# Calculate pubids
pubid <- function(x, prefix="", range=1000:9999, seed=8675309)
{
  # add a hyphen to prefix if it's not empty and doesn't already have one
  prefix <- ifelse(prefix != "" & substr(prefix, nchar(prefix), nchar(prefix)) != "-", paste0(prefix, "-"), prefix)

  # handle case when input is non-unique or has NA values
  if(anyNA(x) | length(x) != length(unique(x))){
    warning("Input vector x has duplicate or missing values. Output values are based on unique(x[!is.na(x)]).")
  }
  x1 <- sort(unique(x[!is.na(x)]))

  # generate PubIDs
  set.seed(seed)
  out <- as.list(paste0(prefix, sample(x=range, size=length(x1), replace=FALSE)))
  names(out) <- x1
  return(out)
}



# FUNCTION: saveObj()
# CONTEXT: package building
# Description: Takes object inobj and assigns it to [pkgName]_[suffix], e.g. Pantaleo.RV.498_ICS_stats
#              Provide suffix as an expression rather than a string
saveObj <- function(inobj, suffix, prefix = pkgName,env=parent.frame()){
  outname <- paste0(prefix, "_", suffix)
  assign(outname, get(inobj,envir = env), envir = env)
  message(paste0("Saving ", inobj, " as ", outname, "."))
  invisible(inobj)
}



#################
# ICS
#################

# FUNCTION: get_MIMOSA_probs_fun()
# CONTEXT: ICS
# Description : get MIMOSA response probabilities and fdr p values for a given timepoint
# Arguments
#     data_in        : data for a specific study, antigen, cytokine combination, and tcellsub (data.table)
#     ref_antigen_in : baseline antigen to use (character scaler)
#     seed_in        : seed for MIMOSA (scaler)
# Note that many variable names here assume standard flow processing code. You may have to update the function if data columns are named differently.
get_MIMOSA_probs_fun <- function(data_in, ref_antigen_in, seed_in = 537526546){
  tmp <- copy(data_in)
  tmp[, CountNeg := ParentCount - Count]

  #Need to Change ref_antigen_in level so I can run it in the ConstructMIMOSAExpressionSet function
  tmp[Stim == ref_antigen_in, Stim := "ref_antigen_in"]
  E <- ConstructMIMOSAExpressionSet(tmp,
                                    reference = Stim == "ref_antigen_in",
                                    measure.columns = c("Count", "CountNeg"),
                                    default.cast.formula = component ~ AnimalID + Stim + Population + Parent,
                                    .variables = .(AnimalID, Population, Parent),
                                    featureCols = 1,
                                    ref.append.replace = "_REF")

  set.seed(seed_in)

  result <- MIMOSA(CountNeg + Count ~ AnimalID | Stim,
                   data = E,
                   method = "mcmc",
                   subset = RefTreat %in% 'Treatment',
                   ref = RefTreat %in% 'Reference',
                   run.parallel = FALSE,
                   seed=seed_in)

  MIMOSA_response_prob <- getZ(result)[,'Pr.response']
  MIMOSA_fdr_P <- adjustMimosaFDR(MIMOSA_response_prob)
  MIMOSA_N <- nrow(tmp[Stim!="ref_antigen_in"])

  out <- list(Results = data.table(AnimalID = E@phenoData@data[grep('Treatment',rownames(E@phenoData@data)), 1],
                                   Stim = E@phenoData@data[grep('Treatment',rownames(E@phenoData@data)), 2],
                                   Population = E@phenoData@data[grep('Treatment',rownames(E@phenoData@data)), 3],
                                   Parent = E@phenoData@data[grep('Treatment',rownames(E@phenoData@data)), 4],
                                   MIMOSA_response_prob,
                                   MIMOSA_fdr_P,
                                   MIMOSA_N),
              seed_used = seed_in)
  return(out)
}


# FUNCTION: adjustMimosaFDR()
# CONTEXT: ICS (MIMOSA)
# Description : get fdr q values from MIMOSA response probabilities (i.e. for MIMOSA and MIMOSA2 response probabilities)
adjustMimosaFDR = function(x){
  dt = data.table(1:length(x),pr.null=1-x,pr.r=x)
  ret = dt[order(pr.null,decreasing=F),q:=cumsum(pr.null)/.I]
  ret[, q_final := max(q), by = pr.r]
  return(ret$q_final)
}


#################
# NAb
#################


# FUNCTION:  nabTiter()
# CONTEXT: NAb
# Description: Convert text titer strings to numeric, handling > and <
nabTiter <- function(titer, method = c("dilution", "concentration")){
  method <- match.arg(method)
  switch(method,
         dilution = ifelse(grepl("<", titer), as.numeric(gsub("[<>]", "", titer))/2,
                           ifelse(grepl(">", titer), as.numeric(gsub("[<>]", "", titer)),
                                  as.numeric(titer))),
         concentration = ifelse(grepl("<", titer), as.numeric(gsub("[<>]", "", titer))/2,
                                ifelse(grepl(">", titer), as.numeric(gsub("[<>]", "", titer))*2,
                                       as.numeric(titer)))
  )
}

# FUNCTION:  nabResponse()
# CONTEXT: NAb
# Description: NAb response call
nabResponse <- function(titer){
  response <- ifelse(grepl("<", titer), 0, 1)
  return(response)
}


