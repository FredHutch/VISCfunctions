# --------------------------------------------------#
#  UTILITY FUNCTIONS FOR BAMA ASSAY DATA PROCESSING #
# --------------------------------------------------#

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
BAMAresponseCall <- function(dat, def = "delta3", sermuc = "ser") {
  if (length(def) == 1) {
    def <- rep(def, nrow(dat))
  }
  rc <- as.integer(rep(NA, nrow(dat)))
  fold <- as.numeric(substr(def, 6, 10))
  deltaInx <- which(substr(tolower(def), 1, 5) == "delta")
  ratioInx <- which(substr(tolower(def), 1, 5) == "ratio")
  if (tolower(sermuc) == "ser") {
    cond1 <- dat$delta >= dat$pos_threshold
    cond2 <- dat$delta > pmax(0, fold * dat$delta_baseline)
    cond3 <- dat$fi_bkgd > pmax(0, fold * dat$fi_bkgd_baseline)
    rc[deltaInx] <-
      ifelse(apply(cbind(cond1, cond2, cond3)[deltaInx, ], 1, function(x)
        sum(is.na(x))) > 0,
        NA,
        as.integer(cond1 & cond2 & cond3)[deltaInx])
    cond1 <- dat$fi_bkgd >= dat$pos_threshold
    cond2 <- dat$fi_bkgd / dat$fi_bkgd_blank > fold
    cond3 <- dat$fi_bkgd > 3 * dat$fi_bkgd_baseline
    rc[ratioInx] <-
      ifelse(apply(cbind(cond1, cond2, cond3)[ratioInx, ], 1, function(x)
        sum(is.na(x))) > 0,
        NA,
        as.integer(cond1 & cond2 & cond3)[ratioInx])
  } else if (tolower(sermuc) == "muc") {
    cond1 <- dat$delta >= 100
    cond2 <- dat$spec_actvy > fold * dat$spec_actvy_baseline
    rc <-
      ifelse(apply(cbind(cond1, cond2), 1, function(x)
        sum(is.na(x))) > 0,
        NA,
        as.integer(cond1 & cond2))
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
GenerateAUC <- function(x, y) {
  m <- length(x)
  #average y
  y1 <- y[1:(m - 1)]
  y2 <- y[2:m]
  y_m <- (y1 + y2) / 2
  #set negative values to 0
  y_m <- ifelse(y_m < 0, 0, y_m)
  #calculate x distance
  x1 <- x[1:(m - 1)]
  x2 <- x[2:m]
  x_w <- x2 - x1
  #each trapezoid area
  tArea <- x_w * y_m
  return(sum(tArea))
}


# FUNCTION: addQCind
# CONTEXT: BAMA
# determine observations that should get flagged for filtering (missing baseline, missing FI, missing visit, etc.)
addQCind <- function(dat, bl) {
  # keep only needed columns to save memory/time
  N <- nrow(dat)

  f1 <- f2 <- f3 <- f4 <- f5 <- rep(NA, N)

  # identify samples with missing FI
  missingfi <-
    which(is.na(dat$delta) |
            is.na(dat$fi_bkgd) | is.na(dat$fi_bkgd_blank))
  f1[missingfi] <- "Missing MFI"

  # missing baseline
  missingbl <-
    which(
      is.na(dat$delta_baseline) |
        is.na(dat$fi_bkgd_baseline) | is.na(dat$fi_bkgd_blank_baseline)
    )
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
  filter_reason <-
    apply(cbind(f1, f2, f3, f4), 1, function(x) {
      paste0(x[!is.na(x)], collapse = ", ")
    })
  filter_reason[filter_reason == ""] <- "Not Filtered"
  return(filter_reason)
}

# FUNCTION: writePerm()
# CONTEXT: BAMA
# write rda file for data package, or write qc csv file for Tomaras lab (depending on qc argument)
writePerm <- function(df,
                      suffix,
                      excl = NULL,
                      qc = FALSE,
                      env) {
  qckeepVars <-
    c(
      "notebook",
      "instrument_type",
      "filename",
      "testdt",
      "detection",
      "isotype",
      "spectype",
      "spec_primary",
      "ptid",
      "visitno",
      "antigen",
      "antigen_lot",
      "beadsetno",
      "dilution",
      "pctcv",
      "fi",
      "fi_bkgd",
      "blank",
      "fi_bkgd_blank",
      "fi_bkgd_baseline",
      "delta",
      "pos_threshold",
      "response",
      "filter_reason",
      "tab",
      "spec_actvy",
      "auc_atlas"
    )
  # NOTE: group should never be in here, for lab blinding.
  if (!is.null(excl)) {
    qckeepVars <- setdiff(qckeepVars, excl)
  }

  outname <- paste(pkgName, suffix, sep = "_")
  if (qc == TRUE) {
    write.csv(df[, qckeepVars],
              file.path(
                DataPackageR::project_data_path(),
                paste0("QC_", outname, ".csv")
              ),
              row.names = FALSE)
    message("Writing ", paste0("QC_", outname, ".csv"))
  } else {
    return(df[, qckeepVars])
  }
}
