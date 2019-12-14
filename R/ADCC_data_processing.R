# FUNCTION: get_titer_fun()
# CONTEXT: ADCC LUC
# compute ADCC Luciferase titer
get_titer_fun <- function(cutoff_in, activity_in, dilution_in) {
  data_here <- data.table(activity_in, dilution_in)

  if (any(data_here$activity_in > cutoff_in, na.rm = TRUE)) {
    #sorting data by dilution (lowest to highest)
    setkey(data_here, dilution_in)
    for (i in 1:(nrow(data_here) - 1)) {
      #Checking if cutoff is between the next two points
      if (data_here$activity_in[i] >= cutoff_in &
          data_here$activity_in[i + 1] < cutoff_in) {
        slope_here <-
          (data_here$activity_in[i] - data_here$activity_in[i + 1]) / (log10(data_here$dilution_in[i]) - log10(data_here$dilution_in[i + 1]))
        int_here <-
          data_here$activity_in[i] - (slope_here * log10(data_here$dilution_in[i]))
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
