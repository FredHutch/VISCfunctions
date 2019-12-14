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
get_MIMOSA_probs_fun <-
  function(data_in, ref_antigen_in, seed_in = 537526546) {
    tmp <- copy(data_in)
    tmp[, CountNeg := ParentCount - Count]

    #Need to Change ref_antigen_in level so I can run it in the ConstructMIMOSAExpressionSet function
    tmp[Stim == ref_antigen_in, Stim := "ref_antigen_in"]
    E <- ConstructMIMOSAExpressionSet(
      tmp,
      reference = Stim == "ref_antigen_in",
      measure.columns = c("Count", "CountNeg"),
      default.cast.formula = component ~ AnimalID + Stim + Population + Parent,
      .variables = .(AnimalID, Population, Parent),
      featureCols = 1,
      ref.append.replace = "_REF"
    )

    set.seed(seed_in)

    result <- MIMOSA(
      CountNeg + Count ~ AnimalID | Stim,
      data = E,
      method = "mcmc",
      subset = RefTreat %in% 'Treatment',
      ref = RefTreat %in% 'Reference',
      run.parallel = FALSE,
      seed = seed_in
    )

    MIMOSA_response_prob <- getZ(result)[, 'Pr.response']
    MIMOSA_fdr_P <- adjustMimosaFDR(MIMOSA_response_prob)
    MIMOSA_N <- nrow(tmp[Stim != "ref_antigen_in"])

    out <-
      list(
        Results = data.table(
          AnimalID = E@phenoData@data[grep('Treatment', rownames(E@phenoData@data)), 1],
          Stim = E@phenoData@data[grep('Treatment', rownames(E@phenoData@data)), 2],
          Population = E@phenoData@data[grep('Treatment', rownames(E@phenoData@data)), 3],
          Parent = E@phenoData@data[grep('Treatment', rownames(E@phenoData@data)), 4],
          MIMOSA_response_prob,
          MIMOSA_fdr_P,
          MIMOSA_N
        ),
        seed_used = seed_in
      )
    return(out)
  }


# FUNCTION: adjustMimosaFDR()
# CONTEXT: ICS (MIMOSA)
# Description : get fdr q values from MIMOSA response probabilities (i.e. for MIMOSA and MIMOSA2 response probabilities)
adjustMimosaFDR = function(x) {
  dt = data.table(1:length(x), pr.null = 1 - x, pr.r = x)
  ret = dt[order(pr.null, decreasing = F), q := cumsum(pr.null) / .I]
  ret[, q_final := max(q), by = pr.r]
  return(ret$q_final)
}
