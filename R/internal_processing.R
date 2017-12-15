# utility function to validate the provided SHA
.validate_sha <- function(repo, sha){
  hashes <- sapply(git2r::commits(repo), function(x) x@sha)
  if(any(grep(sha, hashes))){
    sha <- hashes[which(grepl(paste0("^", sha), hashes))]
  } else {
    stop("The provided commit SHA does not appear in the commit history. Please verify it and try again.")
  }
  if(length(sha) > 1){stop("The provided commit SHA is not unique. Please use a longer partial SHA.")}
  return(sha)
}

