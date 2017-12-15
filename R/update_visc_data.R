#' Copy data files from latest version of VISCfunctions.data on github.fhcrc.org to user's local VISCfunctions repo
#'
#' Note that the working directory when the function is called must be inside your local VISCfunctions repo.
#'
#' @param package The name of the package. By default, VISCfunctions.data, but this function will technically
#' copy the data files from any package on VIDD-VISC github, should the need arise. For now, disabled elsewhere.
#' @param branch Supply if data to copy is on a branch other than 'master'
#' @param sha Supply if data version desired is tied to a particular commit of the source package (VISCfunctions)
#' @export
update_visc_data <- function(package = "VISCfunctions.data", branch = NULL, sha = NULL){

  # args for git2r::clone
  url <- paste0("https://github.fhcrc.org/VIDD-VISC/", package)
  cred <- git2r::cred_token() # this can be fleshed out later to include ssh with git2r::cred_ssh_key

  # create a temp directory for cloning
  local <- file.path(tempfile(pattern="upvida-"), package)
  dir.create(local, recursive=TRUE)

  # clone
  repo <- git2r::clone(url = url, local_path = local, branch = branch, credentials = cred, progress = FALSE)

  # validate sha, and retrieve full sha is partial is provided
  if(!is.null(sha)){
    shaV <- .validate_sha(repo, sha)
    git2r::checkout(git2r::revparse_single(repo, shaV)) # checkout sha if provided
  }

  # copy data
  sep <- .Platform$file.sep
  WDV <- .validate_wd()
  if(WDV != FALSE){
    dataFromPath <- file.path(local, "data")
    dataToPath <- WDV
    filesToCopy <- list.files(dataFromPath, pattern = ".[rR][dD][aA]$")
    if(length(filesToCopy)==0)
      message("No .rda files to copy!")
    for(f in filesToCopy){
      result <- file.copy(from = file.path(local, "data", f), to = dataToPath, overwrite = TRUE, copy.mode = TRUE, recursive = FALSE)
      message("Copying ", package, sep, "data", sep, f, " to ", dataToPath, sep, "... ", ifelse(result=="TRUE", "Success.", "Failure."))
    }
  }
  return(invisible())
}



# utility function to ensure working directory is within a git repo called VISCfunctions
.validate_wd <- function(wd = getwd()){

  sep <- .Platform$file.sep
  curr_repo <- gsub(paste0("[", sep, "]$"), "",
                    gsub("/", sep, git2r::discover_repository(wd), fixed = TRUE))
  if(length(curr_repo)==0){stop("Working directory must be inside a git repo when calling update_visc_data.")}

  repo_main <- gsub(paste0(sep, ".git"), "", file.path(curr_repo))
  tryCatch(this_package <- devtools::as.package(repo_main),
           error = function(e){stop("The current repo does not appear to be an R package.")})

  data_dir <- file.path(repo_main, "data")
  if(!dir.exists(data_dir))
    dir.create(data_dir)

  if(grepl("VISCfunctions/.git$", curr_repo)){ # for now, restrict this function to VISCfunctions
    return(data_dir)
  } else {
    stop("At this time, update_visc_data can only be used to copy data to the VISCfunctions package.")
  }

}
