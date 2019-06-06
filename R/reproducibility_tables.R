#' Get Full Username from ID
#'
#' For a given ID looks up user name
#'
#' @param id ID to look full name up. If null (default) looks up ID of current user
#'
#' @return First and Last name associated with ID
#'
#' @details
#'
#' If \code{id} null, uses system "USERNAME" variable for Windows and "USER" variable for Linux and MACs. Full Name is found in Windows via the \code{net} command, and via ldap search in Linux and MACs. The ldap search will only work on SCHARPs network at Fred Hutching Cancer Research Center.
#'
#'
#' @examples
#'
#' get_full_name()
#'
#' @export

get_full_name <- function(id = NULL){
  switch(Sys.info()[['sysname']],
         Windows = {
           if (is.null(id)) {id <- Sys.getenv("USERNAME")}
           myargs <- paste("user /domain", id)
           user <- tryCatch({system2(command = "net", args = myargs, stdout = TRUE, stderr = FALSE)},
                            warning = function(w){NULL},
                            error = function(e){NULL})
           user <- gsub("Full Name\ *", "", user[grepl("FULL NAME", toupper(user))])
           if (length(user) > 0) {
             user <- paste(strsplit(gsub(",", "", user), " ")[[1]][c(2, 1)], collapse = " ")
           } else {
             user <- id
           }
         },
         Linux   = {
           if (is.null(id)) {id <- Sys.getenv("USER")}
           myargs <- paste0("-x -h ldapint.pc.scharp.org -b dc=scharp,dc=org uid=", id)
           user <- system2("ldapsearch", args = myargs, stdout = TRUE)
           user <- user[grep("cn:", user)]
           if (length(user) > 0) {
             user <- gsub("[a-z]+: ", "", user)
           } else {
             user <- id
           }
         },
         Darwin  = {
           if (is.null(id)) {id <- Sys.getenv("USER")}
           myargs <- paste0("-x -h ldapint.pc.scharp.org -b dc=scharp,dc=org uid=", id)
           user <- system2("ldapsearch", args = myargs, stdout = TRUE)
           user <- user[grep("cn:", user)]
           if (length(user) > 0) {
             user <- gsub("[a-z]+: ", "", user)
           } else {
             user <- id
           }
         }
  )
  return(user)
}



#' Get Reproducibility Tables
#'
#' Creating tables used at the end of reports, for reproducibility. Most of the information is based off of sessioninfo::session_info()
#'
#'
#' @return list of length two, containing dataframe of Software Session Information and dataframe of Software Package Version Information
#'
#' @details
#'
#' Both tables usually printing with \code{kable()} at the end of a report.
#'
#' If any loaded packages have a \code{DataVersion} field then the Software Package Version Information will contain a \code{data.version} column.
#'
#' Full Name is found in Windows via the \code{net} command, and via ldap search in Linux and MACs. The ldap search will only work on SCHARPs network at Fred Hutching Cancer Research Center. If there is an error attempting to get the Full Name, the system usernam will be displayed instead.
#'
#'
#' @examples
#'
#' my_session_info <- get_session_info()
#'
#' library(dplyr)
#'
#' # Simple HTML Display
#' kableExtra::kable(my_session_info$platform_table, 'html',
#'       caption = "Reproducibility Software Session Information") %>%
#'       kableExtra::kable_styling()
#'
#' kableExtra::kable(my_session_info$packages_table, 'html',
#'       caption = "Reproducibility Software Package Version Information") %>%
#'       kableExtra::kable_styling()
#'
#'
#' # Latex Display
#' kableExtra::kable(my_session_info$platform_table, 'latex', booktabs = TRUE,
#'       linesep = '', caption = "Reproducibility Software Session Information") %>%
#'       kableExtra::kable_styling(font_size = 7)
#'
#' kableExtra::kable(my_session_info$packages_table, 'latex', booktabs = TRUE,
#'       linesep = '', caption = "Reproducibility Software Package Version Information") %>%
#'       kableExtra::kable_styling(font_size = 7)
#'
#' @export


get_session_info <- function(){

  username <- tryCatch(get_full_name(),
                       error = function(c)
                         ifelse(Sys.info()[['sysname']] == 'Windows',
                                Sys.getenv("USERNAME"),
                                Sys.getenv("USER")))

  my_session_info <- sessioninfo::session_info()

  platform <- my_session_info[[1]]
  packages <- my_session_info[[2]]

  # TABLE 1
  my_session_info1 <- data.frame(
    name = names(platform),
    value = matrix(unlist(platform), nrow = length(platform)),
    stringsAsFactors = FALSE)

  my_current_input <- ifelse(is.null(ci <- knitr::current_input()), 'No Input File Detected', ci)
  my_current_input_w_dir <- ifelse(is.null(ci <-  knitr::current_input(dir = TRUE)), 'No Input File Detected', ci)

  file_name <-  data.frame(
    name = 'file name',
    value = my_current_input,
    stringsAsFactors = FALSE)

  # Add user info
  user_info <- data.frame(
    name = 'user',
    value = username,
    stringsAsFactors = FALSE)

  gitremoteorg <- tryCatch(system2("git" ,"remote -v", stdout = TRUE, stderr = FALSE)[1],
                           error = function(c) '', warning = function(c) '')
  gitremote <-  substr(gitremoteorg,
                       regexpr("\t", gitremoteorg) + 1,
                       regexpr(" ", gitremoteorg) - 1)

  if (is.na(gitremote) || gitremote == "" || grepl('fatal', gitremote)) {
    # No Remote Connection, so just give absolute path
    folder_info <- data.frame(
      name = 'location',
      value = ifelse(my_current_input_w_dir != 'No Input File Detected', dirname(my_current_input_w_dir), getwd()),
      stringsAsFactors = FALSE)
    my_session_info1 <- rbind(my_session_info1, folder_info, file_name, user_info)
  } else{
    if (my_current_input_w_dir != 'No Input File Detected') {

      all_git_files <- system2("git" ,"ls-files -co --no-empty-directory --full-name", stdout = TRUE, stderr = FALSE)
      folder_info_in <- dirname(all_git_files[unlist(lapply(all_git_files, function(xx) grepl(xx, my_current_input_w_dir)))])

    } else {
      folder_info_in <- 'No Input File Location Detected'
    }


    # Dropping matching file names that do not match folder path
    folder_info <- data.frame(
      name = 'location',
      value = folder_info_in,
      stringsAsFactors = FALSE)

    url_info <- data.frame(
      name = 'repo',
      value = gitremote,
      stringsAsFactors = FALSE)

    my_session_info1 <- rbind(my_session_info1, url_info, file_name, folder_info, user_info)
  }


  # TABLE 2
  my_session_info2 <- packages[packages$attached,] # Only want attached packages
  my_session_info2 <- data.frame(package = my_session_info2$package,
                                 version = my_session_info2$loadedversion,
                                 # Pulling in Data Version numbers
                                 data.version = purrr::map_chr(my_session_info2$package, utils::packageDescription, fields = 'DataVersion'),
                                 date = my_session_info2$date,
                                 source = my_session_info2$source,
                                 stringsAsFactors = FALSE)
  if (any(!is.na(my_session_info2$data.version)))
    my_session_info2$data.version[is.na(my_session_info2$data.version)] <- '' else
      my_session_info2 <- my_session_info2[, -match('data.version', colnames(my_session_info2))]

  list(platform_table = my_session_info1, packages_table = my_session_info2)
}
