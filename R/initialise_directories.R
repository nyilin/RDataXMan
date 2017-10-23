#' Initialise working directory
#'
#' @param wkdir The working directory of all subfolders, e.g., \code{Working
#'   directory}.
#' @details This function initialises the working directory by automatically
#'   creating the \code{public_data}, \code{public_template} and \code{research}
#'   subfolders for users, which can also be manually created by user as long as
#'   the folder names are followed exactly.
#'
#'   After the working directory is initialised, user should make sure that all
#'   public non-database data files are within the \code{public_data} subfolder.
#' @export
initWkdir <- function(wkdir) {
  dt <- file.path(wkdir, "public_data")
  wrn <- "If your data is stored as flat tables, please move your data into the 'public_data' folder within working directory.\n"
  if (!dir.exists(wkdir)) {
    dir.create(dt, recursive = TRUE)
    warning(simpleWarning(wrn))
  } else {
    if (!dir.exists(dt)) {
      dir.create(dt)
      warning(simpleWarning(wrn))
    } else if (length(dir(dt)) == 0) {
      warning(simpleWarning(wrn))
    }
  }
  tmpl <- file.path(wkdir, "public_data_template")
  rsrch <- file.path(wkdir, "research")
  if (!dir.exists(tmpl)) {
    dir.create(tmpl, recursive = TRUE)
  }
  if (!dir.exists(rsrch)) {
    dir.create(rsrch, recursive = TRUE)
  }
  message(simpleMessage(paste("Working directory", wkdir, "has been initialised.\n")))
}
#' Initialise research folder
#'
#' @param wkdir The working directory of all subfolders, e.g., \code{Working
#'   directory}.
#' @param research.folder Name of the research folder.
#' @details This function initialises the research folder by automatically
#'   creating the research folder specified within the \code{research} subfolder
#'   of the working directory, and then creating the \code{private_data},
#'   \code{private_data_template}, \code{request_input} and
#'   \code{request_output} in the research folder. This process can also be done
#'   manually, as long as the folder structure and folder names are followed
#'   exactly.
#'
#'   After the research folder is initialised, user should make sure that all
#'   private non-database data files are within the \code{private_data}
#'   subfolder. Relevant inclusion files and variables lists should be saved in
#'   \code{request_input} subfolder within the research folder for data
#'   extraction.
#' @export
initResearchFolder <- function(wkdir, research.folder) {
  rsrch_fldr <- file.path(wkdir, "research")
  if (!dir.exists(rsrch_fldr)) {
    dir.create(rsrch_fldr)
  }
  fldr <- file.path(rsrch_fldr, research.folder)
  dir.create(file.path(fldr, "private_data"), recursive = TRUE)
  dir.create(file.path(fldr, "private_data_template"), recursive = TRUE)
  dir.create(file.path(fldr, "request_input"), recursive = TRUE)
  dir.create(file.path(fldr, "request_output"), recursive = TRUE)
  message(simpleMessage(paste("Research folder", research.folder, "has been created in folder 'research' within the working directory. Please copy your private data to 'private_data' folder.\n")))
}
