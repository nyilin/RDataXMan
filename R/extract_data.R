#' Extract data by users' requirements.
#' @description This function extracts data based on the request forms users
#'   have filled and saved in the \code{request_output} folder of selected
#'   research folder.
#' @param wkdir The path to working directory. See \code{\link{initWkdir}} for
#'   details on a working directory.
#' @param research.folder The name of research folder. See
#'   \code{\link{initResearchFolder}} for details on a research folder.
#' @param inclusion.xls.file Name(s) of request form(s) with inclusion
#'   criterion. Multiple request forms should be specified as a character
#'   vector.
#' @param variable.xls.file Name(s) of request form(s) with variable lists.
#'   Multiple request forms should be specified as a character vector. Default
#'   is \code{NA}, where no variable list is specified, and variables in the
#'   inclusion criteria will be extracted instead.
#' @param dataLogic Whether to take \code{union} or \code{intersection} of
#'   inclusion criteria, if multiple criteria specified. Default is \code{NA}.
#' @param select.output \code{1} to generate lists of identifier variables from
#'   merged inclusion criteria; \code{2} to generate an Excel file with summary
#'   statistics for both inclusion criteria and variable lists; \code{3} to
#'   generate \code{csv} files with data extracted based on each request form;
#'   \code{4} to generate a single \code{csv} file for the final merged data.
#'   Multiple selection should be specified as a vector.
#' @param overwrite Whether to overwrite existing request form. Default is
#'   \code{TRUE}.
#' @param database Name of database. Should be either \code{private} or
#'   \code{public} for flat tables, indicating whether data is stored in
#'   \code{public_data} or \code{research/[research folder]/private_data}, or
#'   the actual name of database if extracting data from a database.
#' @param username User name for accessing database if \code{data.type} is not
#'   \code{flat}. Default is \code{NA} for flat tables.
#' @param password Password for accessing database if \code{data.type} is not
#'   \code{flat}. Default is \code{NA} for flat tables.
#' @param conn_string Connection string for accessing ORE server. Default is
#'   \code{NA}.
#' @import xlsx
#' @import readxl
#' @import RMySQL
#' @import haven
#' @import data.table
#' @import stringr
#' @examples
#' \dontrun{
#' extract_data(wkdir = "Working directory", research.folder = "requestnum001",
#'              inclusion.xls.file = "inclusion.Diagnosis_DIAGNOSIS_CD(DIAGNOSIS_DESC_ICD_VERSION)",
#'              variable.xls.file = "variable.Patient(PATIENT_NRIC)",
#'              select.output = c(1, 2, 4))
#' }
#' @return Returns a list of identifier variables, path to the Excel file with
#'   summary statistics, extracted data and merged data, if any of these are
#'   selected with \code{selected.output}. These are also written as \code{csv}
#'   files in \code{research/[research folder]/request_output} folder.The
#'   \code{summary.xls} is returned. The \code{summary.xls} will includes count
#'   summary sheet and variable summary sheet.
#' @seealso \code{\link{genInclusion}}, \code{\link{genVariable}}
#' @export
extract_data <- function(wkdir = getwd(), research.folder = NA,
                         inclusion.xls.file = NA, variable.xls.file = NA,
                         database = NA, dataLogic = NA, select.output = NA,
                         overwrite = TRUE, username = NA, password = NA,
                         conn_string = NA) {
  ## change "" input to NA.
  research.folder <- check_input(research.folder)
  inclusion.xls.file <- check_input(inclusion.xls.file)
  variable.xls.file <- check_input(variable.xls.file)
  database <- check_input(database)
  dataLogic <- check_input(dataLogic)
  select.output <- check_input(select.output)
  username <- check_input(username)
  password <- check_input(password)
  conn_string <- check_input(conn_string)
  ##== check that the arguments are correctly specified:
  research.folder <- research.folder[1]
  if (is.na(research.folder)) {
    stop(simpleError(
      "Please specify a single research folder for this extraction."
    ))
  }

  inclusion.xls.file <- unique(na.omit(as.character(inclusion.xls.file)))
  if (length(inclusion.xls.file) == 0) {
    stop(simpleError("Please specify inclusion criteria for this extraction."))
  }

  # Allow `NA`: in this case only extract data based on inclusion and count
  # inclusion
  variable.xls.file <- unique(na.omit(as.character(variable.xls.file)))

  if ((length(inclusion.xls.file) > 1) | (length(variable.xls.file) > 1)) {
    if (is.na(dataLogic)) {
      tmp.multi.file <- read_excel(
        path = file.path("research", research.folder,
                         "request_input/multiple_inclusion_specification.xls"),
        sheet = "specification"
      )
      tmp.multi <- tmp.multi.file[which(tmp.multi.file[, "Argument"] ==
                                          "data.logic"),
                                  "Specification"]
      if (is.na(tmp.multi)) {
        stop(simpleError("`dataLogic` is required when more than one inclusion criteria and/or variable list are specified."))
      }
      dataLogic <- tmp.multi
    } else {
      dataLogic <- tolower(dataLogic)
      if (!dataLogic %in% c("union", "intersection")) {
        stop(simpleError("`dataLogic` must be either `union` or `intersection`."))
      }
    }
  }

  if (!all(select.output %in% 1:4)) {
    stop(simpleError("`select.output` can only take values 1, 2, 3, and/or 4."))
  }

  if (!overwrite %in% c(TRUE, FALSE)) {
    stop(simpleError("Please specify either `TRUE` or `FALSE` for `overwrite`."))
  }

  # Set working direct and check subfolders
  setwd(wkdir)
  if (!dir.exists("research")) {
    stop(simpleError("Please create `research` folder in the working directory specified, and create a subfolder for this extraction within it."))
  }
  if (!dir.exists(file.path("research", research.folder))) {
    stop(paste("Please create a subfolder for this extraction in `research` folder."))
  }
  if (!dir.exists(file.path("research", research.folder, "request_input"))) {
    stop(simpleError(paste("Please create a `request_input` folder within the research folder, and copy annotated inclusion criteria and variable lists to this folder.")))
  }
  if (!dir.exists(file.path("research", research.folder, "request_output"))) {
    dir.create(file.path("research", research.folder, "request_output"))
  }
  # Extract data based on inclusion and variable lists
  inclu_list <- process_inclu(research.folder = research.folder,
                              inclusion.xls.file = inclusion.xls.file,
                              dataLogic = dataLogic, overwrite = overwrite,
                              username = username, password = password)
  if (length(variable.xls.file) > 0) {
    var_info <- process_var(research.folder = research.folder,
                            variable.xls.file = variable.xls.file,
                            inclu_list = inclu_list,
                            overwrite = overwrite,
                            username = username, password = password)
  } else {
    var_info <- NULL
  }
  # Extract output
  output <- list(id.var = NULL, summary.stat = NULL, raw.extract.dat = NULL,
                 merge.extract.dat = NULL)
  if ("1" %in% select.output) {
    # Write unique identifiers by inclusion
    # output$id.var <- write_id(id_merged = inclu_list$id_merged,
    #                           type = "inclusion",
    #                           research.folder = research.folder)
    output$id.var <- write_id(
      id_merged = inclu_list$data_merged[, names(inclu_list$id_merged),
                                         with = FALSE],
      type = "inclusion", research.folder = research.folder
    )
  }
  if ("3" %in% select.output) {
    # If only 1 inclusion criteria, and extraction-option==TRUE, we remove extraction for inclusion as the information is exactly
    # the same as merge_inclusioin
    if("4" %in% select.output && length(inclusion.xls.file)==1){
      message(simpleMessage(
        paste("Only 1 inclusion criteria:", paste(inclusion.xls.file),
              "be selected.\n",
              "Extraction for inclusion is the same as merge_inclusion. \n")
        ))
    }else{
      # Write raw data by inclusion and variable list
      output$raw.extract.dat$inc <- write_data_raw(
        data_list = inclu_list$inclu_list,
        type = "inclusion",
        research.folder = research.folder
      )
    }


    if (!is.null(var_info)) {
      # If only 1 variable list, and extraction-option==TRUE, we remove extraction for variable as the information is exactly
      # the same as merge_dat
      if("4" %in% select.output && length(variable.xls.file)==1){
        message(simpleMessage(
          paste("Only 1 variable list:", paste(variable.xls.file),
                " be selected.\n",
                "Extraction for variable is the same as merge_dat. \n")
        ))
      }else{
        output$raw.extract.dat$var <- write_data_raw(
          data_list = var_info$var_list$var_list,
          type = "variable",
          research.folder = research.folder
        )
      }
    }
  }
  if ("4" %in% select.output) {
    # Write merged data based on dataLogic
    output$merge.extract.dat$inc <- inclu_list$data_merged
    write.csv(inclu_list$data_merged,
              file = paste0("research/", research.folder,
                            "/request_output/merge_inclusion.csv"),
              row.names = FALSE, na="")
    if (!is.null(var_info) & nrow(var_info$var_list$data_merged) > 0) {
      output$merge.extract.dat$var <- var_info$var_list$data_merged
      write.csv(var_info$var_list$data_merged,
                file = paste0("research/", research.folder,
                              "/request_output/merge_dat.csv",na=""),
                row.names = FALSE)
    }
  }
  if ("2" %in% select.output) {
    # Count number of unique identifiers and key variable by identifiers
    c_inclu <- try(count_inclu(inclu_list), silent = TRUE)
    if (inherits(c_inclu, "try-error")) {
      warning(simpleWarning(
        "The content you want to write into excel is too big."
      ))
    }
    if (all(is.na(variable.xls.file))) {
      # file <- paste0(
      #   "research/", research.folder, "/request_output/",
      #   paste(unlist(lapply(inclu_list$inclu_list, function(l) l$table_name)),
      #         collapse = ","),
      #   "_summary.xlsx"
      # )
      file <- paste0("research/", research.folder, "/request_output/summary_list.xlsx")
      setting <- data.frame(
        Argument = c("inclusion criterion", "variable list", "wkdir",
                     "research.folder"),
        Specification = c(paste(inclusion.xls.file, collapse = ","),
                          "empty", wkdir, research.folder),
        stringsAsFactors = FALSE
      )
    } else {
      # file <- paste0(
      #   "research/",research.folder,"/request_output/",
      #   paste(unlist(lapply(inclu_list$inclu_list, function(l) l$table_name)),
      #         collapse = ","),
      #   "_with variable list_summary.xlsx"
      # )
      file <- paste0(
        "research/",research.folder,"/request_output/summary_list.xlsx"
      )
      setting <- data.frame(
        Argument = c("inclusion criterion", "variable list", "dataLogic",
                     "wkdir", "research.folder"),
        Specification = c(paste(inclusion.xls.file, collapse = ","),
                          paste(variable.xls.file,collapse = ","),
                          dataLogic, wkdir, research.folder),
        stringsAsFactors = FALSE
      )
    }
    write_to_file(table = setting, sheetName = "argument list", file = file,
                  overwrite = overwrite)
    xlsx::write.xlsx(c_inclu$count_all, file = file,
                     sheetName = "inclusion_count_overall",
                     row.names = FALSE, append = TRUE, showNA = FALSE)
    if (!is.null(c_inclu$count_key)) {
      lapply(1:length(c_inclu$count_key), function(i) {
        # Do not need to check overwrite now
        write_to_file(table = c_inclu$count_key[[i]],
                      file = file, sheetName = paste0("inclusion_count_", i))
      })
    }
    if (!is.null(var_info) & !is.null(var_info$var_summ)) {
      xlsx::write.xlsx(var_info$var_summ, file = file,
                       sheetName = "variable_summary",
                       row.names = FALSE, append = TRUE, showNA = FALSE)
    }else{
      warning(simpleWarning("No data extracted with inclusion criteria specified.\n"))
    }
    message(simpleMessage(
      paste("Summary statistics of", paste(inclusion.xls.file, collapse = ","),
            "saved into", file, "\n")
    ))
    output$summary.stat <- file
  }
  output
}
