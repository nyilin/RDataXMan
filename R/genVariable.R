#' Generate variable list.
#' @description This function generates a request form containing a list of
#'   variables available in a particular key variable in a dataset selected,
#'   based on which users can select variables to extract for data extraction.
#' @param wkdir The path to working directory. See \code{\link{initWkdir}} for
#'   details on a working directory.
#' @param research.folder The name of research folder. See
#'   \code{\link{initResearchFolder}} for details on a research folder.
#' @param table_name The name of dataset. File extension must be included if the
#'   data is saved as a flat file instead of in a database, e.g. `data.csv`
#'   should be given instead of simply `data`.
#' @param identifier.var The name(s) of identifier variable(s) of the variable
#'   list. Multiple identifier variables should be specified as a character
#'   vector.
#' @param omit.var The name(s) of variables in the dataset to exclude from the
#'   variable list, if any. \code{identifier.var} are automatically excluded
#'   from the resulting variable list since they will always be in the extracted
#'   data. Multiple omit variables should be specified as a character vector.
#'   Default is \code{NA}.
#' @param data.type \code{flat} for flat tables and \code{sql} fo MySQL database.
#' @param database Name of database. Should be either \code{private} or
#'   \code{public} for flat tables, indicating whether data is stored in
#'   \code{public_data} or \code{research/[research folder]/private_data}, or
#'   the actual name of database if extracting data from a database.
#' @param overwrite Whether to overwrite existing request form. Default is
#'   \code{TRUE}.
#' @param username User name for accessing database if \code{data.type} is not
#'   \code{flat}. Default is \code{NA} for flat tables.
#' @param password Password for accessing database if \code{data.type} is not
#'   \code{flat}. Default is \code{NA} for flat tables.
#' @examples \dontrun{
#' genVariable(wkdir = "Working directory", research.folder = "requestnum001",
#'             table_name = "table_demographic.txt",
#'             data.type = "flat", database = "public",
#'             identifier.var = "PATIENT_NRIC",
#'             omit.var = c("PATIENT_NRIC", "NATIONALITY", "POSTAL_CODE"))
#' }
#' @return Returns the name of variable list generated. See \code{details}.
#' @details Writes an \code{.xls} file with two sheets to
#'   \code{public_data_template} if lining to database, and to either
#'   \code{public_data_template} or \code{private_data_template} depending on
#'   \code{database} for flat tables.
#'
#' The first sheet \code{overall list} contains unique values of key variable
#' with description (if \code{key.desc} is specfied and exists in the table),
#' remarks and selection. This sheet will be used to specify inclusion criteria
#' for data extraction in later steps.
#'
#' The second sheet \code{setting list} contains detailed information about
#' the database.
#' @seealso \code{\link{genInclusion}}, \code{\link{extract_data}}
#' @import xlsx
#' @export
genVariable <- function(wkdir = getwd(), research.folder = NA,
                        table_name = NA, data.type = NA, database = NA,
                        identifier.var = NA, omit.var = NA, overwrite = TRUE,
                        username = NA, password = NA) {
  ##Check that the arguments are correctly specified:
  table_name <- check_input(table_name)
  identifier.var <- check_input(identifier.var)
  omit.var <- check_input(omit.var)
  data.type <- check_input(data.type)
  username <- check_input(username)
  password <- check_input(password)
  database <- check_input(database)
  if (length(table_name) > 1) {
    table_name <- table_name[1]
    warning(simpleWarning("Only the first entry in `table_name` is used."))
  }
  if (is.na(table_name)) {
    stop(simpleError("`table_name` is required and should be as a string."))
  }

  if (all(is.na(identifier.var))) {
    stop(simpleError("`identifier.var` is required and should be a string or a character vector."))
  }

  omit.var <- na.omit(omit.var)

  # data.type <- tolower(data.type[1])
  # if (is.na(data.type)) {
  #   file_vec <- unlist(strsplit(table_name, split = "\\."))
  #   if (length(file_vec) == 1) {
  #     stop(simpleError("Please either specify `data.type`, or include file extension in `table_name` if data is stored in a flat table."))
  #   } else {
  #     data.type <- tolower(file_vec[length(file_vec)])
  #   }
  # }

  # Only allow `sql` or `flat` for `data.type`
  data.type <- tolower(data.type)
  data.type <- match.arg(data.type, c("sql", "flat"))
  if (data.type == "flat") {
    # Input is flat table. Interpret data.type from file name
    file_vec <- unlist(strsplit(table_name, split = "\\."))
    if (length(file_vec) == 1) {
      stop(simpleError("Please include file extension in `table_name`."))
    } else {
      data.type <- tolower(file_vec[length(file_vec)])
    }
    # rename database when data.type equal to "flat". i.e., "public" rename as "public_data"; "private" rename as "private_data"
    database <- database_full(research.folder,database,data.type)
    # Create template folder name based on the database when data.type=="flat"
    template_folder <- paste0(database[2], "_template")
  } else {
    # data.type is ORE, Oracle and SQL, the template_folder is "public_data_template".
    template_folder <- "public_data_template"
  }

  if (!overwrite %in% c(TRUE, FALSE)) {
    stop(simpleError("Please specify either `TRUE` or `FALSE` for `overwrite`."))
  }
  ##==Set working direct and generate the subfolders:
  setwd(wkdir)
  if (!dir.exists(database[1]) & !data.type %in% c("sql")) {
    stop(simpleError("Please create folder `data` in your working directory and move all data stored in flat tables to this folder."))
  }
  if (!dir.exists("research")) {
    dir.create("research")
  }
  if (!dir.exists(template_folder)) {
    dir.create(template_folder)
  }

  identifier.var <- unzip(identifier.var)

  # Load data
  dat0 <- access_bridge(data.type = data.type,
                        database = database[1], table_name = table_name,
                        username = username, password = password,
                        type = "variable")
  # dat <- dat0$dat
  variables <- dat0$dat
  variables <- (t(variables)[1,])
  variables <- variables[!variables %in% identifier.var]
  variables <- variables[!variables %in% omit.var]
  database <- dat0$database
  # variables <- colnames(dat)[!(colnames(dat) %in% omit.var)]
  if (!all(identifier.var %in% t(dat0$dat)[1, ])) {
    warning(simpleWarning(
      "`identifier.var` should be column name in the data table."
    ))
  }
  ##== generate overall list in variable list
  overall_list <- data.frame(sno = 1:length(variables),
                             variable = variables,
                             remarks = rep(NA, length(variables)),
                             selection = rep(NA, length(variables)))
  time <- Sys.time()
  time <- as.character(format(time,usetz = FALSE))
  time <- gsub(" ", "_", time)
  time <- gsub(":","",time)
  time <- gsub("-","",time)
  if (all(is.na(identifier.var))) {
    if (data.type %in% c("ore", "oracle", "sql")) {
      file <- paste0(template_folder,"/variable.", table_name, "_", data.type, "_",
                     username, "_", time, ".xls")
    } else {
      if (template_folder == "public_data_template") {
        file <- paste0(template_folder,"/variable.", table_name, "_", data.type, "_", time, ".xls")
      } else if (basename(template_folder) == "private_data_template") {
        file <- paste0(template_folder,"/variable.", table_name, "_", data.type, ".xls")
      }
    }
  } else {
    if (data.type %in% c("sql")) {
      file <- paste0(template_folder,"/variable.", table_name, "_",
                     "(", paste(identifier.var, collapse = "_"), ")_",
                     data.type, "_",
                     username, "_", time, ".xls")
    } else {
     if (template_folder == "public_data_template") {
        file <- paste0(template_folder,"/variable.", table_name,
                       "(", paste(identifier.var, collapse = "_"), ")_",
                       data.type, "_", time,".xls")
      } else if (basename(template_folder) == "private_data_template") {
        file <- paste0(template_folder,"/variable.", table_name,
                       "(", paste(identifier.var, collapse = "_"), ")_",
                       data.type, ".xls")
      }
    }
  }
  if (file.exists(file)) {
    msg <- paste("The file:'", file, "' already exists")
    if (overwrite) {
      msg <- paste(msg, "and will be overwritten\n")
      file.remove(file)
    } else {
      stop(simpleError(paste0(msg, ".\n")))
    }
  }
  write.xlsx(overall_list, file = file, sheetName = "overall list",
             row.names = FALSE, showNA = FALSE)
  message(simpleMessage(paste("  ** Request form generated:\n  ", file, "\n")))
  ##== generate setting list in variable list.
  setting_list <- data.frame(
    Argument = c("wkdir", "table_name", "identifier.var", "data.type",
                 "database", "username.req", "password.req"),
    Specification = c(wkdir, table_name, paste(identifier.var, collapse = ","),
                      data.type, database,
                      username.req = data.type %in% c("sql"),
                      password.req = data.type %in% c("sql"))
  )
  write.xlsx(setting_list, file = file, sheetName = "setting list",
             append = TRUE, row.names = FALSE, showNA = FALSE)
  message(simpleMessage("****Request completed\n\n"))
  file
}
