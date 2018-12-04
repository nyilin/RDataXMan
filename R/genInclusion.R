#' Generate inclusion criterion
#' @description This function generates a request form containing a list of all
#'   unique values of a particular key variable in a dataset selected, based on
#'   which users can specify their inclusion criteria for data extraction.
#' @param wkdir The path to working directory. See \code{\link{initWkdir}} for
#'   details on a working directory.
#' @param research.folder The name of research folder. See
#'   \code{\link{initResearchFolder}} for details on a research folder.
#' @param table_name The name of dataset. File extension must be included if the
#'   data is saved as a flat file instead of in a database, e.g. `data.csv`
#'   should be given instead of simply `data`.
#' @param key.var The name of variable based on which inclusion criterion is
#'   specified. Vector with multiple elements is not allowed. To have complex
#'   inclusion criteria based on multiple variables, generate one request form
#'   for each variable.
#' @param key.desc The name of variable(s) that describes \code{key.var}, if
#'   any. Multiple description variables should be specified as a character
#'   vector. Default is \code{NA}, where no description variable is selected.
#' @param identifier.var The name(s) of identifier variable(s) of the inclusion
#'   criterion. Multiple identifier variables should be specified as a character
#'   vector.
#' @param count Whether a count summary should be generated for each of
#'   \code{identifier.var}, as a logical vector with the same length as
#'   \code{identifier.var}. Default value is
#'   \code{rep(TRUE, length(identifier.var))}.
#' @param data.type \code{flat} for flat tables, \code{sql} fo rSQL database,
#'   \code{ore} for Oracle R Enterprise (ORE) database, and \code{oracle} for
#'   Oracle in SQL mode.
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
#' @param conn_string Connection string for accessing ORE server. Default is
#'   \code{NA}.
#' @examples
#' \dontrun{
#' # Generate the inclusion criterion from public table `movement_table.txt`
#' # based on variable `DIAGNOSIS_CD`. `DIAGNOSIS_DESC` and `ICD_VERSION`
#' # contains detailed description for the key variable.
#' #
#' # Unique entries are identified by `PATIENT_NRIC` and `CASE_NO`.
#' # Unique `DIAGNOSIS_CD` is counted by `PATIENT_NRIC` and `CASE_NO`.
#' #
#' genInclusion(wkdir = "Working directory", research.folder = "requestnum001",
#'              table_name = `movement_table.txt`, data.type = "flat",
#'              database = "public",
#'              key.var = "DIAGNOSIS_CD",
#'              key.desc = c("DIAGNOSIS_DESC", "ICD_VERSION"),
#'              identifier.var = c("PATIENT_NRIC", "CASE_NO"))
#' }
#' @return Returns the name of inclusion criteria generated. See \code{details}.
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
#' @seealso \code{\link{genVariable}}, \code{\link{extract_data}}
#' @export
genInclusion <- function(wkdir = getwd(), research.folder = NA,
                         table_name = NA, data.type = NA, database = NA,
                         key.var = NA, key.desc = NA, identifier.var = NA,
                         count = NA, overwrite = TRUE,
                         username = NA, password = NA, conn_string = NA) {
  # If any of the input is not NA or legal value, set to NA. Only considers the
  # first entry if multiple entries is specified for a single input (this could
  # happen when using Rcmdr)
  research.folder <- check_input(research.folder)
  table_name <- check_input(table_name)
  data.type <- check_input(data.type)
  database <- check_input(database)
  key.var <- check_input(key.var)
  key.desc <- check_input(key.desc)
  identifier.var <- check_input(identifier.var)
  count <- check_input(count)
  username <- check_input(username)
  password <- check_input(password)
  conn_string <- check_input(conn_string)
  ##== 1 check that the arguments are correctly specified:
  research.folder <- research.folder[1]
  if (is.na(research.folder)) {
    stop(simpleError(
      "Please specify a single research folder for this extraction."
    ))
  }

  if (length(table_name) > 1) {
    table_name <- table_name[1]
    warning(simpleWarning("Only the first entry in `table_name` is used."))
  }
  if (is.na(table_name)) {
    stop(simpleError("`table_name` is required and should be as a string."))
  }

  if (length(key.var) > 1) {
    key.var <- key.var[1]
    warning(simpleWarning("Only the first entry in `key.var` is used."))
  }
  if (is.na(key.var)) {
    stop(simpleError("`key.var` is required and should be a string."))
  }

  if (all(is.na(key.desc))) {
    key.desc <- NA
  }

  if (all(is.na(identifier.var))) {
    stop(simpleError("`identifier.var` is required and should be a string or a character vector."))
  }

  if (all(is.na(count))) {
    cat("`count` was not specified or specified wrongly. Treated as TRUE for all `identifier.var` by default.\n")
    count <- rep(TRUE, length(identifier.var))
  } else if (length(na.omit(count)) == 1) {
    count <- rep(na.omit(count), length(identifier.var))
  } else {
    stop(simpleError("`count` should either be a single `TRUE` or `FALSE`, or a vector of `TRUE` and `FALSE` with the same length as `identifier.var`."))
  }

  # Only allow `ore`, `oracle`, `sql` or `flat` for `data.type`
  data.type <- tolower(data.type[1])
  if (is.na(data.type) | is.null(data.type)) {
    stop(simpleError(
      "`data.type` is required. Please specify `flat` for flat tables."
    ))
  }
  data.type <- match.arg(data.type, c("ore", "sql", "oracle", "flat"))
  if (data.type == "flat") {
    # Input is flat table. Interpret data.type from file name
    file_vec <- unlist(strsplit(table_name, split = "\\."))
    if (length(file_vec) == 1) {
      stop(simpleError("Please include file extension in `table_name`."))
    } else {
      data.type <- tolower(file_vec[length(file_vec)])
    }
    # rename database when data.type equal to "flat". i.e., "public" rename as "public_data"; "private" rename as "private_data"
    database <- database_full(research.folder, database,data.type)
    # Create template folder name based on the database when data.type=="flat"
    template_folder <- paste(database[2],"_template",sep = "")
    # if(stringr::str_extract(database,"private")%in%"private"){
    #   template_folder <- file.path("research", research.folder, template_folder)
    # }
  }else{
    # data.type is ORE, Oracle and SQL, the template_folder is "public_data_template".
    template_folder <- "public_data_template"
  }

  if (!overwrite %in% c(TRUE, FALSE)) {
    stop(simpleError("Please specify either `TRUE` or `FALSE` for `overwrite`."))
  }
  ###==Set working direct and generate the subfolders:
  setwd(wkdir)
  if (!dir.exists("research")) {
    dir.create("research")
  }
  if (!dir.exists(database[1]) & !data.type %in% c("ore", "oracle", "sql")) {
    stop(simpleError("Please create folder `data` in your working directory and move all data stored in flat tables to this folder."))
  }

  if (!dir.exists(template_folder)) {
    dir.create(template_folder)
  }

  # Load data
  dat0 <- access_bridge(data.type = data.type, conn_string = conn_string,
                        database = database[1], table_name = table_name,
                        username = username, password = password,
                        type = "inclusion")
  dat <- dat0$dat
  conn_string <- dat0$conn_string
  database <- dat0$database
  # Check whether identifier varible is the varaible in inc.dat
  if (!all(identifier.var %in% colnames(dat))) {
    warning(simpleWarning(
      "`identifier.var` should be column name in the data table."
    ))
  }
  ##== 3 generate the overall list in inclusion criterion.
  ###== 3.1 get all the inclusion criterion from the database.
  inc_all <- unique(dat[, na.omit(c(key.var,key.desc)), with = FALSE])
  inc_all <- inc_all[order(inc_all[, key.var, with = FALSE]), ]
  overall_list <- data.frame(sno = c(1:nrow(inc_all)),
                             inc_all,
                             remarks = rep(NA, nrow(inc_all)),
                             selection = rep(NA, nrow(inc_all)),
                             logic = rep(NA, nrow(inc_all)),
                             check.names = TRUE, stringsAsFactors = FALSE)
  time <- Sys.time()
  time <- format(time,usetz = FALSE)
  time <- gsub(" ", "_", time)
  time <- gsub(":","",time)
  time <- gsub("-","",time)
  if (all(is.na(key.desc))) {
    if (data.type %in% c("ore", "oracle", "sql")) {
      file <- paste0(template_folder, "/inclusion.", table_name, "_", key.var,
                     "_", data.type, "_", username, "_", time, ".xls")
    } else {
      if (template_folder == "public_data_template") {
        file <- paste0(template_folder, "/inclusion.", table_name, "_", key.var,
                       "_", data.type, "_", time, ".xls")
      } else if (basename(template_folder) == "private_data_template") {
        file <- paste0(template_folder, "/inclusion.", table_name, "_", key.var,
                       "_", data.type, ".xls")
      }
    }
  } else {
    if (data.type %in% c("ore", "oracle", "sql")) {
      file <- paste0(template_folder, "/inclusion.", table_name, "_",
                     key.var, "_", "(", paste(key.desc, collapse = "_"), ")_",
                     data.type, "_", username, "_", time, ".xls")
    } else {
      if (template_folder == "public_data_template") {
        file <- paste0(template_folder, "/inclusion.", table_name, "_", key.var,
                       "(", paste(key.desc, collapse = "_"), ")_",
                       data.type, "_", time, ".xls")
      } else if (basename(template_folder) == "private_data_template") {
        file <- paste0(template_folder,"/inclusion.", table_name, "_", key.var,
                       "(", paste(key.desc, collapse = "_"), ")_",
                       data.type, ".xls")
      }
    }
  }
  if (file.exists(file)) {
    msg <- paste("The file:'", file, "' already exists")
    if (overwrite) {
      msg <- paste(msg, "and will be overwritten\n")
      message(simpleMessage(msg))
      file.remove(file)
    } else {
      stop(simpleError(paste0(msg, ".\n")))
    }
  }
  xlsx::write.xlsx(overall_list, file = file, sheetName = "overall list",
                   row.names = FALSE, showNA = FALSE)
  message(simpleMessage(paste("  ** Request form generated:\n  ", file, "\n")))
  ###== 3.2 count the number of identifier.var for each key.var

  if (count[1]) {
    message(simpleMessage(
      paste("  ** Counting", key.var, "by", identifier.var[1], "...")
    ))
    v <- na.omit(c(key.var, key.desc))
    inc_unique <- unique(dat[, c(v, identifier.var[1]), with = FALSE])
    inc_unique <- cbind(inc_unique[, with = FALSE], n = 1)
    nc <- ncol(inc_unique)
    inc_count <- aggregate(
      as.formula(paste("n ~", paste(v, collapse = " + "))),
      data = inc_unique, FUN = sum
    )
    colnames(inc_count)[ncol(inc_count)] <- paste("count.", identifier.var[1])
    inc_count <- inc_count[order(inc_count[,key.var]),]
  }
  if (length(identifier.var) > 1) {
    sapply(seq_along(identifier.var)[-1], function(i) {
      if (count[i]) {
        message(simpleMessage(
          paste("  ** Counting", key.var, "by", identifier.var[i], "...")
        ))
        v <- na.omit(c(key.var, key.desc))
        inc_unique <- unique(dat[, c(v, identifier.var[i]), with = FALSE])
        inc_unique <- cbind(inc_unique[, with = FALSE], n = 1)
        nc <- ncol(inc_unique)
        inc_count_i <- aggregate(
          as.formula(paste("n ~", paste(v, collapse = " + "))),
          data = inc_unique, FUN = sum
        )
        colnames(inc_count_i)[ncol(inc_count_i)] <- paste("count.", identifier.var[i])
      }
      inc_count <- merge(inc_count,inc_count_i, by = v, all = TRUE)
      inc_count <- inc_count[order(inc_count[,key.var]),]
      xlsx::write.xlsx(inc_count, file = file,
                       sheetName = "count_by_identifier.var",
                       append = TRUE, row.names = FALSE, showNA = FALSE)
    })
  } else {
    xlsx::write.xlsx(inc_count, file = file,
                     sheetName = "count_by_identifier.var",
                     append = TRUE, row.names = FALSE, showNA = FALSE)
  }
  message(simpleMessage("done\n"))
  if (all(is.na(key.desc))) {
    key.desc <- NA
  } else {
    key.desc <- paste(key.desc, collapse = ",")
  }
  setting_list <- data.frame(
    Argument = c("wkdir", "table_name", "key.var", "key.desc", "identifier.var",
                 "data.type", "conn_string", "database",
                 "username.req", "password.req"),
    Specification = c(wkdir, table_name,
                      key.var, key.desc,
                      paste(identifier.var, collapse = ","),
                      data.type, conn_string, database,
                      username.req = data.type %in% c("ore", "oracle", "sql"),
                      password.req = data.type %in% c("ore", "oracle", "sql"))
  )
  xlsx::write.xlsx(setting_list, file = file, sheetName = "setting list",
                   append = TRUE, row.names = FALSE, showNA = FALSE)
  message(simpleMessage("****Request completed\n\n"))
  file
}
