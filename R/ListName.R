#' List file name, table name and variable name of specific table
#' @description List all the file names of inclusion criteria or variables list.
#' @inheritParams genVariable
#' @param mode character string to indicate whether list file names of inclusion
#'   criteria file or variable list.
#' @return character vector includes all the file names based on the mode
#'   choice.
#' @export
ListFN <- function(wkdir = getwd(), research.folder = NA, mode = NA) {
  research.folder <- research.folder[1]
  if (is.na(research.folder)) {
    stop(simpleError(
      "Please specify a single research folder."
    ))
  }
  if (!dir.exists(file.path(wkdir,"research",research.folder))) {
    stop(simpleError(
      paste0("Please create folder`research/", research.folder, "` in your working
      directory and move all selected inclusion criteria and variable list stored
      into this folder.")
    ))
  }
  tmp <- list.files(path = file.path(wkdir, "research", research.folder,
                                     "request_input"))
  if (is.na(mode)) {
    stop(simpleError(
      paste("Failed to find list mode, please specify inclusion or variable mode. \n")
    ))
  }
  if (tolower(mode) %in% c("inc", "inclusion", "inclusion criteria")) {
    ListFN <- do.call("c", lapply(seq_along(tmp), function(i) {
      if ("inclusion" %in% unlist(strsplit(tmp[i], ".", fixed = TRUE))) {
        list <- tmp[i]
      }
    }))
  }
  if (tolower(mode) %in% c("var", "variable", "variable list")) {
    ListFN <- do.call("c", lapply(seq_along(tmp), function(i) {
      if ("variable" %in% unlist(strsplit(tmp[i], ".", fixed = TRUE))) {
        list <- tmp[i]
      }
    }))
  }
  return(ListFN)
}

#' Functions to link to MySQL database and extract column name
#' @description Link to MySQL database and extract column names of table in
#'   database.
#' @inheritParams genVariable
#' @import DBI
#' @import RMariaDB
list_TN_mysql <- function(database, username, password) {
  con <- try(dbConnect(MariaDB(), username = username,
                       password = password, dbname = database),
             silent = TRUE)
  if (inherits(con, "try-error")) {
    stop(simpleError(
      "Access to MySQL failed. Please check `username`, `password` and `database` specified.
      OR check whether the Library of RMySQL intalled normally."
    ))
  } else {
    message(simpleMessage(
      "Connected to MySQL database\n"
    ))
  }
  sttm = sprintf("SELECT TABLE_NAME FROM INFORMATION_SCHEMA.TABLES
                 WHERE TABLE_TYPE='BASE TABLE' AND TABLE_SCHEMA= '%s'", database)
  res <- dbSendQuery(con, statement = sttm)
  list <- dbFetch(res = res)
  list <- list[, 1]
  dbClearResult(res = res)
  dbDisconnect(con)
  return(list)
}
#' Functions to link to flat database and extract column name
#' @describeIn list_TN_mysql Link to flat database and extract column names of
#'   table.
#' @inheritParams genVariable
list_TN_flat <- function(wkdir, research.folder, database){
  if (is.na(wkdir)) {
    stop(simpleError(
      "Please specify the working directory."
    ))
  }

  if (!dir.exists(file.path(wkdir,database))) {
    stop(simpleError("`database` should be the folder containing the flat table, which is either `public_data`, or the path to `private_data` relative to current working directory."))
  }
  list <- list.files(path = file.path(wkdir,database))
  return(list)
}
#' List all the table names the user can access
#' @inheritParams genVariable
#' @return character vector includes all the table names based on the database
#'   choice.
#' @export
ListTN <- function(wkdir = getwd(), database = NA, research.folder = NA,
                   data.type = NA, conn_string = NA,
                   username = NA, password = NA) {
  #database should not be empty.
  if (is.na(database)) {
    stop(simpleError(
      "Please specify a single database."
    ))
  }
  if (tolower(data.type) == "sql") {
    ListTN <- list_TN_mysql(database, username, password)
  }
  if (tolower(data.type) == "flat") {
    if (is.na(database)) {
      warning(simpleWarning("Value for `database` is neither `public` nor `private`. Interpreted as `public` by default."))
      database <- "public_data"
    }
    if (database == "public") {
      database <- "public_data"
    } else if (database == "private") {
      research.folder <- research.folder[1]
      if (is.na(research.folder)) {
        stop(simpleError(
          "Please specify a single research folder."
        ))
      }
      database <- file.path("research", research.folder, "private_data")
    }
    ListTN <- list_TN_flat(wkdir, database, research.folder = NA)
  }
  return(ListTN)
}


#' Functions to link to MySQL database and extract column name
#' @description Link to MySQL database and extract column names of
#'   table in database.
#' @inheritParams genVariable
#' @import DBI
#' @import RMariaDB
list_VN_mysql <- function(database, table_name, username, password) {
  table_name <- tolower(table_name)
  con <- try(dbConnect(MariaDB(), username = username,
                       password = password, dbname = database),
             silent = TRUE)
  if (inherits(con, "try-error")) {
    stop(simpleError(
      "Access to MySQL failed. Please check `username`, `password` and `database` specified."
    ))
  } else {
    message(simpleMessage(
      "Connected to MySQL database\n"
    ))
  }
  if (dbExistsTable(con, table_name, schema = database)) {
    sttm = sprintf("SELECT COLUMN_NAME FROM INFORMATION_SCHEMA.COLUMNS where TABLE_SCHEMA = '%s' AND TABLE_NAME = '%s'", database, table_name)
    res <- dbSendQuery(con, statement = sttm)
    dat <- t(dbFetch(res = res))
    list <- dat[1, ]
    success_msg(table_name, database)
    dbClearResult(res = res)
    dbDisconnect(con)
    return(list)
  } else {
    dbDisconnect(con)
    fail_error(table_name, database)
  }
}
#' Functions to link to flat database and extract column name
#' @describeIn list_VN_mysql Link to flat tables and extract column names.
#' @inheritParams genVariable
#' @importFrom readxl read_excel
#' @importFrom haven read_dta read_sav
list_VN_flat <- function(wkdir, database, table_name) {
  if (is.na(wkdir)) {
    stop(simpleError(
      "Please specify the working directory."
    ))
  }
  #setwd(wkdir)
  if (!dir.exists(file.path(wkdir,database))) {
    stop(simpleError("`database` should be the folder containing the flat table, which is either `public_data`, or the path to `private_data` relative to current working directory."))
  }

  table_file <- file.path(wkdir, database, table_name)
  if (!file.exists(table_file)) {
    fail_error(table_name, database, is_database = FALSE)
  }
  file_vec <- unlist(strsplit(table_name, split = "\\."))
  if (file_vec[length(file_vec)] %in% c("xlsx","xls")) {
    file_vec[length(file_vec)] <- "xlsx"
  }
  dat <- switch(tolower(file_vec[length(file_vec)]),
                rds = readRDS(table_file),
                rdata = get(load(table_file)),
                #csv = data.table::fread(table_file),
                #txt = data.table::fread(table_file),
                csv = read.csv(table_file, header = TRUE),
                txt = read.table(table_file, header = TRUE),
                xlsx = read_excel(table_file),
                dta = read_dta(table_file),
                sav = read_sav(table_file))
  success_msg(table_name, database, is_database = FALSE)
  list <- colnames(dat)
  return(list)
}
#' List all the variable names for specific table
#' @inheritParams genInclusion
#' @param table_name For database this is the name of table. For flat file this
#'   is the name of file to read in, including file extension.
#' @return character vector includes all the table names based on the database
#'   choice.
#' @export
ListVN <- function(wkdir = getwd(), research.folder = NA, data.type = NA,
                   database = NA, table_name = NA,
                   conn_string = NA, username = NA, password = NA) {
  #database should not be empty.
  if (is.na(database)) {
    stop(simpleError("Please specify a single database."))
  }
  if (tolower(data.type) == "sql") {
    ListVN <- list_VN_mysql(database, table_name, username, password)
  }
  if (tolower(data.type) == "flat") {
    if (is.na(database)) {
      warning(simpleWarning("Value for `database` is neither `public` nor `private`. Interpreted as `public` by default."))
      database <- "public_data"
    }
    if (database == "public") {
      database <- "public_data"
    } else if (database == "private") {
      research.folder <- research.folder[1]
      if (is.na(research.folder)) {
        stop(simpleError("Please specify a single research folder."))
      }
      database <- file.path("research", research.folder, "private_data")
    }
    ListVN <- list_VN_flat(wkdir, database, table_name)
  }
  return(ListVN)
}
