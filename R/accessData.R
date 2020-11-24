#' Functions to link to MySQL database
#' @describeIn Link to MySQL database
#' @inheritParams access_bridge
#' @import DBI
#' @import RMariaDB
access_mysql <- function(database, table_name, username, password) {
  table_name <- tolower(table_name)
  con <- try(dbConnect(MariaDB(), username = username, password = password,
                       dbname = database),
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
  if (dbExistsTable(con, table_name)) {
    dat <- dbReadTable(con, table_name)
    success_msg(table_name, database)
    dbDisconnect(con)
    return(dat)
  } else {
    dbDisconnect(con)
    fail_error(table_name, database)
  }
}
#' Functions to link to flat database
#' @describeIn Load R data (\code{.RData})
#' @inheritParams access_bridge
#' @details Request forms are read in with xlsx::read.xlsx, which reads in ICD
#'   codes as character. Functions to read in data should read ICD codes as
#'   character as well in optimal cases.
#'
#'   Cannot use data.table::fread because it does not work with R version 3.2.0.
#'
#'   \code{xlsx::read.xlsx} will also read ICD code as character. So if
#'   \code{haven::read_dta} and \code{haven::read_sav} reads ICD as numeric,
#'   then we cannot do anything about it. (In simulated data it is still
#'   character)
#' @importFrom readxl read_excel
#' @importFrom haven read_dta read_sav
access_flat <- function(database, table_name, data.type) {
  if (!dir.exists(database)) {
    stop(simpleError("`database` should be the folder containing the flat table, which is either `public_data`, or the path to `private_data` relative to current working directory."))
  }
  table_file <- file.path(database, table_name)
  if (!file.exists(table_file)) {
    fail_error(table_name, database, is_database = FALSE)
  }
  dat <- switch(data.type,
                rds = readRDS(table_file),
                rdata = get(load(table_file)),
                csv = read.csv(table_file,header = TRUE,
                               stringsAsFactors = FALSE),
                txt = read.table(table_file, header = TRUE,
                                 stringsAsFactors = FALSE),
                excel = read_excel(table_file),
                dta = read_dta(table_file),
                sav = read_sav(table_file))
  success_msg(table_name, database, is_database = FALSE)
  dat
}
