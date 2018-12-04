#' Functions to link to ORE database
#' @describeIn Linking to ORE database
#' @inheritParams access_bridge
access_ore <- function(conn_string, database, table_name, username, password) {
  if (!"ORE" %in% rownames(installed.packages())) {
    stop(simpleError(
      "ORE is not available. Please contact your IT administrator for help."
    ))
  }
  # library(ORE)
  con <- try(ore.connect(user = username, password = password,
                         conn_string = conn_string, all = TRUE),
             silent = TRUE)
  if (inherits(con, "try-error")) {
    stop(simpleError(
      "Access to ORE failed. Please check `username`, `password` and `conn_string` specified."
    ))
  } else {
    if (ore.is.connected()) {
      # Identify tables in NUHS schema and make visible to Rstudio
      ore.sync(database)
      ore.attach(database)
      message(simpleMessage(
        paste("Connected to ORE database", database, "\n")
      ))
    } else {
      stop(simpleError(
        paste("Connection to ORE database", database, "failed.")
      ))
    }
  }
  #extract table from dataframe
  if (ore.exists(table_name,database)) {
    dat <- ore.get(table_name, database)
    # Convert all columns to character
    dat <- ore.pull(dat)
    dat <- factor_to_char(dat)
    dat <- time_to_char(dat)
    success_msg(table_name, database)
    return(dat)
  } else {
    ore.disconnect()
    fail_error(table_name, database)
  }
  ore.disconnect()
}

#' Functions to link to Oracle database
#' @describeIn Link to Oracle database
#' @inheritParams access_bridge
access_oracle <- function(database, table_name, username, password) {
  if (!"ROracle" %in% rownames(installed.packages())) {
    stop(simpleError(
      "ROracle is not available. Please contact your IT administrator for help."
    ))
  }
  # library(ROracle)
  drv <- dbDriver("Oracle")
  con <- try(dbConnect(drv = drv, username = username, password = password), silent = TRUE)
  if (inherits(con, "try-error")) {
    stop(simpleError(
      "Access to Oracle failed. Please check `username`, `password` and `database` specified."
    ))
  } else {
    message(simpleMessage(
      "Connected to Oracle database\n"
    ))
  }
  if (dbExistsTable(con, table_name)) {
    dat <- dbReadTable(con, table_name)
    success_msg(table_name, database)
    return(dat)
  } else {
    dbDisconnect(con)
    fail_error(table_name, database)
  }
  dbDisconnect(con)
}

#' Functions to link to MySQL database
#' @describeIn Link to MySQL database
#' @inheritParams access_bridge
#' @import RMySQL
access_mysql <- function(database, table_name, username, password) {
  table_name <- tolower(table_name)
  con <- try(dbConnect(MySQL(), username = username, password = password,
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
    return(dat)
  } else {
    dbDisconnect(con)
    fail_error(table_name, database)
  }
  dbDisconnect(con)
}

#' Functions to link to flat database
#' @describeIn Load R data (\code{.RData})
#' @inheritParams access_bridge
#' @details Request forms are read in with xlsx::read.xlsx, which reads in ICD
#'   codes as charactor. Functions to read in data should read ICD codes as
#'   character as well in optimal cases.
#'
#'   Cannot use data.table::fread because it does not work with R version 3.2.0.
#'
#'   \code{xlsx::read.xlsx} will also read ICD code as character. So if
#'   \code{haven::read_dta} and \code{haven::read_sav} reads ICD as numeric,
#'   then we cannot do anything about it. (In simulated data it is still
#'   character)
access_flat <- function(database, table_name, data.type) {
  if (!dir.exists(database)) {
    stop(simpleError("`database` should be the folder containing the flat table, which is either `public_data`, or the path to `private_data` relative to current working directory."))
  }
  table_file <- file.path(database, table_name)
  if (!file.exists(table_file)) {
    fail_error(table_name, database, is_database = FALSE)
  }
  dat <- switch(data.type,
                rdata = get(load(table_file)),
                csv = read.csv(table_file,header = TRUE,
                               stringsAsFactors = FALSE),
                txt = read.table(table_file, header = TRUE,
                                 stringsAsFactors = FALSE),
                excel = readxl::read_excel(table_file),
                dta = haven::read_dta(table_file),
                sav = haven::read_sav(table_file))
  success_msg(table_name, database, is_database = FALSE)
  dat
}
