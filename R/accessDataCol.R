#' Functions to link to MySQL database and extract column name
#' @describeIn access_ore_col Link to MySQL database and extract column names of
#'   table in database.
#' @inheritParams access_bridge
#' @import DBI
#' @import RMariaDB
access_mysql_col <- function(database, table_name, username, password) {
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
  if (dbExistsTable(con, table_name)) {
    sttm = sprintf("SELECT COLUMN_NAME FROM INFORMATION_SCHEMA.COLUMNS where TABLE_SCHEMA = '%s' AND TABLE_NAME = '%s'", database, table_name)
    res <- dbSendQuery(con, statement = sttm)
    dat <- t(dbFetch(res = res))
    success_msg(table_name, database)
    dbClearResult(res = res)
    dbDisconnect(con)
    return(dat[1, ])
  } else {
    dbDisconnect(con)
    fail_error(table_name, database)
  }
}
#' Functions to link to flat database and extract column name
#' @describeIn access_ore_col Load column names of table (\code{character
#'   string}).
#' @inheritParams access_bridge
#' @importFrom readxl read_excel
#' @importFrom haven read_dta read_sav
access_flat_col <- function(database, table_name, data.type) {
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
                #delim = data.table::fread(table_file),
                csv = read.csv(table_file,header = TRUE, stringsAsFactors = FALSE),
                txt = read.table(table_file, header = TRUE, stringsAsFactors = FALSE),
                excel = read_excel(table_file),
                dta = read_dta(table_file),
                sav = read_sav(table_file))
  success_msg(table_name, database, is_database = FALSE)
  return(colnames(dat))
}
