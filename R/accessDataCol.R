#' Functions to link to ORE database and extract column name
#' @description Linking to ORE database and extract column names of table in
#'   database.
#' @inheritParams access_bridge
access_ore_col <- function(conn_string, database, table_name, username, password) {
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
  #extract colunm names of table from dataframe
  if (ore.exists(table_name,database)) {
    sttm <- sprintf("CREATE TABLE COLNAME AS SELECT COLUMN_NAME FROM ALL_TAB_COLUMNS where table_name= '%s'",table_name)
    ore.exec(sttm)
    ore.sync(table = "COLNAME")
    list <- as.character(ore.pull(COLNAME)[,1])
    ore.drop("COLNAME")
    ore.disconnect()
    return(list)
    # dat <- ore.get(table_name, database)
    # dat <- ore.pull(dat)
    # dat <- factor_to_char(dat)
    # success_msg(table_name, database)
    # return(names(dat))
    #dat <- names(dat)
  } else {
    ore.disconnect()
    fail_error(table_name, database)
  }
}
#' Functions to link to Oracle database and extract column name
#' @describeIn access_ore_col Link to Oracle database and extract the column
#'   names of table in database.
#' @inheritParams access_bridge
access_oracle_col <- function(database, table_name, username, password) {
  if (!"ROracle" %in% rownames(installed.packages())) {
    stop(simpleError(
      "ROracle is not available. Please contact your IT administrator for help."
    ))
  }
  # library(ROracle)
  drv <- dbDriver("Oracle")
  con <- try(dbConnect(drv = drv, username = username, password = password),
             silent = TRUE)
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
    # sttm = paste("SELECT * FROM ( SELECT ROW_NUMBER() AS rownumber,columns FROM", paste(database,".",table_name,sep=""),
    # ") AS",table_name,"WHERE rownumber =1")
    #sttm = paste("SELECT * FROM",paste(database,".",table_name,sep=""),"LIMIT 1 OFFSET 0")
    sttm = sprintf("SELECT COLUMN_NAME FROM ALL_TAB_COLUMNS where table_name= '%s'", table_name)
    #sttm = paste("select ROW_NUMBER(1) from",paste(database,".",table_name,".","COLUMNS",sep=""))
    #sttm = paste("select top 1 from",paste(database,".",table_name,sep=""))
    res <- dbSendQuery(con, statement = sttm)
    dat <- t(fetch(res = res))
    success_msg(table_name, database)
    dbDisconnect(con)
    return(dat[1,])
  } else {
    dbDisconnect(con)
    fail_error(table_name, database)
  }
}
#' Functions to link to MySQL database and extract column name
#' @describeIn access_ore_col Link to MySQL database and extract column names of
#'   table in database.
#' @inheritParams access_bridge
#' @import RMySQL
access_mysql_col <- function(database, table_name, username, password) {
  table_name <- tolower(table_name)
  con <- try(dbConnect(MySQL(), username = username,
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
    dat <- t(fetch(res = res))
    success_msg(table_name, database)
    dbDisconnect(con)
    return(dat[1,])
  } else {
    dbDisconnect(con)
    fail_error(table_name, database)
  }
}
#' Functions to link to flat database and extract column name
#' @describeIn access_ore_col Load column names of table (\code{character
#'   string}).
#' @inheritParams access_bridge
access_flat_col <- function(database, table_name, data.type) {
  if (!dir.exists(database)) {
    stop(simpleError("`database` should be the folder containing the flat table, which is either `public_data`, or the path to `private_data` relative to current working directory."))
  }
  table_file <- file.path(database, table_name)
  if (!file.exists(table_file)) {
    fail_error(table_name, database, is_database = FALSE)
  }
  dat <- switch(data.type,
                rdata = get(load(table_file)),
                #delim = data.table::fread(table_file),
                csv = read.csv(table_file,header = TRUE, stringsAsFactors=FALSE),
                txt = read.table(table_file, header = TRUE, stringsAsFactors=FALSE),
                excel = readxl::read_excel(table_file),
                dta = haven::read_dta(table_file),
                sav = haven::read_sav(table_file))
  success_msg(table_name, database, is_database = FALSE)
  return(colnames(dat))
}
