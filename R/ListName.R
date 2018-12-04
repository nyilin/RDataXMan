#' List file name, table name and variable name of specific table
#' @description List all the file names of inclusion criteria or varaibles list.
#' @inheritParams genVariable
#' @param mode character string to indicate whether list file names of inclusion
#'   criteria file or variable list.
#' @return character vector includes all the file names based on the mode
#'   choice.
#' @export
ListFN <- function(wkdir=getwd(), research.folder=NA, mode=NA){
  research.folder <- research.folder[1]
  if (is.na(research.folder)) {
    stop(simpleError(
      "Please specify a single research folder."
    ))
  }
  #setwd(wkdir)
  if (!dir.exists(file.path(wkdir,"research",research.folder))) {
    stop(simpleError(
      paste0("Please create folder`research/",research.folder,"` in your working
      directory and move all selected inclusion criteria and variable list stored
      into this folder.")
    ))
  }
  tmp <- list.files(path=file.path(wkdir,"research",research.folder,"request_input"))
  if(is.na(mode)){
    stop(simpleError(
      paste("Failed to find list mode, please specify inclusion or variable mode. \n")
    ))
  }
  if(tolower(mode)%in%c("inc","inclusion","inclusion criteria")){
    ListFN <- do.call("c",lapply(seq_along(tmp),function(i){
      if("inclusion"%in%unlist(strsplit(tmp[i],".",fixed=TRUE))){
        list <- tmp[i]
      }
    }))
  }
  if(tolower(mode)%in%c("var","variable","variable list")){
    ListFN <- do.call("c",lapply(seq_along(tmp),function(i){
      if("variable"%in%unlist(strsplit(tmp[i],".",fixed=TRUE))){
        list <- tmp[i]
      }
    }))
  }
  return(ListFN)
}

#' List all the table names the user can access
#' @inheritParams genVariable
#' @return character vector includes all the table names based on the database
#'   choice.
#' @export
ListTN <- function(wkdir=getwd(), database=NA, research.folder=NA, data.type=NA,
                   conn_string=NA, username=NA, password=NA){
  #database should not be empty.
  if (is.na(database)) {
    stop(simpleError(
      "Please specify a single database."
    ))
  }
  if(tolower(data.type)=="ore"){
    ListTN <- list_TN_ore(database, conn_string, username, password)
  }
  if(tolower(data.type)=="oracle"){
    ListTN <- list_TN_oracle(database, username, password)
  }
  if(tolower(data.type)=="sql"){
    ListTN <- list_TN_mysql(database, username, password)
  }
  if(tolower(data.type)=="flat"){
    if (is.na(database)) {
      warning(simpleWarning("Value for `database` is neither `public` nor `private`. Interpreted as `public` by default."))
      database <- "public_data"}

    if(database=="public"){
    database <- "public_data"
    }else if (database == "private"){
      research.folder <- research.folder[1]
      if (is.na(research.folder)) {
        stop(simpleError(
          "Please specify a single research folder."
        ))
      }
    database <- file.path("research", research.folder, "private_data")
    }
    ListTN <- list_TN_flat(wkdir, database, research.folder=NA)
  }
  return(ListTN)
}
#' Functions to link to ORE database and extract column name
#' @description Link to ORE database and extract the column names of table in
#'   database.
#' @inheritParams genVariable
list_TN_ore <- function(database, conn_string, username, password) {
  if (!"ORE" %in% rownames(installed.packages())) {
    stop(simpleError(
      "ORE is not available. Please contact your IT administrator for help."
    ))
  }
  con <- try(ore.connect(user = username, password = password,
                              conn_string = conn_string, all = TRUE),
             silent = TRUE)
  if (inherits(con, "try-error")) {
    stop(simpleError(
      "Access to ORE failed. Please check `username`, `password` and `conn_string` specified."
    ))
  }else{
    if (ore.is.connected()) {
      # Identify tables in NUHS schema and make visible to Rstudio
      ore.sync(database)
      ore.attach(database)
      message(simpleMessage(
        paste("Connected to ORE database", database, "\n")
      ))
      list <- ore.ls()
    }else{
      stop(simpleError(
        paste("Connection to ORE database", database, "failed.")
      ))
    }

  }
  return(list)
  ore.disconnect()
}
#' Functions to link to Oracle database and extract column name
#' @describeIn list_TN_ore Link to Oracle database and extract the column names
#'   of table in database.
#' @inheritParams genVariable
list_TN_oracle <- function(database, username, password) {
  if (!"ROracle" %in% rownames(installed.packages())) {
    stop(simpleError(
      "ROracle is not available. Please contact your IT administrator for help."
    ))
  }
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
  sttm = sprintf("SELECT TABLE_NAME FROM ALL_TABLES WHERE OWNER= '%s'", database)
  list <- fetch(dbSendQuery(con, statement = sttm))[,1]
  return(list)
  dbDisconnect(con)
}
#' Functions to link to MySQL database and extract column name
#' @describeIn list_TN_ore Link to MySQL database and extract column names of
#'   table in database.
#' @inheritParams genVariable
#' @import RMySQL
list_TN_mysql <- function(database, username, password) {
  con <- try(dbConnect(MySQL(), username = username,
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
  list <- fetch(dbSendQuery(con, statement = sttm))
  list <- list[,1]
  return(list)
  dbDisconnect(con)
}

#' Functions to link to flat database and extract column name
#' @describeIn list_TN_ore Link to flat database and extract column names of
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

#' List all the variable names for specific table
#' @inheritParams genInclusion
#' @param table_name For database this is the name of table. For flat file this
#'   is the name of file to read in, including file extension.
#' @return character vector includes all the table names based on the database
#'   choice.
#' @export
ListVN <- function(wkdir=getwd(), research.folder=NA, data.type=NA,
                   database=NA, table_name=NA,
                   conn_string=NA, username=NA, password=NA){
  #database should not be empty.
  if (is.na(database)) {
    stop(simpleError(
      "Please specify a single database."
    ))
  }
  if(tolower(data.type)=="ore"){
    ListVN <- list_VN_ore(database, table_name, conn_string, username, password)
  }
  if(tolower(data.type)=="oracle"){
    ListVN <- list_VN_oracle(database, table_name, username, password)
  }
  if(tolower(data.type)=="sql"){
    ListVN <- list_VN_mysql(database, table_name, username, password)
  }
  if(tolower(data.type)=="flat"){
    if (is.na(database)) {
      warning(simpleWarning("Value for `database` is neither `public` nor `private`. Interpreted as `public` by default."))
      database <- "public_data"}

    if(database=="public"){
      database <- "public_data"
    }else if (database == "private"){
      research.folder <- research.folder[1]
      if (is.na(research.folder)) {
        stop(simpleError(
          "Please specify a single research folder."
        ))
      }
      database <- file.path("research", research.folder, "private_data")
    }
    ListVN <- list_VN_flat(wkdir, database, table_name)
  }
  return(ListVN)
}
#' Functions to link to ORE database and extract column name
#' @description Link to ORE database and extract the column names of table in
#'   database.
#' @inheritParams genVariable
#' @details For flat files, must use \code{data.table::fread} to read
#'   deliminated files (i.e. \code{txt} and \code{csv}), because this will
#'   result in character ICD codes, which is consistent with ICD read by
#'   \code{readxl::read_excel} (used to read in selection).
#'
#'   \code{xlsx::read.xlsx} will also read ICD code as character. So if
#'   \code{haven::read_dta} and \code{haven::read_sav} reads ICD as numeric,
#'   then we cannot do anything about it. (In simulated data it is still
#'   character)
list_VN_ore <- function(database, table_name, conn_string, username, password) {
  if (!"ORE" %in% rownames(installed.packages())) {
    stop(simpleError(
      "ORE is not available. Please contact your IT administrator for help."
    ))
  }
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
  if (ore.exists(table_name, database)) {
    sttm <- sprintf("CREATE TABLE COLNAME AS SELECT COLUMN_NAME FROM ALL_TAB_COLUMNS where table_name= '%s'",table_name)
    ore.exec(sttm)
    ore.sync(table="COLNAME")
    list <- as.character(ore.pull(COLNAME)[,1])
    ore.drop("COLNAME")
    return(list)
    # dat <- ore.get(table_name, database)
    # # Convert all columns to character
    # dat <- ore.pull(dat)
    # dat <- factor_to_char(dat)
    # success_msg(table_name, database)
    # list <- names(dat)
    # return(list)
  } else {
    fail_error(table_name, database)
  }
  ore.disconnect()
}
#' Functions to link to Oracle database and extract column name
#' @describeIn list_VN_ore Link to Oracle database and extract the column names
#'   of table in database.
#' @inheritParams genVariable
list_VN_oracle <- function(database, table_name, username, password) {
  if (!"ROracle" %in% rownames(installed.packages())) {
    stop(simpleError(
      "ROracle is not available. Please contact your IT administrator for help."
    ))
  }
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
    sttm = sprintf("SELECT COLUMN_NAME FROM ALL_TAB_COLUMNS where table_name= '%s'", table_name)
    res <- dbSendQuery(con, statement = sttm)
    dat <- t(fetch(res = res))
    success_msg(table_name, database)
    list <- dat[1,]
    return(list)
  } else {
    fail_error(table_name, database)
  }
  dbDisconnect(con)
}
#' Functions to link to MySQL database and extract column name
#' @describeIn list_VN_ore Link to MySQL database and extract column names of
#'   table in database.
#' @inheritParams genVariable
#' @import RMySQL
list_VN_mysql <- function(database, table_name, username, password) {
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
  if (dbExistsTable(con, table_name, schema = database)) {
    sttm = sprintf("SELECT COLUMN_NAME FROM INFORMATION_SCHEMA.COLUMNS where TABLE_SCHEMA = '%s' AND TABLE_NAME = '%s'", database, table_name)
    res <- dbSendQuery(con, statement = sttm)
    dat <- t(fetch(res = res))
    list <- dat[1,]
    success_msg(table_name, database)
    return(list)
  } else {
    fail_error(table_name, database)
  }
  dbDisconnect(con)
}

#' Functions to link to flat database and extract column name
#' @describeIn Llist_VN_ore oad R data (\code{.RData})
#' @inheritParams genVariable
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

  table_file <- file.path(wkdir,database, table_name)
  if (!file.exists(table_file)) {
    fail_error(table_name, database, is_database = FALSE)
  }
  file_vec <- unlist(strsplit(table_name, split = "\\."))
  if(file_vec[length(file_vec)] %in% c("xlsx","xls")){
    file_vec[length(file_vec)] <- "xlsx"
  }
  dat <- switch(tolower(file_vec[length(file_vec)]),
                rdata = get(load(table_file)),
                #csv = data.table::fread(table_file),
                #txt = data.table::fread(table_file),
                csv = read.csv(table_file, header = TRUE),
                txt = read.table(table_file, header = TRUE),
                xlsx = readxl::read_excel(table_file),
                dta = haven::read_dta(table_file),
                sav = haven::read_sav(table_file))
  success_msg(table_name, database, is_database = FALSE)
  list <- colnames(dat)
  return(list)
}
