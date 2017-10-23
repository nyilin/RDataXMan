#' List file name, table name and variable name of specific table
#' List all the file names of inclusion criteria or varaibles list  --------
#' @param wkdir character string with the whole work-directory.
#' @param research.folder character string with the name of research folder's name
#' @param mode character string to indicate whether list file names of inclusion criteria file or variable list.
#' @return character vector includes all the file names based on the mode choice.
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


#' List all the table names the user can access --------
#' @param wkdir character string with the whole work-directory
#' @param research.folder character string with the name of research folder, only use when your table saved in the private_data folder
#' @param database character string with the real schema name or 'public'/'private'.
#' @param data.type character string with data type including 'ore'/'oracle'/'sql'/'flat'.
#' @param conn_string character string only use when the data.type equal to 'ore'.
#' @param username character string only use when you need to access to server(i.e., ore,oracle,sql).
#' @param password character string only use when you need to access to server(i.e., ore,oracle,sql).
#' @return character vector includes all the table names based on the database choice.
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

list_TN_ore <- function(database, conn_string, username, password) {
  if (!"ORE" %in% rownames(installed.packages())) {
    stop(simpleError(
      "ORE is not available. Please contact your IT administrator for help."
    ))
  }
  library(ORE)
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
      list<-ore.ls()
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
#' @describeIn Link to Oracle database and extract the column names of table in database.
list_TN_oracle <- function(database, username, password) {
  if (!"ROracle" %in% rownames(installed.packages())) {
    stop(simpleError(
      "ROracle is not available. Please contact your IT administrator for help."
    ))
  }
  library(ROracle)
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

  sttm = sprintf("SELECT TABLE_NAME FROM ALL_TABLES WHERE OWNER= '%s'", database)
  list <- fetch(dbSendQuery(con, statement = sttm))[,1]
  return(list)
  dbDisconnect(con)
}
#' Functions to link to MySQL database and extract column name
#' @describeIn Link to MySQL database and extract column names of table in database.
list_TN_mysql <- function(database, username, password) {
  library(RMySQL)
  con <- try(dbConnect(MySQL(), username = username, password = password,
                       dbname = database),
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

#' List all the variable names for specific table --------
#' @param wkdir icharacter string with the whole work-directory
#' @param research.folder character string with the name of research folder, only use when your table saved in the private_data folder
#' @param database character string with the real schema name or 'public'/'private'.
#' @param data.type character string with data type including 'ore'/'oracle'/'sql'/'flat'.
#' @param conn_string character string only use when the data.type equal to 'ore'.
#' @param username character string only use when you need to access to server(i.e., ore,oracle,sql).
#' @param password character string only use when you need to access to server(i.e., ore,oracle,sql).
#' @return character vector includes all the table names based on the database choice.
#' @export
ListVN <- function(wkdir=getwd(), research.folder=NA, data.type=NA,
                   database=NA,table_name=NA,
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

list_VN_ore <- function(database, table_name, conn_string, username, password) {
  if (!"ORE" %in% rownames(installed.packages())) {
    stop(simpleError(
      "ORE is not available. Please contact your IT administrator for help."
    ))
  }
  library(ORE)
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
#' @describeIn Link to Oracle database and extract the column names of table in database.
list_VN_oracle <- function(database, table_name, username, password) {
  if (!"ROracle" %in% rownames(installed.packages())) {
    stop(simpleError(
      "ROracle is not available. Please contact your IT administrator for help."
    ))
  }
  library(ROracle)
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
#' @describeIn Link to MySQL database and extract column names of table in database.
list_VN_mysql <- function(database, table_name, username, password) {
  table_name <- tolower(table_name)
  con <- try(dbConnect(MySQL(), username = username, password = password,
                       dbname=database),
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
  if (dbExistsTable(con, table_name, schema=database)) {
    sttm = sprintf("SELECT COLUMN_NAME FROM INFORMATION_SCHEMA.COLUMNS where TABLE_SCHEMA = '%s' AND TABLE_NAME = '%s'", database, table_name)
    res <- dbSendQuery(con, statement = sttm)
    dat <- t(fetch(res = res))
    list<-dat[1,]
    success_msg(table_name, database)
    return(list)
  } else {
    fail_error(table_name, database)
  }
  dbDisconnect(con)
}

#' Functions to link to flat database and extract column name
#' @describeIn Load R data (\code{.RData})
#' @param table_name The name of file to read in, including file extension.
#' @details Must use \code{data.table::fread} to read deliminated files (i.e.
#'   \code{txt} and \code{csv}), because this will result in character ICD
#'   codes, which is consistent with ICD read by \code{readxl::read_excel} (used
#'   to read in selection).
#'
#'   \code{xlsx::read.xlsx} will also read ICD code as character. So if
#'   \code{haven::read_dta} and \code{haven::read_sav} reads ICD as numeric,
#'   then we cannot do anything about it. (In simulated data it is still
#'   character)
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

}


# Other useful functions --------------------------------------------------

#' Converts factor columns to character
#' @details This is needed because \code{ore.pull} automatically converts
#'   character columns to factors.
factor_to_char <- function(data) {
  data[] <- lapply(data, function(x) {
    if (is.factor(x)) {
      as.character(x)
    } else {
      x
    }
  })
}
#' Flag message for successfully loading a table
success_msg <- function(table_name, database, is_database = TRUE) {
  if (is_database) {
    message(simpleMessage(
      paste("Table", table_name, "is extracted from database", database, "\n")
    ))
  } else {
    message(simpleMessage(
      paste("Table", table_name, "is loaded\n")
    ))
  }
}
#' Flag error for failing to find a table
fail_error <- function(table_name, database, is_database = TRUE) {
  if (is_database) {
    stop(simpleError(
      paste("Failed to find table", table_name, "in database", database, "\n")
    ))
  } else {
    stop(simpleError(
      paste("Failed to find table", table_name, "in", database, "folder\n")
    ))
  }
}


#' # Example -----------------------------------------------------------------
#' wkdir <- "Z:/DASA/RDataXMan Package/example/test_RDataXMan_XH"
#' #' Test of ListFN
#' ListFN(wkdir = wkdir,research.folder = "research_4",mode = "inclusion")
#' # [1] "inclusion.V2M_C_DIAGNOSIS_P.csv_DIAGNOSIS_CD(DIAGNOSIS_DESC).csv.xls"
#' # [2] "inclusion.V2M_C_PATIENT_BASIC.xlsx_DEATH_IND(DEATH_DATE).xlsx.xls"
#'
#' #' Test of ListTN
#' #' flat table
#' ListTN(wkdir = wkdir,database = "public",data.type = "flat")
#' # [1] "V2M_C_DIAGNOSIS_P.csv"      "V2M_C_DIAGNOSIS_P.dta"      "V2M_C_DIAGNOSIS_P.Rdata"
#' # [4] "V2M_C_DIAGNOSIS_P.sav"      "V2M_C_DIAGNOSIS_P.txt"      "V2M_C_DIAGNOSIS_P.xlsx"
#' # [7] "V2M_C_MOVEMENT_PC.csv"      "V2M_C_MOVEMENT_PC.dta"      "V2M_C_MOVEMENT_PC.Rdata"
#' # [10] "V2M_C_MOVEMENT_PC.sav"      "V2M_C_MOVEMENT_PC.txt"      "V2M_C_MOVEMENT_PC.xlsx"
#' # [13] "V2M_C_MOVEMENT_PC_OP.csv"   "V2M_C_MOVEMENT_PC_OP.dta"   "V2M_C_MOVEMENT_PC_OP.Rdata"
#' # [16] "V2M_C_MOVEMENT_PC_OP.sav"   "V2M_C_MOVEMENT_PC_OP.txt"   "V2M_C_MOVEMENT_PC_OP.xlsx"
#' # [19] "V2M_C_PATIENT_BASIC.csv"    "V2M_C_PATIENT_BASIC.dta"    "V2M_C_PATIENT_BASIC.Rdata"
#' # [22] "V2M_C_PATIENT_BASIC.sav"    "V2M_C_PATIENT_BASIC.txt"    "V2M_C_PATIENT_BASIC.xlsx"
#'
#' #' oracle table
#' ListTN(database = "EPHSY",data.type = "ore",username = "ephsy",password = "char53",conn_string = "ORCLTEST")
#' # Connected to ORE database EPHSY
#' # [1] "V2M_C_DIAGNOSIS_P"              "V2M_C_DIAGNOSIS_PTEST"          "V2M_C_MOVEMENT_PC"
#' # [4] "V2M_C_MOVEMENT_PC_OP"           "V2M_C_MOVEMENT_PC_OPTEST"       "V2M_C_MOVEMENT_PCTEST"
#' # [7] "V2M_C_PATIENT_BASIC"            "V2M_C_PATIENT_BASIC_RACEOthers" "V2M_C_PATIENT_BASICTEST"
#' # [10] "V2M_CASE_NO_TOTAL_COST"         "V2M_DIAGNOSIS_PS"               "V2M_MOVEMENT_PC"
#' # [13] "V2M_PATIENT_BASIC"
#'
#' #' oracle table
#' ListTN(database = "EPHSY",data.type = "oracle",username = "ephsy",password = "char53")
#' # [1] "V2M_DIAGNOSIS_PS"               "V2M_MOVEMENT_PC"                "V2M_PATIENT_BASIC"
#' # [4] "V2M_CASE_NO_TOTAL_COST"         "V2M_C_PATIENT_BASIC_RACEOthers" "V2M_C_MOVEMENT_PC_OP"
#' # [7] "V2M_C_DIAGNOSIS_P"              "V2M_C_MOVEMENT_PC_OPTEST"       "V2M_C_DIAGNOSIS_PTEST"
#' # [10] "V2M_C_MOVEMENT_PCTEST"          "V2M_C_PATIENT_BASICTEST"        "V2M_C_PATIENT_BASIC"
#' # [13] "V2M_C_MOVEMENT_PC"
#'
#' # ListTN(database = "EPHSY",data.type = "sql",username = "ephsy",password = "1111")
#' # Show Traceback
#' #
#' # Rerun with Debug
#' # Error: package 'DBI' 0.3.1 is loaded, but >= 0.4 is required by 'RMySQL'
#'
#' #' Test of ListVN
#' #' flat table
#' ListVN(wkdir = wkdir,data.type = "flat",database = "public",table_name = "V2M_C_PATIENT_BASIC.csv")
#' # Table V2M_C_PATIENT_BASIC.csv is loaded
#' # [1] "PATIENT_NRIC" "GENDER"       "DEATH_DATE"   "DEATH_IND"    "RACE"         "BIRTH_YEAR"
#'
#' #' ore table
#' ListVN(data.type = "ore",database = "EPHSY",table_name = "V2M_C_PATIENT_BASIC",conn_string = "ORCLTEST",username = "ephsy",password = "char53")
#' # Connected to ORE database EPHSY
#' # Table V2M_C_PATIENT_BASIC is extracted from database EPHSY
#' # [1] "PATIENT_NRIC" "GENDER"       "DEATH_DATE"   "DEATH_IND"    "RACE"         "BIRTH_YEAR"
#'
#' #' oracle table
#' ListVN(data.type = "oracle",database = "EPHSY",table_name = "V2M_C_PATIENT_BASIC",username = "ephsy",password = "char53")
#' # Connected to Oracle database
#' # Table V2M_C_PATIENT_BASIC is extracted from database EPHSY
#' # [1] "BIRTH_YEAR"   "RACE"         "DEATH_IND"    "DEATH_DATE"   "GENDER"       "PATIENT_NRIC"
