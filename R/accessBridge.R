#' Checks whether input data type is supported
#' @inheritParams access_bridge
#' @return Returns \code{data.type} except for \code{xls} and \code{xlsx}, in
#'   which case \code{excel} is returned instead because both types of files
#'   share the same function for importing data.
check_data_type <- function(data.type) {
  if (is.na(data.type) | is.null(data.type)) {
    return(NA)
  }
  types <- list(data_base = c("ore", "sql", "oracle"),
                non_data_base = c("txt", "csv", "xls", "xlsx",
                                  "rdata", "sav", "dta"))
  data.type <- tolower(data.type)
  if (!data.type %in% unlist(types)) {
    stop(simpleError(
      paste(data.type, "is not a supported data type.",
            "Supported data types are (case-insensitive):\n - Database:\n",
            paste0("\t - ", types$data_base, "\n", collapse = ""),
            "- None-database:\n",
            paste0("\t - ", types$non_data_base, "\n", collapse = ""),
            "You can leave `data.type` as default",
            "if you are not linking to database",
            "and file extension is included in `table_name`.")
    ))
  }
  # Return `Excel` for excel files because they can be read using `read_excel`,
  # and `delim` for csv or txt files because they can be read using `fread`
  if (data.type %in% c("xls", "xlsx")) {
    "excel"
  }
  # else if (data.type %in% c("csv", "txt")) {
  #   "delim"
  # }
  else {
    data.type
  }
}
#' Wrapper function to link to data
#' @inheritParams genVariable
#' @param type To decide whether to extract column names (\code{vafiable}) or
#'   whole data.
#' @return Returns the linked data (as \code{data.table}), connection string
#'   (\code{conn_string} for ORE database) and name of database or flat table.
access_bridge <- function(data.type, conn_string, database, table_name,
                          username, password , type = NA) {
  data.type <- check_data_type(as.character(data.type))
  # Type is argument to decide extract column names or whole data.
  if (is.na(type) | is.null(type)) {
    type <- "others"
  }
  if (data.type %in% c("ore", "oracle", "sql")) {
    # To access database, username and password are required
    if (is.na(username) | is.na(password)) {
      stop(simpleError("`username`, `password`, `database` and `table_name` are required to access database."))
    }
    if (data.type == "ore") {
      if (is.na(conn_string)) {
        stop(simpleError("`conn_string` is required to access ORE database."))
      }

      type <- tolower(type)
      if(type %in% c("variable", "col", "column", "columns")){
        dat <- access_ore_col(conn_string = conn_string, database = database,
                              table_name = table_name,
                              username = username, password = password)
      }else{
        dat <- access_ore(conn_string = conn_string, database = database,
                          table_name = table_name,
                          username = username, password = password)
      }

    } else if (data.type == "oracle") {
      if(type %in% c("variable", "col", "column", "columns")){
        dat <- access_oracle_col(database = database,
                              table_name = table_name,
                              username = username, password = password)
      }else{
        dat <- access_oracle(database = database,
                          table_name = table_name,
                          username = username, password = password)
      }
    } else if (data.type == "sql") {
      if(type %in% c("variable", "col", "column", "columns")){
        dat <- access_mysql_col(database = database, table_name = table_name,
                                username = username, password = password)
      } else {
        dat <- access_mysql(database = database, table_name = table_name,
                            username = username, password = password)
      }

    }
  } else {
    # To access flat table. Requires database and research.folder
    # # To convert and identifty the database
    if(type %in% c("variable", "col", "column", "columns")){
      dat <- access_flat_col(database = database, table_name = table_name,
                             data.type = data.type)
    }else{
      dat <- access_flat(database = database, table_name = table_name,
                         data.type = data.type)
    }

  }
  list(dat = data.table::as.data.table(dat),
       conn_string = conn_string, database = database)
}
