#' Checks whether input data type is supported
#' @inheritParams access_bridge
#' @return Returns \code{data.type} except for \code{xls} and \code{xlsx}, in
#'   which case \code{excel} is returned instead because both types of files
#'   share the same function for importing data.
check_data_type <- function(data.type) {
  if (is.na(data.type) | is.null(data.type)) {
    return(NA)
  }
  types <- list(
    data_base = c("sql"),
    non_data_base = c("txt", "csv", "xls", "xlsx", "rdata", "rds", "sav", "dta")
  )
  data.type <- tolower(data.type)
  if (!data.type %in% unlist(types)) {
    stop(simpleError(
      paste(data.type, "is not a supported data type.",
            "Supported data types are (case-insensitive):\n - Database:\n",
            paste0("\t - ", types$data_base, "\n"),
            "- None-database:\n",
            paste0("\t - ", types$non_data_base, "\n"),
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
#' @return Returns the linked data (as \code{data.table}) and name of database
#'   or flat table.
#' @import data.table
access_bridge <- function(data.type, database, table_name,
                          username, password, type = NA) {
  data.type <- check_data_type(as.character(data.type))
  # Type is argument to decide extract column names or whole data.
  if (is.na(type) | is.null(type)) {
    type <- "others"
  }
  if (data.type %in% c("sql")) {
    # To access database, username and password are required
    if (is.na(username) | is.na(password)) {
      stop(simpleError(
        "`username`, `password`, `database` and `table_name` are required to access database."
      ))
    }
    if (type %in% c("variable", "col", "column", "columns")) {
      dat <- access_mysql_col(database = database, table_name = table_name,
                              username = username, password = password)
    } else {
      dat <- access_mysql(database = database, table_name = table_name,
                          username = username, password = password)
    }
  } else {
    # To access flat table. Requires database and research.folder
    # # To convert and identify the database
    if (type %in% c("variable", "col", "column", "columns")) {
      dat <- access_flat_col(database = database, table_name = table_name,
                             data.type = data.type)
    } else {
      dat <- access_flat(database = database, table_name = table_name,
                         data.type = data.type)
    }
  }
  list(dat = as.data.table(dat), database = database)
}
