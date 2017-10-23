
# Generic helper functions ------------------------------------------------

#' Replaces `*` in a string with digits 0 to 9
replace_key_var <- function(s) {
  if (!str_detect(string = s, pattern = "[*]")) {
    return(s)
  } else {
    str_replace(string = s, pattern = "[*]", replacement = as.character(c(0:9)))
  }
}
#' Find common strings in all elements of a list of strings
find_common <- function(s_list) {
  s_all <- as.character(unlist(s_list))
  s_count <- as.data.frame(table(s_all), stringsAsFactors = FALSE)
  if (!any(s_count$Freq == length(s_list))) {
    list(all = sort(unique(s_all)), common = NULL)
  } else {
    list(all = sort(unique(s_all)),
         common = sort(s_count$s_all[s_count$Freq == length(s_list)]))
  }
}
#' Rename non-key variables in a list of variable names if they appear multiple
#' times in the list of variable names
rename_cols <- function(name_list, all_list = name_list, key_list) {
  name_vec <- unlist(all_list)
  tb <- as.data.frame(table(name_vec))
  names_rep <- tb$name_vec[tb$Freq > 1]
  # names_rep <- setdiff(names_rep, unlist(key_list))
  lapply(1:length(name_list), function(i) {
    nm <- name_list[[i]]
    names_rep_i <- setdiff(names_rep, key_list[[i]])
    i_rep <- which(nm %in% names_rep_i)
    nm[i_rep] <- paste(nm[i_rep], i, sep = ".")
    nm
  })
}

# Check input -------------------------------------------------------------

#' Converts a comma-seperated string into a vector
unzip <- function(string) {
  if (is.character(string) & length(grep(string, pattern = ","))) {
    unlist(strsplit(x = string, split = ","))
  } else {
    string
  }
}
#' Check validity of input for the 3 main functions
#' @details This function is needed because default values of input is
#'   overwritten to \code{""} by Rcmdr.
#' @examples {
#' check_input(NA)
#' check_input(NULL)
#' check_input(NULL, NULL)
#' check_input(NA, NULL)
#' check_input(NULL, NA)
#' check_input(NULL, "a")
#' check_input("a", NULL)
#' check_input(c("a", NULL, NA, "b", ""))
#' }
check_input <- function(...) {
  args <- list(...)
  v <- args[[1]]
  if (is.null(v)) {
    NA
  } else if (all(is.na(v))) {
    NA
  } else if (all(v == "")) {
    NA
  } else {
    i <- which(v %in% c("", " "))
    v[i] <- NA
    as.character(na.omit(v))
  }
}
#' Check data_type and table_name
check_type <- function(table_name, data_type, database) {
  file_vec <- unlist(strsplit(table_name, split = "\\."))
  table_file <- file.path(database, table_name)
  if (file.exists(table_file)) {
    # Input is flat table.
    if (is.na(data_type)) {
      # In data_type is not specified
      data_type <- file_vec[length(file_vec)]
    } else {
      # Check if data_type is consistent
      if (data_type != tolower(file_vec[length(file_vec)])) {
        warning(simpleWarning(
          paste0("-- `data.type` specified is ", data_type,
                 ", yet data type inferred from `table_name` is",
                 tolower(file_vec[length(file_vec)]), ".",
                 " Using data type inferred from `table_name` instead.")
        ))
      }
      data_type <- tolower(file_vec[length(file_vec)])
    }
  } else {
    # Data is flat table without extension, or database
    # data_type must not be NA
    if (is.na(data_type)) {
      stop(simpleError("Please either specify `data.type`, or include file extension in `table_name` if data is stored in a flat table."))
    } else {
      if (!data_type %in% c("ore", "oracle", "sql")) {
        # Data is flat table
        table_name <- paste(table_name, data_type, sep = ".")
      }
    }
    list(table_name = table_name, data_type = data_type)
  }
}

#' Make the database to a full database
database_full <- function(research.folder,database,data_type){
  # Check database
  if (is.na(database)) {
    warning(simpleWarning("Value for `database` is neither `public` nor `private`. Interpreted as `public` by default."))
    database <- c("public_data","public_data")
  }else if(tolower(data_type)%in%c("ore","oracle","sql")){
    database <- c(database,"public_data")
  }else if (database == "private") {
    database <- c(file.path("research", research.folder, "private_data"),
                  file.path("research", research.folder, "private_data"))
  }else {
    database <- c("public_data","public_data")
  }
  database
}


# Read setting list -------------------------------------------------------

#' Extract information from inclusion setting list

get_setting <- function(research_folder, file) {
  # setting_list <- read_excel(file.path("research", research_folder,
  #                                      "request_input", file),
  #                            sheet = "setting list", na = c("", " ", "NA"))
  setting_list <- read.xlsx(file.path("research", research_folder,
                                      "request_input", file),
                            sheetName = "setting list", stringsAsFactors=FALSE )

  #setting_list <- as.data.frame(setting_list)
  setting_list[, 2] <- as.character(setting_list[, 2])
  if ("key.var" %in% setting_list[, 1]) {
    key_var <- setting_list[which(setting_list[, 1] == "key.var"), 2]
    key_desc <- unzip(setting_list[which(setting_list[, 1] == "key.desc"), 2])
  } else {
    key_var <- NULL
    key_desc <- NULL
  }
  list(
    table_name = setting_list[which(setting_list[, 1] == "table_name"), 2],
    key_var = key_var,
    key_desc = key_desc,
    identifier_var = unzip(
      setting_list[which(setting_list[, 1] == "identifier.var"), 2]
    ),
    data_type = setting_list[which(setting_list[, 1] == "data.type"), 2],
    conn_string = setting_list[which(setting_list[, 1] == "conn_string"), 2],
    database = setting_list[which(setting_list[, 1] == "database"), 2]
  )
}

# Extract key for inclusion -----------------------------------------------

#' Read in inclusion in short format
extract_key_short <- function(research_folder, file, key_var, key_desc = NA) {
  request <- read_excel(file.path("research", research_folder, "request_input",
                                  file),
                        sheet = "overall list")
  request <- as.data.frame(request)
  if (!(key_var %in% colnames(request))) {
    stop(simpleError(
      paste(key_var, "is not consistent in the request inclusion criteria.",
            "The first column that is not `sno` is treated as key variable.")
    ))
  }
  # Expand the incomplete key variable and return
  key <- request[, key_var]
  key <- as.vector(unlist(sapply(key, function(k) replace_key_var(k))))
  if (!is.na(key_desc)) {
    desc <- request[, key_desc]
  } else {
    desc <- NULL
  }
  list(key = key, desc = desc,
       is_expanded = any(stringr::str_detect(string = request[, key_var],
                                             pattern = "[*]")))
}
#' Validate request form in long format
#' @details \code{request_file} is annotated inclusion list, and
#'   \code{template_file} is the one generated by \code{genInclusion}. Column
#'   names and \code{key.var} should be identical in the two files.
check_long <- function(research_folder, database, request_file, template_file) {
  # request <- read_excel(file.path("research", research_folder, "request_input",
  #                                 request_file),
  #                       sheet = "overall list")
  request <- read.xlsx(file.path("research", research_folder, "request_input",
                                 request_file),
                       sheetName = "overall list", stringsAsFactors = FALSE)
  request <- as.data.frame(request)
  # template <- read_excel(file.path(paste(database, "_template", sep = ""), template_file),
  #                        sheet = "overall list")
  template <- read.xlsx(file.path(paste(database, "_template", sep = ""), template_file),
                        sheetName = "overall list", stringsAsFactors=FALSE)
  template <- as.data.frame(template)
  if (!(nrow(template) == nrow(request))) {
    stop(simpleError("The rows of the template and request excel files do not match up."))
  }
  if (!("selection" %in% colnames(request) & ("selection" %in% colnames(template)))) {
    stop(simpleError("Column `selection` is required in both template and request form to indicate user selection."))
  }
  template <- template[order(template[, 1]), ]
  request <- request[order(request[, 1]), ]
  if (!all(template[, 1] == request[, 1])) {
    stop(simpleError("The first column of the template and request excel files does not match up."))
  } else {
    message(simpleMessage("  Request form is consistent with template.\n"))
  }
  request
}
extract_selection <- function(selection, selection_symbol, key_var) {
  selection <- tolower(as.character(selection))
  if (!all(na.omit(selection) %in% selection_symbol)) {
    stop(simpleError(
      paste("Please make selection by putting", toString(selection_symbol),
            "in column `selection` of `overall list`.\n")
    ))
  }
  key_var[selection %in% selection_symbol]
}
extract_selection_logic <- function(logic, request, key_var) {
  logic <- paste0(as.character(na.omit(logic)))
  if (length(logic) > 1) {
    stop(simpleError("Please write logical statement in one row."))
  } else {
    sttm <- logic
    subset(request, subset = eval(parse(text = sttm)))[, key_var]
  }
}
#' Read in inclusion in long format or variable list (in this case key_var is
#' variable)
extract_key_long <- function(research_folder, database, file, key_var) {
  request <- check_long(research_folder = research_folder,
                        database = database,
                        request_file = file,
                        template_file = file)
  request <- as.data.frame(request)
  if (!(key_var %in% colnames(request))) {
    stop(simpleError(
      paste(key_var, "is not consistent in the request inclusion criteria.",
            "The first column that is not `sno` is treated as key variable.")
    ))
  }
  key <- extract_selection(selection = request[, "selection"],
                           selection_symbol = "x", key_var = request[, key_var])
  if (length(key) == 0) {
    warning(simpleWarning(
      paste(
        "Using logical statement in the first entry of column `logic`",
        "in `overall list` to determine inclusion criteria.\n"
        # "No", key_var, "satisfies inclusion criteria in '", file,
        # "' selected accroding to selection column.\n"
      )
    ))
    key <- try(extract_selection_logic(
      logic = request[, "logic"], request = request, key_var = key_var
    ))
  }
  list(key = key)
}
#' Read in inclusion and variable (wrapper)
extract_key <- function(research_folder, database, file, key_var, key_desc = NA,
                        type) {
  version_mode <- unlist(strsplit(file, split = ".", fixed = TRUE))
  is_short <- "short" %in% tolower(version_mode)
  if (is_short) {
    message(simpleMessage(
      if (type == "inclusion") {
        paste0(" ** Inclusion criteria '", file, "' ",
               "is in short version.\n")
      } else {
        paste0(" ** Variable list '", file, "' ",
               "is in short version.\n")
      }
    ))
    extract_key_short(research_folder = research_folder, file = file,
                      key_var = key_var, key_desc = key_desc)
  } else {
    message(simpleMessage(
      if (type == "inclusion") {
        paste0(" ** Inclusion criteria '", file, "' ", "is in long version.\n")
      } else {
        paste0(" ** Variable list '", file, "' ", "is in long version.\n")
      }
    ))
    # check template subfolder
    template_folder <- paste0(database, "_template")
    if (!dir.exists(template_folder)) {
      stop(simpleError("Please use functions `genInclusion` and `genVariable` to generate inclusion criteria and variable lists before extracting data."))
    }

    key <- extract_key_long(research_folder = research_folder,
                            database = database,
                            file = file, key_var = key_var)
    key$is_short <- FALSE
    key
  }
}

# Compare short inclusion -------------------------------------------------

#' Write a table to compare key_var and key_desc for short version inclusion
compare_key_short <- function(setting, key) {
  if (is.na(setting$key_desc) & key$is_expanded) {
    if (key$is_expanded) {
      key_var_dat <- as.data.frame(unique(dat[, setting$key_var, with = FALSE]))
      key_var_req <- as.data.frame(key$key)
      names(key_var_req) <- setting$key_var
      summary_comp <- merge(key_var_dat, key_var_req, by = setting$key_var,
                            all = TRUE, sort = TRUE,
                            suffixes = c(".dat", ".req"))
      file <- paste0("compare_requirement_", setting$table_name,
                     "_", setting$key_var, "summary.xlsx")
      list(table = summary_comp, file = file)
    } else {
      NULL
    }
  } else {
    key_var_dat <- as.data.frame(unique(dat[, c(setting$key_var, setting$key_desc),
                                            with = FALSE]))
    key_var_req <- as.data.frame(cbind(key$key, key$desc))
    names(key_var_req) <- c(setting$key_var, setting$key_desc)
    summary_comp <- merge(key_var_dat, key_var_req, by = setting$key_var,
                          all = TRUE, sort = TRUE,
                          suffixes = c(".dat", ".req"))
    file <- paste0("compare_requirement_", setting$table_name,
                   "_", setting$key_var,
                   "(", paste(setting$key_desc, collapse = "_"), ")",
                   "summary.xlsx")
    list(table = summary_comp, file = file)
  }
}

# Extract and merge inclusion data ----------------------------------------

#' Merge inclusion data or variable list
merge_data <- function(data_list, data_logic) {
  dat_m <- as.data.frame(data_list[[1]]$dat)
  if (length(data_list) == 1) {
    return(dat_m)
  }
  colnames(dat_m) <- data_list[[1]]$cname2
  key_m <- data_list[[1]]$identifier_var
  # key_all <- key_m
  for (i in 2:length(data_list)) {
    dat_i <- as.data.frame(data_list[[i]]$dat)
    colnames(dat_i) <- data_list[[i]]$cname2
    key_i <- data_list[[i]]$identifier_var
    # key_all <- union(key_all, key_i)
    key_m <- intersect(key_m, key_i)
    dat_m <- merge(dat_m, dat_i, by = key_m, all = (data_logic == "union"))
    # key_var of the merged data include key_var of all data that are merged
    key_m <- union(key_m, key_i)
  }
  data.table::as.data.table(dat_m)
}
#' Find the intersection or union of all inclusion, based on \code{data_logic}
merge_inclu <- function(inclu_list, data_logic) {
  if (length(inclu_list) == 1) {
    id_merged <- as.list(inclu_list[[1]]$dat[, inclu_list[[1]]$identifier_var,
                                             with = FALSE])
    id_merged <- lapply(id_merged, function(id) unique(id))
    inclu_list[[1]]$cname2 <- colnames(inclu_list[[1]]$dat)
    return(list(inclu_list = inclu_list, data_merged = inclu_list[[1]]$dat,
                id_merged = id_merged,
                id_common = inclu_list[[1]]$identifier_var,
                key_all = inclu_list[[1]]$key_var))
  }
  id <- find_common(lapply(inclu_list, function(l) l$identifier_var))
  id_all <- id$all
  if (is.null(id$common)) {
    stop(simpleError(paste(
      "There is no common identifier variable for all inclusion data.",
      "Data cannot be extracted based on such inclusion criteria."
    )))
  }
  id_common <- id$common
  # Rename variables if they exist in more than 1 inclusion tables
  # Do not rename identifier_var
  id_list <- lapply(inclu_list, function(l) l$identifier_var)
  key_list <- lapply(inclu_list, function(l) l$key_var)
  desc_list <- lapply(inclu_list, function(l) l$key_desc)
  all_list <- lapply(inclu_list, function(l) colnames(l$dat))
  key_list2 <- rename_cols(name_list = key_list, all_list = all_list,
                           key_list = id_list)
  desc_list2 <- rename_cols(name_list = desc_list, all_list = all_list,
                            key_list = id_list)
  cname_list2 <- rename_cols(name_list = all_list, all_list = all_list,
                             key_list = id_list)
  for (i in 1:length(inclu_list)) {
    inclu_list[[i]]$cname2 <- cname_list2[[i]]
    inclu_list[[i]]$key_desc2 <- desc_list2[[i]]
    inclu_list[[i]]$key_var2 <- key_list2[[i]]
  }
  inclu_m <- merge_data(data_list = inclu_list, data_logic = data_logic)
  id_merged <- as.list(inclu_m[, unique(id_all), with = FALSE])
  id_merged <- lapply(id_merged, unique)
  # Filter raw inclusion data
  inclu_list <- lapply(inclu_list, function(l) {
    l$dat <- filter_data(dat = l$dat, identifier_var = l$identifier_var,
                         id_merged = id_merged)
    l
  })
  list(inclu_list = inclu_list, data_merged = inclu_m,
       id_merged = id_merged, id_common = id_common,
       key_all = unlist(lapply(inclu_list, function(l) l$key_var2)))
}
#' Processing request form
process_inclu <- function(research.folder, inclusion.xls.file, data_logic,
                          overwrite, username=NA, password=NA){
  inclu_list <- lapply(seq_along(inclusion.xls.file), function(i) {
    setting <- get_setting(research_folder = research.folder,
                           file = inclusion.xls.file[i])
    if (basename(setting$database) == "private_data") {
      setting$db <- "private"
    } else {
      setting$db <- "public"
    }
    database <- database_full(research.folder,setting$db,setting$data_type)
    # Extract selected values of key_var and key_desc (if any)
    key <- extract_key(research_folder = research.folder,
                       database = database[2],
                       file = inclusion.xls.file[i],
                       key_var = setting$key_var,
                       key_desc = setting$key_desc, type = "inclusion")
    # Load inclusion data
    # database <- database_full(research.folder,setting$database,setting$data_type)
    # print(database)
    dat_full <- access_bridge(data_type = setting$data_type,
                              conn_string = setting$conn_string,
                              database = setting$database,
                              table_name = setting$table_name,
                              username = username,
                              password = password)$dat
    if (!any(setting$identifier_var %in% colnames(dat_full))) {
      id_vec <- setting$identifier_var[!setting$identifier_var %in%
                                         colnames(dat_full)]
      stop(simpleError(
        paste("Identifier variables", toString(id_vec),
              "are not found in table", setting$table_name)
      ))
    }
    if (!setting$key_var %in% colnames(dat_full)) {
      stop(simpleError(
        paste("Key variable", setting$key_var, "is not found in table",
              setting$table_name)
      ))
    }
    if (any(!is.na(setting$key_desc) & !setting$key_desc %in% colnames(dat_full))) {
      stop(simpleError(
        paste("Key variable description", setting$key_desc,
              "is not found in table", setting$table_name)
      ))
    }
    # Extract subset of interest
    dat <- dat_full[get(setting$key_var) %in% key$key,
                    na.omit(c(setting$identifier_var, setting$key_var,
                              setting$key_desc)),
                    with = FALSE]
    dat <- unique(dat)
    if (nrow(dat) == 0) {
      warning(simpleWarning(
        paste("No rows in", setting$table_name,
              "satisfy inclusion criteria specified.")
      ))
    }
    # If we use identifier_var as key_var, there will be duplicated columns in
    # an inclusion data. In this case we will remove duplicates.
    dat <- dat[, unique(names(dat)), with = FALSE]
    # Compare key_desc for short version
    if (key$is_short) {
      summary_comp <- compare_key_short(setting = setting, key = key)
      if (!is.null(summary_comp)) {
        write_to_file(table = summary_comp$table,
                      file = file.path("research", research.folder,
                                       "request_output", summary_comp$file),
                      sheetName = "Compare.request", overwrite = overwrite)
      }
    }
    list(dat = dat, file_name = inclusion.xls.file[i],
         table_name = setting$table_name,
         key_var = setting$key_var, key_desc = setting$key_desc,
         identifier_var = setting$identifier_var, index = i)
  })
  # Merge inclusion data based on data_logic
  merge_inclu(inclu_list, data_logic)
}

# Summarise inclusion -----------------------------------------------------

count_inclu <- function(inclu_list) {
  id_vec <- names(inclu_list$id_merged)
  # Count unique identifiers
  count_all <- do.call("rbind", lapply(id_vec, function(id) {
    message(simpleMessage(
      paste0(" ** Counting the total unique number of ", id, "\n")
    ))
    c(paste("Total unique", id), length(unique(inclu_list$id_merged[[id]])))
  }))
  colnames(count_all) <- c("Item", "Summary")
  # Count key variable by identifier
  if (nrow(inclu_list$data_merged) == 0) {
    warning(simpleWarning(
      " ** No data extracted with inclusion criteria specified.\n"
    ))
    count_key <- NULL
  } else {
    count_key <- lapply(inclu_list$key_all, function(key) {
      message(simpleMessage(
        paste0(" ** Counting the total unique number of ", key,
               " by each identifier variable\n")
      ))
      dat_m <- as.data.frame(
        unique(inclu_list$data_merged[, c(key, id_vec[1]), with = FALSE])
      )
      dat_m$n <- 1
      count <- aggregate(dat_m$n, by = list(dat_m[, key]), sum)
      colnames(count) <- c(key, paste("Count by", id_vec[1]))
      if (length(id_vec) > 1) {
        for (i in 2:length(id_vec)) {
          dat_m <- as.data.frame(
            unique(inclu_list$data_merged[, c(key, id_vec[i]), with = FALSE])
          )
          dat_m$n <- 1
          count_i <- aggregate(dat_m$n, by = list(dat_m[, key]), sum)
          colnames(count_i) <- c(key, paste("Count by", id_vec[i]))
          count <- merge(count, count_i, by = 1, all = TRUE)
        }
      }
      count[, -1] <- apply(as.data.frame(count[, -1]), 2, function(x) {
        x[is.na(x)] <- 0
        x
      })
      count
    })
  }
  list(count_all = count_all, count_key = count_key)
}

# Extract and merge variable list -----------------------------------------

filter_data <- function(dat, identifier_var, id_merged) {
  id_common <- intersect(identifier_var, names(id_merged))
  rows <- unlist(lapply(id_common, function(id) {
    which(unlist(dat[, id, with = FALSE]) %in% id_merged[[id]])
  }))
  count <- as.data.frame(table(rows), stringsAsFactors = FALSE)
  rows <- as.numeric(count$rows[count$Freq == length(id_common)])
  if (length(rows) == 0) {
    warning(simpleWarning("No data extracted with inclusion criteria specified.\n"))
  }
  unique(dat[rows, ])
}
#' Find the intersection or union of all variable lists, based on
#' \code{data_logic}
merge_var <- function(var_list, data_logic) {
  if (length(var_list) == 1) {
    id_merged <- as.list(var_list[[1]]$dat[, var_list[[1]]$identifier_var,
                                           with = FALSE])
    id_merged <- lapply(id_merged, function(id) unique(id))
    var_list[[1]]$cname2 <- colnames(var_list[[1]]$dat)
    return(list(var_list = var_list, data_merged = var_list[[1]]$dat,
                id_merged = id_merged,
                id_common = var_list[[1]]$identifier_var))
  }
  id <- find_common(lapply(var_list, function(l) l$identifier_var))
  id_all <- id$all
  if (is.null(id$common)) {
    warning(simpleWarning("There is no common identifier variable for all variable lists. Data cannot be merged based on such variable lists."))
    return(list(var_list = var_list, data_merged = NULL,
                id_merged = id_merged,
                id_common = var_list[[1]]$identifier_var))
  }
  id_common <- id$common
  # Rename variables if they are exist in more than 1 inclusion tables
  # Do not rename identifier_var
  id_list <- lapply(var_list, function(l) l$identifier_var)
  all_list <- lapply(var_list, function(l) colnames(l$dat))
  cname_list2 <- rename_cols(name_list = all_list, key_list = id_list)
  for (i in 1:length(var_list)) {
    var_list[[i]]$cname2 <- cname_list2[[i]]
  }
  var_m <- merge_data(data_list = var_list, data_logic = data_logic)
  id_merged <- as.list(var_m[, unique(id_all), with = FALSE])
  id_merged <- lapply(id_merged, unique)
  list(var_list = var_list, data_merged = var_m,
       id_merged = id_merged, id_common = id_common)
}
process_var <- function(research.folder, variable.xls.file,
                        inclu_list, overwrite, username = NA, password = NA) {
  if (all(is.na(variable.xls.file))) {
    return(NULL)
  }
  id.no.keep <- NULL
  var_list <- lapply(seq_along(variable.xls.file), function(i) {
    setting <- get_setting(research_folder = research.folder,
                           file = variable.xls.file[i])
    if (basename(setting$database) == "private_data") {
      setting$db <- "private"
    } else {
      setting$db <- "public"
    }
    database <- database_full(research.folder, setting$db, setting$data_type)

    # Extract selected values of key_var and key_desc (if any)
    var_names <- extract_key(research_folder = research.folder,
                             database = database[2],
                             file = variable.xls.file[i],
                             key_var = "variable", type = "variable")

    # Load inclusion data
    dat_full <- access_bridge(data_type = setting$data_type,
                              conn_string = setting$conn_string,
                              database = setting$database,
                              table_name = setting$table_name,
                              username = username,
                              password = password)$dat
    if (!any(setting$identifier_var %in% colnames(dat_full))) {
      id_vec <- setting$identifier_var[!setting$identifier_var %in%
                                         colnames(dat_full)]
      stop(simpleError(
        paste("Identifier variables", toString(id_vec),
              "are not found in table", setting$table_name)
      ))
    }
    #dat <- dat_full[, var_names$key, with = FALSE]
    dat <- dat_full[, unique(c(setting$identifier_var, var_names$key)),
                    with = FALSE]
    dat <- filter_data(dat = unique(dat),
                       identifier_var = setting$identifier_var,
                       id_merged = inclu_list$id_merged)

    list(dat = dat, file_name = variable.xls.file[i],
         table_name = setting$table_name,
         identifier_var = setting$identifier_var,
         other_var = setdiff(colnames(dat), setting$identifier_var), index = i)
  })
  id.keep <- unique(unlist(lapply(seq_along(variable.xls.file), function(i){
    var_list[[i]]$other_var
  })))
  #identifier_var <- var_list[[1]]$identifier_var
  var_list <- merge_var(var_list = var_list, data_logic = "Union")
  # Summarise each variable in the merged data
  var_m <- var_list$data_merged
  if (nrow(var_m) == 0) {
    var_summ <- NULL
  } else {
    identifier_var <- var_list[[1]]$identifier_var
    id_no_keep <- which(names(var_m) %in% identifier_var)

    var_summ <- do.call("rbind", lapply(names(var_m)[which(names(var_m)%in%id.keep)], function(var) {
      x <- as.data.frame(var_m)[, var]
      if (var %in% c(names(inclu_list$id_merged), inclu_list$key_all,
                     names(var_list$id_merged))) {
        summ <- summarise_cat(x)
        cbind(Variable = c(var, rep("", nrow(summ) - 1)), summ)
      } else {
        if (is.character(x) | is.factor(x) | is.logical(x) |
            inherits(x, "Date") | inherits(x, "POSIXct")) {
          summ <- summarise_cat(x)
          cbind(Variable = c(var, rep("", nrow(summ) - 1)), summ)
        } else {
          cbind(Variable = var, summarise_cts(x))
        }
      }
    }))
  }
  list(var_list = var_list, var_summ = var_summ)
}

# Summarise variable list -------------------------------------------------

summarise_cts <- function(x) {
  data.frame(N = sum(!is.na(x)), Group = NA,
             Summary = sprintf("%.2f (%.2f)",
                               mean(x, na.rm = TRUE), sd(x, na.rm = TRUE)),
             Type = "Mean(S.D.)")
}
summarise_cat <- function(x) {
  if (all(is.na(x))) {
    data.frame(N = 0, Group = NA, Summary = sprintf("%d (%.1f%%)", NA, NA),
               Type = "")
  } else {
    count <- as.data.frame(table(x))
    count$Prop <- count$Freq / length(x)
    summ <- data.frame(Group = count$x,
                       Summary = paste0(count$Freq,
                                        " (", round(count$Prop * 100, 2), "%)"),
                       Type = "N(%)")
    summ <- cbind(N = c(length(x), rep(NA, nrow(summ) - 1)), summ)
    if (anyNA(x)) {
      rbind(
        summ,
        data.frame(N = NA, Group = NA,
                   Summary = sprintf("%d (%.1f%%)", sum(is.na(x)),
                                     sum(is.na(x)) / length(x) * 100),
                   Type = "")
      )
    } else {
      summ
    }
  }
}

# Write information to file -----------------------------------------------

#' Check whether file exists and write file
#' @details No need to check \code{overwrite} when adding sheets to the file. In
#'   this case, set it to \code{NULL}.
write_to_file <- function(table, file, sheetName, overwrite = NULL) {
  if (!is.null(overwrite)) {
    if (file.exists(file)) {
      msg <- paste("The file:'", file, "' already exists")
      if (overwrite) {
        msg <- paste(msg, "and will be overwritten\n")
        file.remove(file)
      } else {
        stop(simpleError(paste0(msg, ".\n")))
      }
    }
    xlsx::write.xlsx(table, file = file, sheetName = sheetName,
                     row.names = FALSE, showNA = FALSE)
  } else {
    xlsx::write.xlsx(table, file = file, sheetName = sheetName,
                     row.names = FALSE, showNA = FALSE, append = TRUE)
  }
}
#' Write the list of unique identifiers to \code{csv}
write_id <- function(id_merged, type, research.folder) {
  # type is either "inclusion" or "variable"
  # len_max <- max(unlist(lapply(id_merged, function(id) length(id))))
  # id_merged_mat <- do.call("cbind", lapply(id_merged, function(id) {
  #   c(id, rep("", len_max - length(id)))
  # }))
  id_merged_mat <- unique(id_merged)
  write.csv(id_merged_mat,
            file = paste0("research/", research.folder,
                          "/request_output/", type, "_identifier_var.csv"),
            row.names = FALSE, na = "")
  id_merged
}
#' Write a list of raw data to a group of \code{csv}
write_data_raw <- function(data_list, type, research.folder) {
  lapply(data_list, function(l) {
    dat <- l$dat
    colnames(dat) <- l$cname2
    # file = paste0("research/", research.folder,
    #               "/request_output/extract_dat_", type, "_raw_dat_",
    #               l$index, ".csv")
    file = paste0("research/", research.folder,"/request_output/extract_dat_",l$file_name,".csv")
    write.csv(dat,
              file = file,
              row.names = FALSE, na="")
    return(dat)
  })
}
