#' Format columns to be consistent within all databases
#'
#' @return A dataframe with column names lower case and dotted.
format_columns <- function(db) {
  db |>
    dplyr::select(-dplyr::all_of(tidyselect::starts_with("..."))) |>
    dplyr::rename_with(~ stringr::str_replace_all(., " ", ".")) |>
    dplyr::select_all(tolower)
}

#' Get the names of the accessible databases
#'
#' @return a list of database names
#' @export
get_database_names <- function() {
  return (c("program_names_db"))
}

#' Get UWL data base from name
#'
#' @return A dataframe containing database.
#' @export
get_db_from_name <- function(db) {
  if(! db %in% get_database_names()) {
    stop("Unknown database .", db, call. = FALSE)
  }

  readr::read_csv(paste0("https://raw.githubusercontent.com/rallen-uwl/uwldb/main/", db, ".csv"), show_col_types = FALSE) |>
    format_columns ()
}

#' Get UWL program names database
#'
#' @return A dataframe containing program name database.
#' @export
get_program_name_db <- function() {
  get_db_from_name("program_names_db")
}

#' Get UWL student program data
#' currently from WINGS report UWL_SR_REQ_TERM_BY_COLLEGE
#'
#' @return A dataframe containing student program information.
#' @export
get_student_program_db <- function(filename = NA) {
  if (is.na(filename)) {
    stop("current version requires file to create database.")
  }

  file <- normalizePath(filename, mustWork = TRUE)
  ext <- tolower(tools::file_ext(file))

  handler_list <- list(
    csv = list(routine = readr::read_csv, options = list(file = file, show_col_types = FALSE)),
    xls = list(routine = readxl::read_excel, options = list(path = file, skip = 1)),
    xlsx = list(routine = readxl::read_excel, options = list(path = file, skip = 1))
  )

  handler <- handler_list[[ext]]

  if (is.null(handler)) {
    stop("Upsupported file type: .", ext, call. = FALSE)
  }

  do.call(handler$routine, handler$options) |>
    format_columns() |>
    dplyr::rename(
      plan.1.name = `1st.major.desc`,
      plan.1.code = `1st.major`,
      plan.2.name = `major/plan.2.desc`,
      plan.2.code = plan.2,
      plan.3.name = plan.3.desc,
      plan.3.code = plan.3,
      plan.4.name = plan.4.descr,
      plan.4.code = plan.4,
      plan.1.req.term = `1st.major.req.term`
    )
}

