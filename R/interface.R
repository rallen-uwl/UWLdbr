#' path to Github repository for UWL database csv files
#'
#' internal variable
db_path <- "https://raw.githubusercontent.com/rallen-uwl/uwldb/main/"

#' Get UWL program name data
#'
#' @return A dataframe containing program name database.
#' @export
get_program_name_db <- function() {
  readr::read_csv(paste0(db_path, "program_names_db.csv")) |>
    dplyr::select(-dplyr::all_of(tidyselect::starts_with("..."))) |>
    dplyr::rename_with(~ stringr::str_replace_all(., " ", ".")) |>
    dplyr::select_all(tolower)
}

#' Get UWL student program data
#' currently from WINGS report UWL_SR_DAC_SUBJECT_IR_ALL
#'
#' @return A dataframe containing student program information.
#' @export
get_student_program_db <- function(filename = NA) {
  if (is.na(filename)) {
    stop("current version requires file to create database.")
  } else {
    file <- normalizePath(filename, mustWork = TRUE)
    ext <- tolower(tools::file_ext(file))

    handlers <- list(
      csv = readr::read_csv,
      xlsx = readxl::read_excel,
      xls = readxl::read_excel
    )

    handler <- handlers[[ext]]

    if (is.null(handler)) {
      stop("Upsupported file type: .", ext, call. = FALSE)
    }

    handler(file) |>
      dplyr::select(-dplyr::all_of(dplyr::starts_with("..."))) %>%
      dplyr::select_all(~ gsub("\\s+|\\.|\\..|\\...", ".", .)) %>%
      dplyr::select_all(tolower)
  }
}
