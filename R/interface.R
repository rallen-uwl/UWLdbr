db_path <- "https://raw.githubusercontent.com/rallen-uwl/uwldb/main/"

#' Get UWL program name data
#'
#' @return A dataframe containing program name database.
#' @export
get_program_name_db <- function() {
  readr::read_csv(paste0(db_path, "program_name_db.csv")) %>%
    dplyr::select(-dplyr::all_of(tidyselect::starts_with("..."))) %>%
    dplyr::rename_with(~stringr::str_replace_all(., " ", ".")) %>%
    dplyr::select_all(tolower)
}
