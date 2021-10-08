#' Import multiple surveys
#'
#' @description
#' `r lifecycle::badge("experimental")`
#' \code{import_surveys} imports all survey files in a given directory and
#' outputs an appended dataframe
#'
#' @param eval_directory the top level directory folder
#' @param filetype one of "dta", "rda", or "rds".
#' @param vars additional quoted variables to be included beyond standard survey
#' identifiers
#' @param version a quoted expression that indicates the file version to import
#' @param file_pattern a quoted regex expression to match file patterns in the
#'   directory
#' @return a dataframe of all appended survey files
#' @examples
#' \dontrun{
#' import_surveys("/root/docs/surveys", filetype = "rda", file_pattern = ".") # all files in directory
#' import_surveys("/root/docs/surveys", filetype = "rda", file_pattern = "annual_survey")
#' import_surveys("/root/docs/surveys",
#'                 filetype = "rda",
#'                 vars = c("Var1", "Var3"),
#'                 file_pattern = "annual_survey")
#' import_surveys("/root/docs/surveys",
#'                 filetype = "dta",
#'                 version = "V_03",
#'                 file_pattern = paste0("\\", version, "_survey.dta$"))
#' }
#' @export
#' @import dplyr
#' @import haven
#' @import stringr
#' @import purrr

import_surveys <- function(eval_directory,
                           filetype = "dta",
                           vars = NULL,
                           version = NULL,
                           file_pattern = paste0("\\", version, "_ALL.dta$")) {

  files <- base::list.files(eval_directory,
                            pattern = file_pattern,
                            recursive = TRUE, # search all sub folders
                            full.names = TRUE, # list full file names
                            include.dirs = TRUE
  ) %>% # include the full file path
    dplyr::as_tibble() %>%
    dplyr::rename(paths = value) %>%
    dplyr::mutate(
      # assume the filename is the 4-digit numeric value after the final "/"
      names = stringr::str_extract(basename(paths), "[:digit:]{4}")
    ) %>%
    dplyr::filter(grepl(paste0({{ filetype }}), paths))

  # key variables
  variables <- c("countrycode", "year", "hhid", "pid")
  if (!is.null(vars)) {
    variables <- c(variables, vars)
  }

  # start with empty list
  file_list <- list()

  if (stringr::str_to_lower(filetype) == "dta") {
    df <- purrr::map2(
      files$names, files$paths,
      function(x, y) {
        file_list[[x]] <<- haven::read_dta(y) %>%
          dplyr::select(any_of(variables))
      }
    )
  } else if (stringr::str_to_lower(filetype) == "rds") {
    df <- purrr::map2(
      files$names, files$paths,
      function(x, y) {
        file_list[[x]] <<- base::readRDS(y) %>%
          dplyr::select(any_of(variables))
      }
    )
  } else if (stringr::str_to_lower(filetype) == "rda") {
    df <- purrr::map2(
      files$names, files$paths,
      function(x, y) {
        file_list[[x]] <<- base::readRDS(y) %>%
          dplyr::select(any_of(variables))
      }
    )
  } else {
    # return error
  }



  df <- dplyr::bind_rows(df)

  return(df)
}
