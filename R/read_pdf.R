#' Read information from a table in PDF format
#' @description
#' `r lifecycle::badge("experimental")`
#' \code{read_pdf} extracts table multipage information into a tibble. This is a
#' very tricky operation that largely depends on trial and error of column
#' parameter arguments.
#'
#' @param pdf_path The file path to the pdf to read
#' @param page_min The starting page number (inclusive) to begin reading the file.
#' @param page_max The ending page number (inclusive) to end reading the file.
#' @param header A boolean statement indicating if the document always has a constant page header on all pages
#' @param varnames A list of variable names of the output columns
#' @param ymin The minimum y coordinate on the page where the function will read information
#' @param xlabel a 2-element list that indicates start and ending x-coordinates of omitted columns
#' @param xmin a list of x-coordinate column minimums. Same length as varnames.
#' @param xmax a list of x-coordinate column maximums. Same length as varnames.
#' @param numlist an optional vector of omitted numbers to filter.
#' @param fuzzy_rows an optional boolean parameter that toggles slightly misaligned rows in pdf
#' @param match_tol the nearest-neighbor y match tolerance for misaligned rows
#' @return a tibble with tables read from a pdf file
#' @examples
#' \dontrun{
#' read_pdf(
#'         header = TRUE,
#'         varnames = c("ingredient", "grams", "ounces"),
#'         ymin = 50,
#'         xlabel = c(150,400),
#'         xmin = c(0,400,500),
#'         xmax = c(100,500,1000),
#'         numlist = NULL,
#'         fuzzy_rows = FALSE
#'         )
#'
#' read_pdf(
#'         header = FALSE,
#'         varnames = c("var1", "var2")),
#'         ymin = 50,
#'         xlabel = c(500,1000),
#'         xmin = c(0,90,200),
#'         xmax = c(100,200,1000),
#'         numlist = NULL,
#'         fuzzy_rows = TRUE,
#'         match_tol = 3
#'         )
#' }
#' @export
#' @import dplyr
#' @import pdftools
#' @import stringr
#' @import purrr
#' @import magrittr
#' @importFrom stats dist
#' @importFrom utils str

read_pdf <- function(pdf_path, page_min, page_max,

                     header = TRUE,
                     varnames = c("var1", "var2", "var3", "var4", "var5"),
                     ymin = 90,
                     xlabel = c(155, 420),
                     xmin = c(91, 131, 415, 446, 501),
                     xmax = c(130, 175, 445, 500, 9999),
                     numlist = NULL,
                     fuzzy_rows = FALSE,
                     match_tol = 2

) {



  ##### Define Functions #####

  # define 1st sub-function to import the table
  import_table_pdf <- function( page, ... ) {



    # define 2nd sub-function that extracts column info from partially-processed data
    col_info <- function(data_in, ...) { # x_min, x_max, var_name

      col <- data_in %>%
        dplyr::filter(x >= ..1 & x < ..2) %>% # 1 = xmin, 2 = xmax
        dplyr::select(text, y) %>%
        dplyr::mutate(varname = as.character(..3))

      return(col)
    }



    # define 3rd sub-function that matches close y-value rows
    #     # finds closes number to itself other than itself within tolerance, if exists
    nearest_neighbor <- function(ref_col, match_tol = 3, ...) {


      purrr::map_dbl(.x = {{ref_col}}, function(x, ...) {


        # # make tibble of matches
        matches <- tibble(
          ys = {{ref_col}},
          match = dplyr::near(x, {{ref_col}}, {{match_tol}}))

        # return closest match value
        closest_y <- matches %>%
          dplyr::filter(match == TRUE) %>% # keep only vals within tolerance
          dplyr::filter(ys != x) %>% # value should not be itself
          dplyr::distinct(ys) %>%
          dplyr::mutate(dif = base::abs(x - ys)) %>%
          dplyr::arrange(dif)

        # return number where dif is smallest only
        return_val <- as.integer(closest_y$ys)[1]


        # if length of return rector is 0, replace with NA
        if (base::length(return_val) == 0) {
          return_val <- NA_integer_
        }


        return(return_val)


      })



    }




    ##### Data work #####


    nvars <- length(varnames)


    # note: we could read the data directly from pdftools each time but that's computationally
    # expensive. It's better to read in the whole pdf as one object outside the function and
    # call the object from outside the function -- until I can figure out a more elegant way
    # to do this in the function itself.


    # subset data loaded by pdftools
    data <- pdf[[page]]

    data_tib <- data %>%
      dplyr::filter(x < xlabel[1] | x > xlabel[2]) %>%
      dplyr::mutate(str = stringr::str_detect(text, "[:alpha:]+$")) %>%
      dplyr::filter(str == FALSE) %>%
      dplyr::select(x, y, text)


    # if header==TRUE, remove page titles; if no data, no obs.
    if (header == TRUE) {
      data_tib <- data_tib %>%
        dplyr::filter(y >= ymin)
    }

    # if header==FALSE, keep only numbers, and remove specified number args
    if (header == FALSE) {
      data_tib <- data_tib %>%
        dplyr::mutate(str = stringr::str_detect(text, "^[:alpha:]+")) %>%
        dplyr::filter(str == FALSE) %>%
        dplyr::select(-str) %>%
        dplyr::filter(((text %in% numlist) & y < ymin) == FALSE) # y >= ymin
    }




    # create a tibble as a shorthand for the pmap function
    purrr.tib <- tibble::tibble(x_min = xmin,
                                x_max = xmax,
                                var_names = varnames,
                                num = seq(1,nvars))


    # append all the elements in a tabular version
    els <- purrr::pmap( purrr.tib, # these args get passed/walked along in sequence
                        col_info,
                        data_in = data_tib # this arg does not.
    )

    els <- base::do.call(rbind, els)

    table_long <- els %>%
      dplyr::mutate(text = stringr::str_replace(text, "p", "")) %>%
      dplyr::mutate(text = stringr::str_replace(text, "^p;", "")) %>%
      dplyr::mutate(text = stringr::str_replace(text, "\\(", "")) %>%
      dplyr::mutate(text = stringr::str_replace(text, "\\)", "")) %>%
      dplyr::mutate(text = stringr::str_replace(text, "\\;", "")) %>%
      # dplyr::filter(!grepl("^p;", text)) %>% # these characters mess up the columns,
      # dplyr::filter(!grepl("\\(", text)) %>% # remove here
      # dplyr::filter(!grepl("\\)", text)) %>%
      #dplyr::filter(!grepl("\\;", text)) %>%
      dplyr::mutate(text = dplyr::case_when(is.null(text) ~ NA_character_,
                                            TRUE ~ text)) %>%
      dplyr::filter(!text == "")


    table_wide <- table_long %>%
      dplyr::group_by(y) %>%
      dplyr::mutate(page_grp = cur_group_id(),
                    page = page,
                    n_in_row = n())


    keys <- c("page", "page_grp", "y")

    if (fuzzy_rows == TRUE) {

      keys <- c("page", "page_grp", "y")

      table_wide %<>%
        dplyr::ungroup() %>%
        dplyr::mutate(nearest_y = nearest_neighbor(ref_col = y,
                                                   match_tol = 3)) %>%
        dplyr::arrange(page_grp, nearest_y) %>%
        dplyr::mutate(y2 = dplyr::case_when(is.na(nearest_y) ~ as.integer(y),
                                            nearest_y >  y  ~ as.integer(nearest_y),
                                            nearest_y <  y  ~ as.integer(y))) %>%
        dplyr::group_by(y2) %>%
        dplyr::mutate(page_grp2 = cur_group_id(),
                      n_in_row2 = n()) %>%
        dplyr::arrange(page_grp2) %>%
        dplyr::select(-y, -page_grp, -n_in_row) %>%
        dplyr::rename(y = y2, page_grp = page_grp2)



    }



    table_wide %<>%
      dplyr::ungroup() %>%
      dplyr::group_by(page, page_grp) %>%
      tidyr::pivot_wider(names_from = "varname",
                         values_from = "text",
                         id_cols = keys)
    #id_cols = all_of(keys)) # this should leave all cols as ids?
    # will this id_cols work for both situations where fuzzy_rows is both
    # true and false since in FALSE situation only page will exist? should
    # with any_of

    return(table_wide)

  }


  ## Import the PDF with pdftools ----
  ## This actually is the first thing that happens
  pdf <- pdftools::pdf_data(pdf_path)


  ## perform the function call with purrr ----
  ## This calls the mega nested function above and is the second set of things that happens
  raw <- purrr::pmap(list(page_min:page_max), import_table_pdf)
  raw <- base::do.call(rbind, raw)

  return(raw)

}
