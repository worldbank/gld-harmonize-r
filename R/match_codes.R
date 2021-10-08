#' Determine most likely matches between two vectors.
#' @description
#' `r lifecycle::badge("experimental")`
#' \code{match_codes} creates a one-to-one key between two classification
#' schemes. Given two vectors, it first finds finding unique one-to-one matches,
#' and then systematically determines most likely matches for imperfect original
#' correspondences.
#'
#' @param df The tibble or data.frame object containing the two columns of data
#'   to match
#' @param country_code The name of the column that contains the country-specific
#'   international_code
#' @param international_code The name of the column that contains the
#'   international code
#' @param pad_vars A quoted character vector of variables to pad, otherwise NULL
#' @param check_matches Run checks to verify quality of results, TRUE or FALSE
#' @param default_grepl remove rows that do not match this pattern
#' @param match_bins the number of bins in the density plot for match quality
#'
#' @return a 4-element list object that contains the final match tibble, a
#'   results tibble, a ggplot by match quality and a ggplot by match stage.
#' @examples
#' \dontrun{
#' # works with any set of two string vectors
#' starwars %>%
#'   match_codes(
#'       country_code = homeworld,
#'       international_code = eye_color,
#'       pad_vars = NULL,
#'       check_matches = FALSE,
#'       default_grepl = "[:digit:]",
#'       match_bins = 20)
#'  }
#' @export
#' @import rlang
#' @import tidyr
#' @import tidyselect
#' @import dplyr
#' @import stringdist
#' @importFrom stats dist density
#' @importFrom utils str

match_codes <- function(df,
                        country_code,
                        international_code,
                        pad_vars = NULL,
                        check_matches = FALSE,
                        default_grepl = "[A-Za-z]",
                        match_bins = 5
) {

  vars_quo <- rlang::quos(pad_vars)

  # Drop columns we are not interest in, drop rows where int code is missing
  df <- df %>%
    dplyr::select(c({{country_code}}, {{international_code}})) %>%
    dplyr::filter(!is.na({{international_code}})) %>%
    dplyr::filter(across(.cols = everything(), ~ !base::grepl(default_grepl, .x)))


  # fill down
  df <- df %>%
    tidyr::fill({{country_code}}, .direction = "down")



  # Step 2 - Match at 4 digits ----------
  # Match if concordance is 100%
  match_1 <- df %>%
    dplyr::count({{ country_code }}, {{ international_code }}) %>%
    dplyr::rename("instance" = n) %>%
    dplyr::group_by({{ country_code }}) %>%
    dplyr::mutate(sum = sum(instance)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      "{{international_code}}_orig"  := {{ international_code }},
      "{{country_code}}_orig"  := {{ country_code }},
      corresp_pct = base::round((instance/sum)*100,1),
      dist = stringdist::stringsim({{ country_code }}, {{ international_code }}),
      str_dist = base::round((dist)*100,1),
      match_stage = 4) %>%
    dplyr::filter(corresp_pct == 100)

  # Review
  done_1 <- match_1 %>% dplyr::select({{country_code}}) %>% dplyr::n_distinct()

  df_dst <- df %>% dplyr::select({{country_code}}) %>% dplyr::n_distinct() # original no of distinct
  rest_1 <- df_dst - done_1



  # Step 3 - Match at 3 digits ------------------------------------
  list <- match_1 %>%
    dplyr::select({{country_code}}) %>%
    dplyr::pull()

  # Reduce df to cases not yet matched, reduce international code to 3 digits
  df_2 <- df %>%
    dplyr::filter((!{{ country_code }} %in% list))


  # Match first 3 digits of country code correspond to first three of international code.
  # here, determine the distance on original 4 digit and filter based on 3 digit. This way
  # we have a record of match to original isco code
  match_2 <- df_2 %>%
    dplyr::mutate(
      "{{international_code}}" := stringr::str_sub({{international_code}}, 1,3)) %>%
    dplyr::count({{ country_code }}, {{ international_code }}) %>%
    dplyr::rename("instance" = n) %>%
    dplyr::group_by({{ country_code }}) %>%
    dplyr::mutate(sum = base::sum(instance)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      corresp_pct = base::round((instance/sum)*100,1),
      match_stage = 3) %>%
    dplyr::filter(corresp_pct == 100)

  # Review
  done_2 <- dplyr::select(match_2, {{country_code}}) %>% dplyr::n_distinct()

  rest_2 <- df_dst - done_1 - done_2




  # Step 4 - match at 2 digits ------------------------------------
  list2 <- match_2 %>%
    dplyr::select({{country_code}}) %>%
    dplyr::pull()

  # Reduce df to cases not yet matched, reduce international code to 2 digits
  df_3 <- df_2 %>%
    dplyr::filter(!({{ country_code }} %in% list2))


  # Match by maximum, a country_code count for cases where df_3 may be null
  if (dim(df_3)[1] > 0) {
    set.seed(61035) # to change
    match_3 <- df_3 %>%
      dplyr::mutate(
        "{{international_code}}" := stringr::str_sub({{international_code}}, 1,2)) %>%
      dplyr::count({{ country_code }}, {{ international_code }}) %>%
      dplyr::rename("instance" = n) %>%
      dplyr::group_by({{ country_code }}) %>%
      dplyr::mutate(sum = base::sum(instance)) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(
        corresp_pct = base::round((instance/sum)*100,1),
        match_stage = 2) %>%
      dplyr::group_by({{ country_code }}) %>%
      dplyr::slice_max(corresp_pct) %>%
      dplyr::sample_n(1)
  } else {
    set.seed(61035)
    match_3 <- df_3 %>%
      dplyr::mutate("{{international_code}}" := stringr::str_sub({{international_code}}, 1,2)) %>%
      dplyr::count({{ country_code }}, {{ international_code }}) %>%
      dplyr::rename("instance" = n) %>%
      dplyr::group_by({{ country_code }}) %>%
      dplyr::mutate(sum = base::sum(instance)) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(
        corresp_pct = base::round((instance/sum)*100,1),
        match_stage = 2)
  }


  # Review
  done_3 <- dplyr::select(match_3, {{country_code}}) %>% dplyr::n_distinct()

  rest_3 <- df_dst - done_1 - done_2 - done_3

  n_dist <- df %>% dplyr::select({{ country_code }}) %>% dplyr::n_distinct()


  # Step 5 - append + ggplot ----------------------------------------------



  concord <- dplyr::bind_rows(match_1, match_2, match_3) %>%
    dplyr::select( {{ country_code }}, {{ international_code }},
                   corresp_pct, match_stage,
                   tidyselect::contains("orig")) %>%
    dplyr::rename("match" = corresp_pct)

  if (!is.null(pad_vars)) {

    concord <- concord %>%
      dplyr::mutate(dplyr::across(tidyselect::all_of(!!!vars_quo), ~ stringr::str_pad( .x, 4, pad = "0", side = "right")))

  }

  if (check_matches == TRUE) {

    # check that every unique value returned in country code is contained in input vector
    input.vector <- df %>%
      dplyr::select({{ country_code }}) %>%
      dplyr::pull()

    concord_check <- concord %>%
      dplyr::mutate(match_input = {{country_code}} %in% input.vector)

    assertthat::assert_that(sum(concord_check$match_input) == base::nrow(concord_check))

    # check that the number of unique values between innput and returned is the same
    n_out <- concord %>% dplyr::select({{ country_code }}) %>% dplyr::n_distinct()
    n_in  <- df %>% dplyr::select({{ country_code }}) %>% dplyr::n_distinct()

    assertthat::assert_that(n_in == n_out)

  }

  results <- tibble::tibble(
    match_no = c(1,2,3),
    obs_matched = c(done_1, done_2, done_3),
    obs_remaining = c(rest_1, rest_2, rest_3),
    orig_n_distinct = c(n_dist, n_dist, n_dist)
  )

  gg_match <- ggplot2::ggplot(concord, ggplot2::aes(match, after_stat(density))) +
    ggplot2::geom_freqpoly(binwidth = match_bins) +
    ggplot2::scale_x_continuous(n.breaks = 10, limits = c(0,100)) +
    ggplot2::theme_minimal() +
    ggplot2::labs(x = "Match Score", y = "Relative Density", title = "Distribution of Match Scores")

  gg_stage <- ggplot2::ggplot(concord, ggplot2::aes(match_stage, after_stat(density))) +
    ggplot2::geom_freqpoly(binwidth = 1) +
    ggplot2::theme_minimal() +
    ggplot2::labs(x = "Match Stage", y = "Relative Density", title = "Distribution of Matches in Match Stages")

  list <- list(concord, results, gg_match, gg_stage)



  return(list)


}

