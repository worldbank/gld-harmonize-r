#' Count unique household and individual observations
#' @description
#' `r lifecycle::badge("experimental")`
#' \code{cound_ids} provides and output data.frame of the number of unique
#' household ids and individual ids within a user-defined grouping
#'
#' @param data a data frame
#' @param groupvars a quoted vector of variables that define uniqueness across surveys
#' @param hhid the household id variable
#' @param pid the individual id variable
#' @return a tibble
#' @examples
#' \dontrun{
#' # Case where survey variables hhid and pid determine uniqueness
#' yearly_data %>%
#'   count_ids(hhid, pid)
#'
#' # Case where survey observations are unique by hhid and pid within each year
#' # and are now appended together in multi-year tibble
#' appended_yearly_data %>%
#'   count_ids(groupvars = c(year), hhid, pid)
#' }
#' @export
#' @import dplyr

count_ids <- function(data,
                      groupvars = NULL,
                      hhid = hhid,
                      pid = pid
) {

  group <- rlang::quos(groupvars)

  count <- data %>%
    dplyr::group_by({{ group }}) %>%
    dplyr::summarise(
      n_hh = n_distinct({{ hhid }}),
      n_ind= n_distinct({{ pid }})
    )

  return(count)

}
