#' Examine column-ordered data in a pdf
#'
#' @description
#' `r lifecycle::badge("experimental")`
#' \code{reveal_pdf_cols} creates a histogram of elements of x-axis positions of
#' the pdf to read. The pdf is read in a semi-processed form and the function
#' then creates a light interactive output, designed to help the user prepare the
#' column parameters for \code{read_pdf}.
#'
#' @param pdf_path the quoted path to the pdf file to read
#' @param page_min the page to begin reading, inclusive
#' @param page_max the page to end reading, inclusive
#' @param nbins the number of histogram bins
#' @return a histogram graph
#' @examples
#' \dontrun{
#' reveal_pdf_cols("/root/docs/my_cooking_pdf.pdf", 1, 3)
#' pdf_hist <- reveal_pdf_cols("/root/docs/my_cooking_pdf.pdf", 1, 3, nbins = 50)
#' }
#' @export
#' @import dplyr
#' @import pdftools
#' @import stringr
#' @import purrr
#' @importFrom magrittr %>%
#' @import plotly
#' @importFrom graphics text


reveal_pdf_cols <- function(pdf_path, page_min, page_max, nbins = 100) {

  import_xy <- function(page, ...) {


    # Data work ----

    ## subset pdf imported from pdftools
    data <- pdf[[page]]

    key_cols <- data %>%
      dplyr::select(x, y)


    return(key_cols)

  } # end import_xy



  ## Import the PDF with pdftools ----
  ## This happens first
  pdf <- pdftools::pdf_data(pdf_path)


  ## perform the function call with purrr ----
  ## This happens second
  raw <- purrr::pmap(list(page_min:page_max), import_xy)
  raw <- base::do.call(rbind, raw)


  # Graph ----
  fig <- plotly::plot_ly(
    data = raw,
    x = ~x,
    type = "histogram",
    nbinsx = nbins
  ) %>%
    plotly::layout(
      title = list(
        text = "Frequency of elements on the x-axis"
      ),
      xaxis = list(
        title = list(
          text = "x-axis position"
        )
      ),
      yaxis = list(
        title = list(
          text = "Frequency"
        )
      )
    )


  return(fig)

}
