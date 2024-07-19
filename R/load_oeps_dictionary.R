#' Read OEPS data documentation
#'
#' `oeps_dictionary` returns the data dictionary for OEPS data at a given
#' geographic scale.
#'
#' @param scale String specifying which geographic scale to pull variables for.
#'   Must be one of "tract", "zcta", "county", or "state".
#'
#' @returns A tibble containing the data dictionary.
#'
#' @examples
#' tract_dictionary <- load_oeps_dictionary("tract")
#' View(tract_dictionary) # for optimal viewing experience
#'
#' @export
load_oeps_dictionary <- function(scale) {
  stopifnot(grepl("state|county|counties|tract|zcta", scale, ignore.case = TRUE))

  if (grepl("state", scale)) {
    return_data <- data_dictionary |>
      filter(scale == "state") |>
      select(-scale)
  }

  if (grepl("county|counties", scale)) {
    return_data <- data_dictionary |>
      filter(scale == "county") |>
      select(-scale)
  }

  if (grepl("tract", scale)) {
    return_data <- data_dictionary |>
      filter(scale == "tract") |>
      select(-scale)
  }

  if (grepl("zcta", scale)) {
    return_data <- data_dictionary |>
      filter(scale == "zcta") |>
      select(-scale)
  }

  return(return_data)
}
