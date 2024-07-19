#' Load OEPS data
#'
#' `load_oeps` packages opioid environment policy scan data with given geographic
#' scale, year, and themes. Can also pull geometries.
#'
#' @param scale String specifying which geographic scale to pull variables for.
#'   Must be one of "tract", "zcta", "county", or "state".
#' @param year String or integer specifying which year to pull variables for.
#'   Must be one of "1980", "1990", "2000", "2010", or "Latest".
#' @param themes String or list of strings specifying which Opioid Risk Environment
#'   themes to pull variables for. Valid themes are "Geography", "Social", "Environment",
#'   "Economic", "Outcome", "Policy", "Composite", or "All". Defaults to "All".
#' @param tidy Boolean specifying whether to return data in tidy format. Defaults False.
#' @param geometry Boolean specifying whether to pull geometries for the dataset
#'   TRUE or not FALSE. Defaults FALSE.
#'
#' @returns A tibble containing the requested variables. Always exports merge keys.
#'
#' @seealso [load_oeps_dictionary()], which loads a basic data dictionary
#'   showing viable datasets.
#' @examples
#' load_oeps("tract", 2010, geometry = TRUE)
#' load_oeps("county", "latest", themes = "social")
#'
#' # themes can also be a list of options
#' load_oeps("zcta", "latest", c("composite", "economic"))
#'
#' @export
load_oeps <- function(scale, year, themes = "All", tidy=FALSE, geometry=FALSE) {

  valid_themes <- data_dictionary[["Theme"]] |>
    unique() |>
    append("all") |>
    paste(collapse = "|")

  stopifnot(
    grepl("state|tract|county|zcta|counties", scale, ignore.case = T),
    grepl("1980|1990|2000|2010|latest", year, ignore.case = T),
    all(grepl(valid_themes[[1]], themes, ignore.case = T))
  )

  scale <- oepsData:::standardize_scale(scale)

  attribute_data <- parse(text = make_object_name(scale, year)) |>
    eval() |>
    filter_by_themes(themes)
  
  attribute_data <- if (tidy) oepsData:::tidify_data(attribute_data) else attribute_data
  
  if (!geometry) {
    return(attribute_data)
  }

  geometry <- oepsData:::retrieve_geometry(scale, quiet = TRUE)

  data <- merge(attribute_data, geometry, on = "HEROP_ID", how = "left")

  return(data)
}

# Filter by theme
#
# Takes user defined list of themes and references data dictionary to
# filter down to only variables within that list as well as geography variables.
filter_by_themes <- function(attribute_data, themes) {
  if (any(grepl("all", themes, ignore.case = TRUE))) {
    return(attribute_data)
  }
  
  is_correct_theme <- grepl(paste(themes, collapse = "|"), data_dictionary$Theme,
    ignore.case = TRUE
  )
  selected_variables <- data_dictionary[is_correct_theme,][["Variable"]]
  
  return_variables <- append(selected_variables, oepsData:::merge_keys())
  variable_subset <- names(attribute_data)[names(attribute_data) %in% return_variables]

  if (all(variable_subset %in% oepsData:::merge_keys())) {
    warning("No variables satisfy specified scale, year, and theme combination.")
  }
  
  return(attribute_data[variable_subset])
}

# Create OEPS data object name
make_object_name <- function(scale, year) {
  scale <- toupper(substr(scale, 1, 1))
  year <- paste0(toupper(substr(year, 1, 1)), substr(year, 2, 7))
  return(paste(scale, year, sep = "_"))
}
