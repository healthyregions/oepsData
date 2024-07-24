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
#' @param states String or vector of strings specifying which states to pull data for.
#' Should be FIPS codes.
#' @param counties String or vector of strings specifying which counties to pull data for.
#' Should be in FIPS codes, and is invalid if scale is states.
#' @param tidy Boolean specifying whether to return data in tidy format. Defaults False.
#' @param geometry Boolean specifying whether to pull geometries for the dataset
#'   TRUE or not FALSE. Defaults FALSE.
#'
#' @returns If geometry is FALSE, a tibble containing the requested variables. 
#' If geometry is TRUE, returns a simple feature collection.
#' 
#' @seealso [load_oeps_dictionary()], which loads a basic data dictionary
#'   showing viable datasets.
#' @examples
#' tracts_2010 <- load_oeps("tract", 2010, geometry = TRUE)
#' county_latest <- load_oeps("county", "latest", themes = "social")
#'
#' # themes can also be a list of options
#' zcta_latest <- load_oeps("zcta", "latest", c("composite", "economic"))
#' 
#' @import sf
#' @export
load_oeps <- function(scale, year, themes = "All", states=NULL, counties=NULL, 
                      tidy=FALSE, geometry=FALSE) {
  # TODO: implement cacheing
  # TODO: implement cartographic boundaries
  # TODO: determine what is needed to migrate to BQ
  
  valid_themes <- append(unique(data_dictionary[["Theme"]]), "all")
  valid_themes <- paste(valid_themes, collapse="|")
  
  stopifnot(
    grepl("state|tract|county|zcta|counties", scale, ignore.case = T),
    grepl("1980|1990|2000|2010|latest", year, ignore.case = T),
    all(grepl(valid_themes[[1]], themes, ignore.case = T)),
    xor(!is.null(counties), grepl("state", scale, ignore.case=T))
  )

  scale <- standardize_scale(scale)

  attribute_data <- eval(parse(text = make_object_name(scale, year)))
  attribute_data <- filter_by_themes(themes)

  if (tidy) attribute_data <- tidify_data(attribute_data)
  if (!is.null(states)) attribute_data <- filter_by_state(attribute_data, states)
  if (!is.null(counties)) attribute_data <- filter_by_county(attribute_data, counties)
  
  if (!geometry) {
    return(attribute_data)
  }

  geometry <- retrieve_geometry(scale, quiet = TRUE)

  data <- merge(attribute_data, geometry, on = "HEROP_ID", how = "left")

  return(sf::st_sf(data))
}

#' Filter by theme
#'
#' Takes user defined list of themes and references data dictionary to
#' filter down to only variables within that list as well as geography variables.
#' 
#' @param attribute_data data.frame containing the attribute data to filter on.
#' @param themes List of themes to filter out.
#'
#' @returns Filtered data.frame.
filter_by_themes <- function(attribute_data, themes) {
  if (any(grepl("all", themes, ignore.case = TRUE))) {
    return(attribute_data)
  }
  
  is_correct_theme <- grepl(paste(themes, collapse = "|"), 
                            data_dictionary$Theme,
                            ignore.case = TRUE
                            )
  selected_variables <- data_dictionary[is_correct_theme,][["Variable"]]
  
  return_variables <- append(selected_variables, merge_keys())
  variable_subset <- names(attribute_data)[names(attribute_data) %in% return_variables]

  if (all(variable_subset %in% merge_keys())) {
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
