#' Load OEPS data
#'
#' `load_oeps` packages Opioid Environment Policy Scan data with given geographic
#' scale, year, and themes. Can also pull geometries.
#'
#' @param scale String specifying which geographic scale to pull variables for.
#'   Must be one of "tract", "zcta", "county", or "state".
#' @param year String or integer specifying which release year to pull variables for.
#'   Must be one of "1980", "1990", "2000", "2010", "2018, or "Latest". "2018"
#'   and "Latest" return the same data. 
#' @param themes String or list of strings specifying which Opioid Risk Environment
#'   themes to pull variables for. Valid themes are "Geography", "Social", "Environment",
#'   "Economic", "Outcome", "Policy", "Composite", or "All". Defaults to "All".
#' @param states String or vector of strings specifying which states to pull data for.
#' Can be FIPS codes, state names, or state abbreviations. Cannot be a vector if 
#' counties is provided. Ignored when scale is ZCTA.
#' @param counties String or vector of strings specifying which counties to pull data for.
#' Can be FIPS codes or county name, and is invalid if scale is states, or if no or multiple
#' states are specified. Ignored when scale is ZCTA.
#' @param tidy Boolean specifying whether to return data in tidy format. Defaults False.
#' @param geometry Boolean specifying whether to pull geometries for the dataset
#'   TRUE or not FALSE. Defaults FALSE.
#' @param cache Boolean specifying whether to use/create cached geometries and
#' attribute data (TRUE) or to pull fresh geometry and attribute data (FALSE).
#' To update cached geometries and data, call [clear_cache()] followed by
#' one of [cache_geometries()], or [cache_oeps_tables()].
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
                      tidy=FALSE, geometry=FALSE, cache=TRUE) {
  
  valid_themes <- append(unique(data_dictionary[["Theme"]]), "all")
  valid_themes <- paste(valid_themes, collapse="|")

  stopifnot(
    grepl("state|tract|county|zcta|counties", scale, ignore.case = T),
    grepl("1980|1990|2000|2010|2018|latest", year, ignore.case = T),
    all(grepl(valid_themes[[1]], themes, ignore.case = T)),
    (is.null(counties) & is.null(states)) | xor(!is.null(counties), grepl("state", scale, ignore.case=T))
  )
  
  scale <- standardize_scale(scale)
  attribute_data <- get_attribute_table(scale, year, cache)
  attribute_data <- filter_by_themes(attribute_data, themes)
  
  if (!(is.null(counties) & is.null(states)) & scale == 'zcta') {
    message('ZCTA scale is not compatible with counties, states parameters; returning
          nationwide results.')
  }
  
  if (scale != "zcta") attribute_data <- filter_by_geography(attribute_data, states, counties)
  
  if (tidy) attribute_data <- tidify_data(attribute_data)

  if (!geometry) return(attribute_data)
  
  geometry <- retrieve_geometry(scale, quiet = TRUE)
  data <- merge(attribute_data, geometry, on = "HEROP_ID", how = "left")
  return(sf::st_sf(data))
}

#' Cache OEPS data tables.
#'
#' Retrieve most recent version of all OEPS attribute data tables and save them
#' to cache. To cache geometries, see [cache_geometries()]. To clear cached
#' data, see [clear_cache()]
#'
#' @return None
#'
#' @export
cache_oeps_tables <- function() {

  scales <- c('state', 'tract', 'county')
  years <- c('1980', '1990', '2000', '2010', 'latest')
  needed <- expand.grid(scale=scales, year=years)

  # zcta only exists at one time period
  needed <- rbind(needed, data.frame(scale='zcta', year='latest'))

  mapply(get_attribute_table, needed$scale, needed$year, cache=TRUE)

  return(invisible(NULL))
}

#' Download attribute data
#'
#' Reads in attribute data tables from GitHub given the desired scale and year.
#'
#' @param scale Same as in `load_oeps`.
#' @param year Same as in `load_oeps`.
#' @param cache whether to cache newly retrieved data and/or check for previously
#' cached data.
#'
#' @returns OEPS data table containing data with the requested scale and year.
#'
#' @import data.table
get_attribute_table <- function(scale, year, cache=FALSE) {
  BASE_URL <- "https://raw.githubusercontent.com/GeoDaCenter/opioid-policy-scan/main/data_final/full_tables/"
  
  if (grepl(year, "2018")) {
    year <- 'latest'
  }
  
  table_name <- paste0(make_object_name(scale, year), '.csv')

  can_retrieve_from_cache <- cache & file.exists(file.path(cache_dir(), table_name))
  if (can_retrieve_from_cache) {
    attribute_table <- data.frame(data.table::fread(file.path(cache_dir(), table_name)))
    return (attribute_table)
  }

  url <- paste0(BASE_URL, '/', table_name)
  attribute_table <- data.frame(data.table::fread(url))

  if (cache) {
    cache_file(attribute_table, table_name)
  }
  return(attribute_table)
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
    warning("No OEPS variables satisfy specified scale, year, and theme combination.")
  }

  return(attribute_data[variable_subset])
}

# Create OEPS data object name
make_object_name <- function(scale, year) {
  scale <- toupper(substr(scale, 1, 1))
  year <- paste0(toupper(substr(year, 1, 1)), substr(year, 2, 7))
  return(paste(scale, year, sep = "_"))
}
