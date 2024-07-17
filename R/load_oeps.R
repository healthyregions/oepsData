#' Load OEPS data 
#' 
#' `load_oeps` packages opioid environment policy scan data with given geographic
#' scale, year, and themes. Can also pull geometries.
#' 
#' @param scale String specifying which geographic scale to pull variables for. 
#'   Must be one of "tract", "zcta", "county", or "state".
#' @param year String or integer specifying which year to pull variables for. 
#'   Must be one of "1980", "1990", "2000", "2010", or "Latest".
#' @param themes String or list of strings specifiying which Opioid Risk Environment 
#'   themes to pull variables for. Valid themes are "Geography", "Social", "Environment", 
#'   "Economic", "Outcome", "Policy", "Composite", or "All". Defaults to "All".
#' @param geometry Boolean specifying whether to pull geometries for the dataset 
#'   (TRUE) or not (FALSE). Defaults FALSE.
#' 
#' @returns A tibble containing the requested variables. Always exports merge keys.
#' 
#' @seealso [load_oeps_dictionary()], which loads a basic data dictionary 
#'   showing viable datasets.
#' @examples
#' load_oeps('tract', 2010, geometry=TRUE)
#' load_oeps('county', 'latest', themes='social')
#' 
#' # themes can also be a list of options
#' load_oeps('zcta', 'latest', c('composite', 'economic'))
#' 
#' @export 
load_oeps <- function(scale, year, themes='All', geometry=FALSE) {
  
  valid_themes <- data_dictionary[['Theme']] |> 
    unique() |>
    append('all') |> 
    reduce(paste, sep='|')
  
  stopifnot(
    grepl('state|tract|county|zcta|counties', scale, ignore.case=T),
    grepl('1980|1990|2000|2010|latest', year, ignore.case=T),
    all(grepl(valid_themes[[1]], themes, ignore.case=T))
  )
  
  scale <- standardize_scale(scale)
  
  attribute_data <- eval(parse(text=make_file_name(scale, year)))
  attribute_data <- filter_by_themes(attribute_data, scale, themes)
  
  if (ncol(attribute_data) == 0) {
    warning('No variables satisfy specified scale, year, and theme combination.')
  }
  
  if (!geometry) {
    return(attribute_data)
  }
  
  geometry <- retrieve_geometry(scale, quiet=TRUE)
  
  data <- merge(attribute_data, geometry, on='HEROP_ID', how='left')
  
  return(data)
}


standardize_scale <- function(scale) {
  if (grepl('state', scale, ignore.case=TRUE)) {
    return('state')
  }
  
  if (grepl('tract', scale, ignore.case=TRUE)) {
    return('tract')
  }
  
  if (grepl('county|counties', scale, ignore.case=TRUE)) {
    return('county')
  }
  
  if (grepl('zcta', scale, ignore.case=TRUE)) {
    return('zcta')
  }
}


filter_by_themes <- function(attribute_data, scale, themes) {
  
  if (any(grepl('all', themes, ignore.case=TRUE))) {
    return(attribute_data)
  }
  
  # always grab geography variables, which are merge keys
  if (!any(grepl('geography', themes, ignore.case=TRUE))) {
    themes <- append(themes, 'geography')  
  }

  variable_has_theme <- grepl(paste(themes, collapse="|"), data_dictionary$Theme, ignore.case=TRUE)
  variable_has_scale <- data_dictionary$geometry == scale
  themes_key <- data_dictionary[variable_has_scale & variable_has_theme,]
  
  variable_subset <- names(attribute_data)[names(attribute_data) %in% themes_key[['Variable']]]

  return(attribute_data[variable_subset])
}


make_file_name <- function(scale, year) {
  scale <- toupper(substr(scale, 1, 1))
  year <- paste0(toupper(substr(year, 1, 1)), substr(year, 2, 7))
  return(paste(scale, year, sep="_"))
}


retrieve_geometry <- function(scale, quiet=FALSE) {
  shape_url <- '/vsicurl/https://raw.githubusercontent.com/GeoDaCenter/opioid-policy-scan/main/data_final/geometryFiles/'

  if (scale == 'state') {
    shape_url <- paste0(shape_url, 'state/states2010.shp')
  }
  
  if ('tract' == scale) {
    shape_url <- paste0(shape_url, 'tract/tracts2010.shp')
  }
  
  if ('county' == scale) {
    shape_url <- paste0(shape_url, 'county/counties2010.shp')
  }
  
  if ('zcta' == scale) {
    shape_url <- paste0(shape_url, 'zcta/zctas2010.shp')
  }
  
  return(st_read(shape_url, quiet=quiet))
}