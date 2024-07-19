#' Standardize scale
#'
#' Take user defined scale and try to map it to one of a selection of possible 
#' inputs.
standardize_scale <- function(scale) {
  if (grepl("state", scale, ignore.case = TRUE)) {
    return("state")
  }
  
  if (grepl("tract", scale, ignore.case = TRUE)) {
    return("tract")
  }
  
  if (grepl("county|counties", scale, ignore.case = TRUE)) {
    return("county")
  }
  
  if (grepl("zcta", scale, ignore.case = TRUE)) {
    return("zcta")
  }
}

#' Retrieve OEPS geometries
retrieve_geometry <- function(scale, quiet = FALSE) {
  shape_url <- "/vsicurl/https://raw.githubusercontent.com/GeoDaCenter/opioid-policy-scan/main/data_final/geometryFiles/"
  
  if (scale == "state") {
    shape_url <- paste0(shape_url, "state/states2010.shp")
  }
  
  if (scale == "tract") {
    shape_url <- paste0(shape_url, "tract/tracts2010.shp")
  }
  
  if (scale == "county") {
    shape_url <- paste0(shape_url, "county/counties2010.shp")
  }
  
  if (scale == "zcta") {
    shape_url <- paste0(shape_url, "zcta/zctas2010.shp")
  }
  
  return(sf::st_read(shape_url, quiet = quiet))
}

#' Tidify non-merge key variables
#'
#' Convert dataframe from wide to tidy format, ignoring merge keys.
tidify_data <- function(df) {
  included_keys <- names(df)[names(df) %in% oepsData:::merge_keys()]
  observation_variables <- names(df)[!names(df) %in% included_keys]
  
  tidy_df <- cbind(df[included_keys], stack(df[observation_variables]))
  tidy_df <- tidy_df[c("HEROP_ID", "GEOID", "ind", "values")]
  names(tidy_df) <- c("HEROP_ID", "GEOID", "variable", "value") 
  
  return(tidy_df)
}

#' Merge keys
#'
#' Returns the list of merge keys used by all data within this package.
#' 
#' @import sf
#' @returns A list of strings (the names of the merge key columns).
merge_keys <- function() {   merge_keys <- c("HEROP_ID", "GEOID", "TRACTCE", "STATEFP", "COUNTYFP") }
