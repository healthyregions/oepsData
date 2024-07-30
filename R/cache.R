#' Get cache_dir
#' 
#' Returns the path to the cache directory in use by oepsData for the given
#' user as a character. 
#' 
#' @import rappdirs
#' @export
cache_dir <- function() { 
  return( rappdirs::user_cache_dir(appname="oepsData", appauthor='HEROP') ) 
}


#' Cache files
#'
#' Take a simple features, dataframe, or tibble object and save it to a cache
#' directory. 
#' 
#' @param obj The object to cache. Should be a simple features object, data.frame,
#' tibble, or matrix.
#' @param file_name String detailing desired file_name. If file type is included,
#' should be one of ".csv" or ".shp"
#' 
#' @returns None.
#' 
#' @seealso [clear_cache]
#' 
#' @import R.cache
#' @import sf
cache_file <- function(obj, file_name) {

  if (!dir.exists(cache_dir())) {
    dir.create(cache_dir(), recursive=T)
  }
  
  if (any(class(obj) == "sf")) {
    
    if (!grepl("\\.shp", file_name)) file_name <- paste0(file_name, ".shp")
    
    sf::st_write(obj, file.path(cache_dir(), file_name), quiet=TRUE)
  }
  
  if (!grepl("\\.csv", file_name)) file_name <- paste0(file_name, ".csv")
  
  write.csv(obj, file.path(cache_dir(), file_name))
}


#' Clear cache
#' 
#' Remove all files from the oepsData cache, and delete the cache file itself.
#' 
#' @returns 0 on a successful cache deletion and 1 on a failure. Deleting a
#' nonexistent cache counts as a success.
#' @export
clear_cache <- function() { return(unlink(cache_dir(), recursive = TRUE)) }