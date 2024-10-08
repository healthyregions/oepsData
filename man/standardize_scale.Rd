% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/geometry.R
\name{standardize_scale}
\alias{standardize_scale}
\title{Standardize scale}
\usage{
standardize_scale(scale)
}
\arguments{
\item{scale}{String containing messy scale whose identity needs guessed.}
}
\value{
One of "state", "tract", "county", or "zcta".
}
\description{
Take user defined scale and try to map it to one of a selection of possible
inputs.
}
