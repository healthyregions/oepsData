# oepsData

An R package for easy access to the Opioid Environment Policy Scan (OEPS) datasets.

- Documentation and vignettes here: [oepsData.healthyregions.org](https://oepsData.healthyregions.org)
  - Documentation is built with [bookdown](https://bookdown.org/) and lives in the `bookdown` branch of this repository ([healthyregions/oepsData](https://github.com/healthyregions/oepsData))
- Core repo for OEPS data management: [healthyregions/oeps](https://github.com/healthyregions/oeps)
- OEPS Explorer: [oeps.healthyregions.org](https://oeps.healthyregions.org)

Please report bugs and leave feedback in [Github issues](https://github.com/healthyregions/oepsData/issues). Thanks!

## Development

Clone this repo and open in RStudio. Then the following commands can be run in the console:

- `devtools::load_all()` Load all functions into namespace without actually installing the package
- `devtools::install_github()` - Installs the package. Re-run this command to re-install.
- `devtools::document()` - Build docs.
- `devtools::check()` - Run checks across the package.
		- Note: `curl` is imported in the package, but never directly called, which produces a warning

### Managing data dictionaries

Data dictionaries are downloaded from Github and parsed with `data-raw/AddInternalData.R`. To update data dictionaries:

1. `source("data-raw/AddInternalData.R")`
  - If you get an error like `cannot unload 'dplyr'` while running this command, try closing and reopening RStudio.
2. `R.sysdata` will be altered -- commit these changes.

## Authors

Ashlynn Wimer (@bucketteOfIvy)
Adam Cox (@mradamcox)

## License

[CC BY 4.0](https://creativecommons.org/licenses/by/4.0/)
