library(picard)
library(testthat)

#build test project for testing
proj_root <- tempfile()
picard::ohdsi_project(proj_root)
usethis::create_project(proj_root, open = FALSE)

#add eunomia block for testing
configBlock <- "Eunomia"
config_file <- file.path(proj_root, "config.yml")
config_block_txt <- glue::glue(
  "\n\n# {configBlock} Credentials
{configBlock}:
  databaseName: {configBlock}
  connectionDetails: !expr Eunomia::getEunomiaConnectionDetails()
  cdm: main
  vocab: main
  write: main
  ")

readr::write_lines(config_block_txt, file = config_file, append = TRUE)

