
#' Retrieve execution settings from config.yml file
#'
#' @param configBlock the configBlock id corresponding to the config.yml file
#' @param file the path to the config yml file
#' @param type eiter the analysis or diagnostics
#' @export
executionSettings <- function(configBlock,
                              file = here::here("config.yml"),
                              type = c("analysis", "diagnostics")) {

  # cd <- config::get("connectionDetails", config = configBlock, file = file)

  type <- checkmate::matchArg(type, c("analysis", "diagnostics")) %>%
    switch(analysis = "analysisCohorts",
           diagnostics = "diagnosticsCohorts")

  executionSettings <- structure(list(
    'connectionDetails' = config::get("connectionDetails", config = configBlock, file = file),
    'cdm' = NA_character_,
    'cdm_schema' = config::get("cdm", config = configBlock, file = file),
    'vocabulary_schema' = config::get("vocab", config = configBlock, file = file),
    'write_schema' = config::get("write", config = configBlock, file = file),
    'cohort_table' = config::get(type, file = file),
    'databaseId' = config::get("databaseName", config = configBlock, file = file),
    'studyName' = config::get("studyName", file = file)
  ), class = "executionSettings")


  return(executionSettings)

}


