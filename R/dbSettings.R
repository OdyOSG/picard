#' Retrieve execution settings from config.yml file
#'
#' @param database the database id corresponding to the config.yml file
#'
#' @export
get_execution_settings <- function(database) {

  executionSettings <- structure(list(
    'connectionDetails' = config::get("connectionDetails", config = database),
    'cdm_schema' = config::get("cdm", config = database),
    'vocabulary_schema' = config::get("vocab", config = database),
    'write_schema' = config::get("write", config = database),
    'cohort_table' = config::get("cohortTableName"),
    'databaseId' = config::get("databaseName", config = database),
    'studyName' = config::get("studyName")
  ), class = "executionSettings")

  return(executionSettings)

}
