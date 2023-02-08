get_driver_fn <- function(execution_settings) {
  #determine the correct driver either postgres or redshift
  driver <- execution_settings$connectionDetails$dbms
  drv <- switch(driver,
                postgresql = RPostgres::Postgres,
                redshift = RPostgres::Redshift)
  return(drv)
}

#' Retrieve execution settings from config.yml file
#'
#' @param configBlock the configBlock id corresponding to the config.yml file
#' @param file the path to the config yml file
#' @param connectToCdm toggle whether to connect to cdm
#'
#' @export
executionSettings <- function(configBlock,
                              file = here::here("config.yml"),
                              type = c("analysis", "diagnostics"),
                              connectToCdm = FALSE) {

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

  if (connectToCdm) {

    fn <- get_driver_fn(executionSettings)
    con <- DBI::dbConnect(
      fn(),
      host     = strsplit(executionSettings$connectionDetails$server(), "/")[[1]][[1]],
      dbname   = strsplit(executionSettings$connectionDetails$server(), "/")[[1]][[2]],
      user     = executionSettings$connectionDetails$user(),
      password = executionSettings$connectionDetails$password(),
      port     = executionSettings$connectionDetails$port()
    )

    executionSettings$cdm <- CDMConnector::cdm_from_con(con,
                                      cdm_schema = executionSettings$cdm_schema,
                                      cdm_tables = CDMConnector::tbl_group("default"),
                                      write_schema = executionSettings$write_schema,
                                      cohort_tables = executionSettings$cohort_table)
    }


  return(executionSettings)

}


