#' Get cohort table names
#' @param type choose type of tables to initialize either analysis or diagnostics
#' set as the active configuration
#' @param configFile the config.yml file to add a configBlock. Default to config.yml in project
#' directory
#' @export
cohortTableNames <- function(type,
                             configFile = here::here("config.yml")) {

  type <- checkmate::matchArg(type, c("analysis", "diagnostics")) %>%
    switch(analysis = "analysisCohorts",
           diagnostics = "diagnosticsCohorts")

  name <- config::get(type,
                      config = "default",
                      file = configFile)

  ll <- list(cohortTable = name,
       cohortInclusionTable = paste0(name, "_inclusion"),
       cohortInclusionResultTable = paste0(name, "_inclusion_result"),
       cohortInclusionStatsTable = paste0(name, "_inclusion_stats"),
       cohortSummaryStatsTable = paste0(name, "_summary_stats"),
       cohortCensorStatsTable = paste0(name, "_censor_stats"))
  return(ll)
}

#' Initialize cohort table
#' @param configBlock the configuration block in the config.yml file
#' @param type choose type of tables to initialize either analysis or diagnostics
#' set as the active configuration
#' @param configFile the config.yml file to add a configBlock. Default to config.yml in project
#' directory
#' @export
initializeCohortTables <- function(configBlock,
                                   type = c("analysis", "diagnostics"),
                                   configFile = here::here("config.yml")) {

  cohortTableNames <- cohortTableNames(type = type, configFile = configFile)


  connectionDetails <- config::get("connectionDetails",
                                   config = configBlock,
                                   file = configFile)

  write_schema <- config::get("write",
                              config = configBlock,
                              file = configFile)



  CohortGenerator::createCohortTables(connectionDetails = connectionDetails,
                                      cohortDatabaseSchema = write_schema,
                                      cohortTableNames = cohortTableNames,
                                      incremental = TRUE)
  invisible(cohortTableNames)

}

drop_tbl <- function(connectionDetails,
                     write_schema,
                     table_name) {

  #set up database connection
  conn <- DatabaseConnector::connect(connectionDetails = connectionDetails)
  on.exit(DatabaseConnector::disconnect(conn))

  sql <- "DROP TABLE IF EXISTS @write_schema.@table_name" %>%
    SqlRender::render(write_schema = write_schema,
                      table_name = table_name) %>%
    SqlRender::translate(targetDialect = connectionDetails$dbms,
                         tempEmulationSchema = NULL)
  DatabaseConnector::executeSql(conn, sql)
  invisible(connectionDetails)
}

#' Drop cohort tables
#' @param configBlock the configuration block in the config.yml file
#' @param type choose type of tables to initialize either analysis or diagnostics
#' set as the active configuration
#' @param configFile the config.yml file to add a configBlock. Default to config.yml in project
#' directory
#' @export
dropCohortTables <- function(configBlock, type = c("analysis", "diagnostics"),
                             configFile = here::here("config.yml")) {

  cohortTableNames <- cohortTableNames(type = type, configFile = configFile)

  connectionDetails <- config::get("connectionDetails",
                                   config = configBlock,
                                   file = configFile)

  write_schema <- config::get("write",
                              config = configBlock,
                              file = configFile)

  purrr::walk(cohortTableNames, ~drop_tbl(connectionDetails = connectionDetails,
                                          write_schema = write_schema,
                                          table_name =  .x))
}

initialize_cohort_tables <- function(projectSpecs) {
  cli::cat_bullet("Step 6: Initialize cohort tables",
                  bullet_col = "green", bullet = "info")
  configFile <- fs::path(projectSpecs$location, projectSpecs$projectName, "config.yml")
  cb <- listConfigBlocks(configFile)
  purrr::walk(cb, ~initializeCohortTables(.x, type = "analysis", configFile = configFile))
  if (projectSpecs$addDiagnostics) {
    purrr::walk(cb, ~initializeCohortTables(.x, type = "diagnostics", configFile = configFile))
  }
  invisible(cb)
}
