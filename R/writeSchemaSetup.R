setupWriteSchema <- function(configBlock) {

  ParallelLogger::logInfo("Initializing Cohort Tables for Diagnostics")
  initializeCohortTables(configBlock = configBlock, type = "diagnostics")

  ParallelLogger::logInfo("Initializing Cohort Tables for Analysis")
  initializeCohortTables(configBlock = configBlock, type = "diagnostics")

}


#' Initialize cohort table
#' @param configBlock the header of the configuration block that needs to be
#' set as the active configuration
#' @param type the type of cohort tables to use either the tables for the analysis
#' or the tables for cohort diagnostics
#' @export
initializeCohortTables <- function(configBlock, type = c("analysis", "diagnostics")) {

  type <- checkmate::matchArg(type, c("analysis", "diagnostics")) %>%
    switch(analysis = "analysisCohorts",
           diagnostics = "diagnosticsCohorts")
  name <- config::get(type)
  connectionDetails <- config::get("connectionDetails", config = configBlock)
  write_schema <- config::get("write", config = configBlock)

  cohortTableNames <- list(cohortTable = name,
                           cohortInclusionTable = paste0(name, "_inclusion"),
                           cohortInclusionResultTable = paste0(name, "_inclusion_result"),
                           cohortInclusionStatsTable = paste0(name, "_inclusion_stats"),
                           cohortSummaryStatsTable = paste0(name, "_summary_stats"),
                           cohortCensorStatsTable = paste0(name, "_censor_stats"))

  CohortGenerator::createCohortTables(connectionDetails = connectionDetails,
                                      cohortDatabaseSchema = write_schema,
                                      cohortTableNames = cohortTableNames,
                                      incremental = TRUE)

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
                         tempEmulationSchema = NULL) %>%
    DatabaseConnector::executeSql(conn, .)
  invisible(connectionDetails)
}



#' Drop cohort tables to refresh analysis for testing
#'
#' @param configBlock the header of the configuration block that needs to be
#' set as the active configuration
#' @param type the type of cohort tables to use either the tables for the analysis
#' or the tables for cohort diagnostics
#' @export
dropCohortTables <- function(configBlock, type = c("analysis", "diagnostics")) {
  type <- checkmate::matchArg(type, c("analysis", "diagnostics")) %>%
    switch(analysis = "analysisCohorts",
           diagnostics = "diagnosticsCohorts")
  connectionDetails <- config::get("connectionDetails", config = configBlock)
  write_schema <- config::get("write", config = configBlock)

  cohortTableNames <- list(cohortTable = name,
                           cohortInclusionTable = paste0(name, "_inclusion"),
                           cohortInclusionResultTable = paste0(name, "_inclusion_result"),
                           cohortInclusionStatsTable = paste0(name, "_inclusion_stats"),
                           cohortSummaryStatsTable = paste0(name, "_summary_stats"),
                           cohortCensorStatsTable = paste0(name, "_censor_stats"))

  purrr::walk(cohortTableNames, ~drop_tbl(connectionDetails = connectionDetails,
                                          write_schema = write_schema,
                                          table_name =  .x))
}

#' Create Write Schema
#' @param configBlock the header of the configuration block that needs to be
#' set as the active configuration
#' @export
createWriteSchema <- function(configBlock) {

  connectionDetails <- config::get("connectionDetails", config = configBlock)
  write_schema <- config::get("write", config = configBlock)

  #set up database connection
  conn <- DatabaseConnector::connect(connectionDetails = connectionDetails)
  on.exit(DatabaseConnector::disconnect(conn))

  sql <- "CREATE SCHEMA IF NOT EXISTS @write_schema AUTHORIZATION @user;" %>%
    SqlRender::render(write_schema = write_schema,
                      user = connectionDetails$user()) %>%
    SqlRender::translate(targetDialect = connectionDetails$dbms,
                         tempEmulationSchema = NULL) %>%
    DatabaseConnector::executeSql(conn, .)
  invisible(configBlock)

}

#' Drop Write Schema
#' @param configBlock the header of the configuration block that needs to be
#' set as the active configuration
#' @export
dropWriteSchema <- function(configBlock) {

  connectionDetails <- config::get("connectionDetails", config = configBlock)
  write_schema <- config::get("write", config = configBlock)

  #set up database connection
  conn <- DatabaseConnector::connect(connectionDetails = connectionDetails)
  on.exit(DatabaseConnector::disconnect(conn))

  sql <- "DROP SCHEMA IF EXISTS @write_schema CASCASE;" %>%
    SqlRender::render(write_schema = write_schema) %>%
    SqlRender::translate(targetDialect = connectionDetails$dbms,
                         tempEmulationSchema = NULL) %>%
    DatabaseConnector::executeSql(conn, .)
  invisible(configBlock)

}
