#' Initialize cohort table
#' @param configBlock the configuration block in the config.yml file
#' @param type choose type of tables to initialize either analysis or diagnostics
#' set as the active configuration
#' @export
initializeCohortTables <- function(configBlock,
                                   type = c("analysis", "diagnostics"),
                                   configFile = here::here("config.yml")) {

  type <- checkmate::matchArg(type, c("analysis", "diagnostics")) %>%
    switch(analysis = "analysisCohorts",
           diagnostics = "diagnosticsCohorts")

  name <- config::get(type,
                      config = configBlock,
                      file = configFile)

  connectionDetails <- config::get("connectionDetails",
                                   config = configBlock,
                                   file = configFile)

  write_schema <- config::get("write",
                              config = configBlock,
                              file = configFile)

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
                         tempEmulationSchema = NULL)
  DatabaseConnector::executeSql(conn, sql)
  invisible(connectionDetails)
}

#' Drop cohort tables
#' @param configBlock the configuration block in the config.yml file
#' @param type choose type of tables to initialize either analysis or diagnostics
#' set as the active configuration
#' @export
dropCohortTables <- function(configBlock, type = c("analysis", "diagnostics"),
                             configFile = here::here("config.yml")) {

  type <- checkmate::matchArg(type, c("analysis", "diagnostics")) %>%
    switch(analysis = "analysisCohorts",
           diagnostics = "diagnosticsCohorts")

  name <- config::get(type,
                      config = configBlock,
                      file = configFile)

  connectionDetails <- config::get("connectionDetails",
                                   config = configBlock,
                                   file = configFile)

  write_schema <- config::get("write",
                              config = configBlock,
                              file = configFile)

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
#' @param configBlock the configuration block in the config.yml file
#' @export
createWriteSchema <- function(configBlock,
                              configFile = here::here("config.yml")) {

  connectionDetails <- config::get("connectionDetails",
                                   config = configBlock,
                                   file = configFile)
  write_schema <- config::get("write",
                              config = configBlock,
                              file = configFile)

  #set up database connection
  conn <- DatabaseConnector::connect(connectionDetails = connectionDetails)
  on.exit(DatabaseConnector::disconnect(conn))

  sql <- "CREATE SCHEMA IF NOT EXISTS @write_schema AUTHORIZATION @user;" %>%
    SqlRender::render(write_schema = write_schema,
                      user = connectionDetails$user()) %>%
    SqlRender::translate(targetDialect = connectionDetails$dbms,
                         tempEmulationSchema = NULL)
  DatabaseConnector::executeSql(conn, sql)
  invisible(configBlock)

}



#' Drop Write Schema
#' @param configBlock the configuration block in the config.yml file
#' @export
dropWriteSchema <- function(configBlock,
                            configFile = here::here("config.yml")) {

  connectionDetails <- config::get("connectionDetails",
                                   config = configBlock,
                                   file = configFile)
  write_schema <- config::get("write",
                              config = configBlock,
                              file = configFile)

  #set up database connection
  conn <- DatabaseConnector::connect(connectionDetails = connectionDetails)
  on.exit(DatabaseConnector::disconnect(conn))

  sql <- "DROP SCHEMA IF EXISTS @write_schema CASCASE;" %>%
    SqlRender::render(write_schema = write_schema) %>%
    SqlRender::translate(targetDialect = connectionDetails$dbms,
                         tempEmulationSchema = NULL)
  DatabaseConnector::executeSql(conn, sql)
  invisible(configBlock)

}
