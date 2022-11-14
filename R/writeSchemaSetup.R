
#' Initialize cohort table
#'
#' @export
initialize_cohort_tables <- function(database) {
  
  name <- config::get("cohortTableName")
  connectionDetails <- config::get("connectionDetails", config = database)
  write_schema <- config::get("write", config = database)
  
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
#' @param database the database id corresponding to the config.yml file
#'
#' @export
drop_cohort_tables <- function(database) {
  name <- config::get("cohortTableName")
  connectionDetails <- config::get("connectionDetails", config = database)
  write_schema <- config::get("write", config = database)
  
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