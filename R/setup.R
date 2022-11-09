#' Edit config file
#'
#' @export
#' @import usethis
edit_config <- function() {
  usethis::ui_todo("Edit config.yml file adding OMOP database")
  path <- here::here("config.yml")
  usethis::edit_file(path)
  usethis::ui_todo("Restart R for changes to take effect")
  invisible(path)
}

#' Set credentials
#'
#' @export
set_credentials <- function(database,
                         credentials = c('dbms', 'user', 'password', 'server',
                                         'port', 'cdm', 'vocab', 'write')) {
  cred <- paste(database, credentials, sep = "_")

  for (i in seq_along(cred)) {
    keyring::key_set(cred[i], prompt = paste(cred[i], ":"))
    ParallelLogger::logInfo("Keyring set for ", crayon::green(cred[i]))
  }
  invisible(cred)
}

#' Check credentials
#'
#' @export
check_credentials <- function(database,
                              credentials = c('dbms', 'user', 'password', 'server',
                                              'port', 'cdm', 'vocab', 'write')) {


  cred <- paste(database, credentials, sep = "_")

  for (i in seq_along(cred)) {
    tmp <- purrr::safely(keyring::key_get)
    if (is.null(tmp(cred[i]$error))) {
      ParallelLogger::logInfo("Keyring for ", crayon::green(cred[i]), " is: ",
                              crayon::blurred(tmp(cred[i]$result)))

      qq <- usethis::ui_nope("Are these credentials correct?", n_no = 1)

      if (qq) {

        ParallelLogger::logInfo("Run function ",
                                crayon::red("picard::set_credentials"),
                                " to update")

      } else{

        ParallelLogger::logInfo("Config.yml is ready to go!")

      }

    } else{
      ParallelLogger::logError("Error: Keyring not set for ", crayon::green(cred[i]))
    }
  }

  invisible(cred)
}




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
