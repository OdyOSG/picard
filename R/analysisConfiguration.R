
edit_config <- function() {
  usethis::ui_todo("Update the config.yml file to correct the schema names")
  path <- here::here("config.yml")
  usethis::edit_file(path)
  usethis::ui_todo("Restart R for changes to take effect")
  invisible(path)
}

set_password <- function(database, credential) {
  cred <- paste(database, credential, sep = "_")
  keyring::key_set(cred, prompt = paste(cred, ":"))
  usethis::ui_done("keyring set for {ui_field(cred)}")
}


#' Check schemas if they are correct
#'
#' @param database the database id corresponding to the config.yml file
#'
#' @export
#' @importFrom usethis ui_field ui_value
check_schemas <- function(database) {

  cdm <- config::get("cdm", config = database)
  usethis::ui_info("cdm schema for {ui_field(database)} is set as {ui_value(cdm)}")

  vocab <- config::get("cdm", config = database)
  usethis::ui_info("vocabulary schema for {ui_field(database)} is set as {ui_value(vocab)}")

  write_schema <- config::get("write", config = database)
  usethis::ui_info("write schema for {ui_field(database)} is set as {ui_value(write_schema)}")
  cli::cat_rule()

  qq <- usethis::ui_nope("Are these schema names correct?", n_no = 1)
  if (qq) {
    usethis::ui_todo("Update the config.yml file to correct the schema names")
    path <- here::here("config.yml")
    usethis::edit_file(path)
    usethis::ui_todo("Restart R for changes to take effect")
    invisible(path)
  } else{
    usethis::ui_done("Schema names checked")
  }

}


#' Check schemas if they are correct
#'
#' @param database the database id corresponding to the config.yml file
#' @param credential the credential to check for the database
#'
#' @export
#' @importFrom usethis ui_field ui_value
check_credential <- function(database,
                             credential = c("user", "password",
                                            "server", "port")) {

  credential <- match.arg(credential)

  # get connection from config
  connectionDetails <- config::get("connectionDetails", config = database)
  #safely eval connection credential
  cred <- purrr::safely(connectionDetails[[credential]])
  check <- cred()

  #check if there is an error
  if (is.null(check$result)) {
    usethis::ui_oops("{ui_value(credential)} not set in {ui_field(database)}")

    #ask to set credential
    # ask if this is correct
    qq <- usethis::ui_yeah("Do you want to set the keyring credential?", n_no = 1)
    #if not run set_password
    if (qq) {
      set_password(database, credential)
    } else {
      usethis::ui_todo("Please set {ui_value(credential)} for {ui_field(database)} using keyring or in config.yml file")
      cli::cat_rule()
    }

  } else{
    vv <- check$result
    usethis::ui_info("{ui_value(credential)} set to {ui_value(vv)}")

    # ask if this is correct
    qq <- usethis::ui_nope("Is this credential correct?", n_no = 1)
    #if not run set_password
    if (qq) {
      set_password(database, credential)
    } else {
      usethis::ui_done("{ui_value(credential)} correct in {ui_field(database)}")
      cli::cat_rule()
    }
  }
}

#' Setup the study and add credentials
#'
#' @param database the database id corresponding to the config.yml file
#' @importFrom usethis ui_field ui_value
#' @export
setup_study <- function(database) {

  check_db <- config::get("databaseName", config = database)
  if (is.null(check_db)) {
    usethis::ui_todo("Add configuration for {ui_field(database)} to config.yml")
    edit_config()
    invisible(database)
  }

  cli::cat_boxx("Check that Schemas are correct")
  check_schemas(database)

  cli::cat_boxx("Check that Credentials are correct")
  purrr::walk(c("user", "password", "server", "port"),
              ~check_credential(database, .x))

  cli::cat_boxx("Initialize Cohort Tables")
  initialize_cohort_tables(database)
}

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
