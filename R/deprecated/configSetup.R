#' Edit config file
#'
#' @export
#' @import usethis
touchConfig <- function() {
  usethis::ui_todo("Edit config.yml file adding OMOP database")
  path <- here::here("config.yml")
  usethis::edit_file(path)
  usethis::ui_todo("Restart R for changes to take effect")
  invisible(path)
}


changeConfigDefault <- function(entry = c("studyName", "analysisCohorts", "diagnosticsCohorts"),
                                value,
                                config_file = here::here("config.yml")) {
  readr::read_file(config_file) %>%
    stringr::str_replace(glue::glue("{entry}:.*"), glue::glue("{entry}: {value}")) %>%
    readr::write_file(file = config_file)
  ParallelLogger::logInfo("Changed default variable ", entry, " to ", value, " in config.yml")
}

#' Change study name in config.yml
#' @param value the value to change for the study name
#' @param config_file the config.yml file to change
#' @export
changeStudyName <- function(value, config_file = here::here("config.yml")) {
  changeConfigDefault(entry = "studyName", value = value, config_file = config_file)
}

#' Change analysis cohort table name in config.yml
#' @param value the value to change for the analysis cohort table
#' @param config_file the config.yml file to change
#' @export
changeAnalysisCohorts <- function(value, config_file = here::here("config.yml")) {
  changeConfigDefault(entry = "analysisCohorts", value = value, config_file = config_file)
}

#' Change diagnostics cohort table name in config.yml
#' @param value the value to change for the diagnostics cohort table
#' @param config_file the config.yml file to change
#' @export
changeDiagnosticsCohorts <- function(value, config_file = here::here("config.yml")) {
  changeConfigDefault(entry = "diagnosticsCohorts", value = value, config_file = config_file)
}

#' Set credentials
#' @param configBlock the header of the configuration block that needs to be
#' set as the active configuration
#' @param databaseName a character string of the database name for the config block. Default
#' to same name as config block
#' @param config_file the config.yml file to add a configBlock. Default to config.yml in project
#' directory
#' @export
addConfigBlock <- function(configBlock,
                           databaseName = configBlock,
                           config_file = here::here("config.yml")) {

  config_block_txt <- glue::glue(
    "\n\n# {configBlock} Credentials
{configBlock}:
  databaseName: {databaseName}
  connectionDetails: !expr DatabaseConnector::createConnectionDetails(
    dbms = keyring::key_get('{configBlock}_dbms'),
    user = keyring::key_get('{configBlock}_user'),
    password = keyring::key_get('{configBlock}_password'),
    server = keyring::key_get('{configBlock}_server'),
    port = keyring::key_get('{configBlock}_port'))
  cdm: !expr keyring::key_get('{configBlock}_cdm')
  vocab: !expr keyring::key_get('{configBlock}_vocab')
  write: !expr keyring::key_get('{configBlock}_write')
  ")

  readr::write_lines(config_block_txt, file = config_file, append = TRUE)
  ParallelLogger::logInfo("Added configuration block for ", configBlock, " in config.yml")
}


newConfigBlock <- function(path,
                           projectName = "ohdsi_study",
                           analysisCohorts,
                           diagnosticsCohorts) {
  #set config path
  configFile <- fs::path(path, "config.yml")

  #create path
  configFile %>% fs::file_create()
  msg <- paste("Creating a new config.yml file at:", crayon::cyan(configFile))
  cli::cat_bullet(msg, bullet_col = "green", bullet = "tick")

  #add config text
  config_block_txt <- glue::glue(
"# Config File for {projectName}
\ndefault:
  projectName: {projectName}
  analysisCohorts: {analysisCohorts}
  diagnosticsCohorts: {diagnosticsCohorts}
"
  )
  #write lines to file
  readr::write_lines(config_block_txt, file = configFile, append = TRUE)
  msg <- paste("Add default info to config")
  cli::cat_bullet(msg, bullet_col = "green", bullet = "tick")
  cli::cat_line("\tprojectName ", crayon::cyan(projectName))
  cli::cat_line("\tanalysisCohorts: ", crayon::cyan(analysisCohorts))
  cli::cat_line("\tdiagnosticsCohorts: ", crayon::cyan(diagnosticsCohorts))

  invisible(configFile)
}

#' Set credentials
#' @param configBlock the header of the configuration block that needs to be
#' set as the active configuration
#' @param credentials a character vector of credentials to set
#' @export
setCredentials <- function(configBlock,
                         credentials = c('dbms', 'user', 'password', 'server',
                                         'port', 'cdm', 'vocab', 'write')) {
  cred <- paste(configBlock, credentials, sep = "_")

  for (i in seq_along(cred)) {
    keyring::key_set(cred[i], prompt = paste(cred[i], ":"))
    ParallelLogger::logInfo("Keyring set for ", crayon::green(cred[i]))
  }
  invisible(cred)
}

#' Check credentials
#' @param configBlock the header of the configuration block that needs to be
#' set as the active configuration
#' @param credentials a character vector of credentials to check
#' @export
checkCredentials <- function(configBlock,
                              credentials = c('dbms', 'user', 'password', 'server',
                                              'port', 'cdm', 'vocab', 'write')) {


  cred <- paste(configBlock, credentials, sep = "_")

  for (i in seq_along(cred)) {
    tmp <- purrr::safely(keyring::key_get)
    check <- tmp(cred[i])

    if (is.null(check$error)) {
      ParallelLogger::logInfo("Keyring for ", crayon::green(cred[i]), " is: ",
                              crayon::blurred(check$result))

    } else{
      ParallelLogger::logError("Error: Keyring not set for ", crayon::green(cred[i]))
    }
  }

  qq <- usethis::ui_nope("Are these credentials correct?", n_no = 1)

  if (qq) {

    ParallelLogger::logInfo("Run function ",
                            crayon::red("picard::set_credentials"),
                            " to update")

  } else{

    ParallelLogger::logInfo("Config.yml is ready to go!")

  }

  invisible(cred)
}


#' Import a config.yml file
#' @param path the path to the config file we want to import
#' @export
importConfigFile <- function(path) {

  path_check <- fs::path_expand_r(path) %>%
    fs::path_ext()
  checkmate::assert_character(path_check, pattern = "yml")

  cli::cat_bullet("Importing config.yml from: ", crayon::cyan(path), bullet = "tick", bullet_col = "green")
  fs::path_expand_r(path) %>%
    fs::file_copy(new_path = here::here(), overwrite = TRUE)
}

#' List config blocks to use
#' @export
listConfigBlocks <- function() {
  path <- here::here("config.yml")
  config_yml <- yaml::yaml.load_file("config.yml", eval.expr = TRUE)
  names(config_yml)[names(config_yml) != "default"]
}

