#' Create new config.yml
#' @param path provide a path for the
#' @param projectName the name of the project
#' @param analysisCohorts the name for the analysis cohort table
#' @param diagnosticsCohorts the name for the diagnostics cohort table
#' @export
newConfig <- function(path,
                      projectName,
                      analysisCohorts,
                      diagnosticsCohorts) {
  #set config path
  configFile <- fs::path(path, "config.yml")

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
  readr::write_lines(config_block_txt, file = configFile)
  msg <- paste("Creating a new config.yml file at:", crayon::cyan(configFile))
  cli::cat_bullet(msg, bullet_col = "green", bullet = "tick")
  msg <- paste("Add default info to config")
  cli::cat_bullet(msg, bullet_col = "green", bullet = "tick")
  cli::cat_line("\tprojectName ", crayon::cyan(projectName))
  cli::cat_line("\tanalysisCohorts: ", crayon::cyan(analysisCohorts))
  cli::cat_line("\tdiagnosticsCohorts: ", crayon::cyan(diagnosticsCohorts))

  invisible(configFile)
}

#' Add config Block
#' @param configBlock the configuration block in the config.yml file
#' @param databaseName a character string of the database name for the config block. Default
#' to same name as config block
#' @param config_file the config.yml file to add a configBlock. Default to config.yml in project
#' directory
#' @export
addConfigBlock <- function(configBlock,
                           databaseName = configBlock,
                           configFile = here::here("config.yml")) {

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

  readr::write_lines(config_block_txt, file = configFile, append = TRUE)
  cli::cat_bullet("Added configuration block for ", crayon::magenta(configBlock), " in config.yml",
                  bullet = "tick", bullet_col = "green")
}


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


create_config_file <- function(projectSpecs) {
  cli::cat_bullet("Step 4: Initialize config.yml in Project",
                  bullet_col = "green", bullet = "info")

  dir_path <- fs::path(projectSpecs$location, projectSpecs$projectName)

  newConfig(path = dir_path,
            projectName = projectSpecs$projectName,
            analysisCohorts = projectSpecs$cohortTableNames$analysisCohorts,
            diagnosticsCohorts = projectSpecs$cohortTableNames$diagnosticsCohorts)

  configFile <- fs::path(dir_path, "config.yml")

  invisible(configFile)
}


readlines <- function(ll) {
  purrr::map(ll, ~readline(prompt = .x))
}


add_config_blocks <- function(projectSpecs){

  cli::cat_bullet("Step 5: Add Config Blocks to config.yml",
                  bullet_col = "green", bullet = "info")

  config_file <- fs::path(projectSpecs$location,
                          projectSpecs$projectName,
                          "config.yml")

  if (length(projectSpecs$configBlocks) > 0) {
    cb <- projectSpecs$configBlocks
  } else{
    nn <- as.integer(readline(prompt = "How many config blocks: "))
    cbPrompt <- purrr::map(
      seq_len(nn),
      ~paste("Enter Config Block", .x, "Name: ")
    )
    cb <- readlines(cbPrompt)
  }
  #check if credentials set
  check <- purrr::map_lgl(cb, ~are_credentials_set(.x))
  if (length(check) == 0) {
    stop("No configBlocks were entered")
  } else {
    if (all(check)) {
      #add config blocks
      purrr::walk(cb, ~addConfigBlock(.x, configFile = configFile))
    } else{
      cb_set <- cb[!check]
      for (i in seq_along(cb_set)) {
        cli::cat_bullet("Set credentials for", crayon::cyan(cb_set[[i]]))
        setCredentials(cb_set[[i]])
      }
      purrr::walk(cb, ~addConfigBlock(.x, configFile = configFile))
    }
  }
  invisible(cb)
}
