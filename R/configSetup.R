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
#' @param configBlock the header of the configuration block that needs to be
#' set as the active configuration
#' @param credentials a character vector of credentials to set
#' @export
set_credentials <- function(configBlock,
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
check_credentials <- function(configBlock,
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
import_config <- function(path) {

  path_check <- fs::path_expand_r(path) %>%
    fs::path_ext()
  checkmate::assert_character(path_check, pattern = "yml")

  ParallelLogger::logInfo("Importing config.yml from: ", crayon::green(path))
  fs::path_expand_r(path) %>%
    fs::file_copy(new_path = here::here(), overwrite = TRUE)
}

#' List config blocks to use
#' @export
list_config_blocks <- function() {
  path <- here::here("config.yml")
  config_yml <- yaml::yaml.load_file("config.yml", eval.expr = TRUE)
  names(config_yml)[names(config_yml) != "default"]
}
