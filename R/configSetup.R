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






