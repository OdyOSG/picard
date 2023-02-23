is_credential_set <- function(configBlock, credential) {
  cred <- paste(configBlock, credential, sep = "_")
  tmp <- purrr::safely(keyring::key_get)(cred)
  if (is.null(tmp$error)) {
    TRUE
  } else{
    FALSE
  }
}

are_credentials_set <- function(configBlock) {
  creds <- c('dbms', 'user', 'password', 'server',
             'port', 'cdm', 'vocab', 'write')
  check <- purrr::map_lgl(creds, ~is_credential_set(configBlock = configBlock, credential = .x))
  if (length(check) > 0 & all(check)) {
    TRUE
  } else {
    FALSE
  }

}

#' Check credentials
#' @param configBlock the configuration block in the config.yml file
#' @export
checkCredentials <- function(configBlock) {
  creds <- c('dbms', 'user', 'password', 'server',
             'port', 'cdm', 'vocab', 'write')
  check <- purrr::map_lgl(creds, ~is_credential_set(configBlock = configBlock, credential = .x))

  if (length(check) > 0 & all(check)) {
    invisible(all(check))
  } else{
    pp <- creds[!check]
    cli::cat_bullet(
      "These credentials were not set for ", crayon::cyan(configBlock),
      bullet = "warning", bullet_col = "yellow"
    )
    cli::cat_line("\t", crayon::green(pp))
  }
}



show_credential <- function(configBlock, credential) {
  cred <- paste(configBlock, credential, sep = "_")
  res <- keyring::key_get(cred)
  cli::cat_line(crayon::cyan(cred), ": ", crayon::blurred(res))
}



#' Show credentials
#' @param configBlock the configuration block in the config.yml file
#' @export
showCredentials <- function(configBlock){
  creds <- c('dbms', 'user', 'password', 'server',
             'port', 'cdm', 'vocab', 'write')
  check <- purrr::quietly(checkCredentials)(configBlock)

  if (!is.null(check$result)) {
    purrr::walk(creds, ~show_credential(configBlock = configBlock, credential = .x))
  } else{
    cli::cat_bullet("Credentials were not set for ", crayon::cyan(configBlock),
                    bullet = "warning", bullet_col = "yellow")
  }
  invisible(check)
}

#' Set credential
#' @description sets a single credential for the config block
#' @param configBlock the configuration block in the config.yml file
#' @param credential a character string for a credential to set
#' @export
setCredential <- function(configBlock,
                          credential) {
  cred <- paste(configBlock, credential, sep = "_")
  keyring::key_set(cred, prompt = paste(cred, ":"))
  cli::cat_bullet("Keyring set for", crayon::cyan(cred))
  invisible(cred)
}

#' Set credentials
#' @description sets all credentials for the config block
#' @param configBlock the configuration block in the config.yml file
#' @export
setCredentials <- function(configBlock) {
  creds <- c('dbms', 'user', 'password', 'server',
             'port', 'cdm', 'vocab', 'write')

  for (i in seq_along(creds)) {
   setCredential(configBlock, credential = creds[i])
  }
  invisible(creds)
}

