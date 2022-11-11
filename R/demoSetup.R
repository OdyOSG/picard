#' load demo cohorts
#' @export
load_boo_demo_cohorts <- function(){

  path <- here::here("input/cohorts_to_create/boo")
  dir.create(path, recursive = TRUE, showWarnings = FALSE)
  # copy files from inst to initialized directory
  from <- list.files(system.file("demo/cohorts", package = "picard", mustWork = TRUE),
                     full.names = TRUE, recursive = FALSE, include.dirs = TRUE)
  r <- file.copy(from = from, to = path, recursive = TRUE, copy.mode = FALSE)

  invisible(r)
}
