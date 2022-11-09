#' Create ohdsi project
#'
#' @param path Folder where the new project should be created
#'
#' @export
#' @importFrom magrittr %>%
ohdsi_project <- function(path) {
  path <- path.expand(path)
  dir.create(path, recursive = TRUE, showWarnings = FALSE)

  # copy files from inst to initialized directory
  from <- list.files(system.file("ohdsi_study", package = "picard", mustWork = TRUE),
                     full.names = TRUE, recursive = FALSE, include.dirs = TRUE)

  r <- file.copy(from = from, to = path, recursive = TRUE, copy.mode = FALSE)

  invisible(r)
}
