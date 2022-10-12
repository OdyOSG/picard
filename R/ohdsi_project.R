#' Create boris project
#'
#'
#' @param path Folder where the new project should be created
#'
#' @export
#' @importFrom magrittr %>%
ohdsi_study_project <- function(path) {
  path <- path.expand(path)
  usethis::create_project(path, rstudio = TRUE)

  # copy files from example except .Rproj file and _targets directory
  from <- list.files(system.file("ohdsi_study", package = "picard", mustWork = TRUE),
                     full.names = TRUE, recursive = FALSE, include.dirs = TRUE)

  r <- file.copy(from = from, to = path, recursive = TRUE, copy.mode = FALSE)

  # if (file.exists(file.path(path, ".gitignore"))) {
  #   readr::write_lines("config.yml", file.path(path, ".gitignore"), append = TRUE)
  # }
  invisible(r)
}
