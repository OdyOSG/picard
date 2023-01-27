#' Create ohdsi project
#'
#' @param path Folder where the new project should be created
#'
#' @export
ohdsi_project <- function(path) {
  path <- fs::path_expand_r(path) %>%
    fs::dir_create()
  template_dir <- fs::path_package("picard", "templates/ohdsi_study")
  from <- template_dir
  to <- path
  r <- file.copy(list.files(from, full.names = TRUE),
                             to,
                             recursive = TRUE)
  invisible(r)
}




