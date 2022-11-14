#' Create ohdsi project
#'
#' @param path Folder where the new project should be created
#'
#' @export
ohdsi_project <- function(path) {
  path <- fs::path_expand_r(path) %>%
    fs::dir_create()
  template_dir <- fs::path_package("picard", "templates/ohdsi_study")
  r <- move_files(from_path = template_dir,
                  to_path = path)

  invisible(r)
}




