#' Create ohdsi project
#'
#' @param path Folder where the new project should be created
#'
#' @export
ohdsi_project <- function(path) {
  path <- fs::path_expand_r(path)
  # create project directory
  fs::dir_create(path)

  template_dir <- fs::path_package("picard", "templates/ohdsi_study")
  r <- move_files(from_path = template_dir,
                  to_path = path)
  #create input directory
  fs::dir_create(fs::path(path, "input"))
  #create output directory
  fs::dir_create(fs::path(path, "output"))
  #create R directory
  fs::dir_create(fs::path(path, "R"))

  #create R directory
  fs::dir_create(fs::path(path, "extras"))
  fs::file_move(path = fs::path(path, "Welcome.Rmd"),
                new_path = fs::path(path, "extras/Welcome.Rmd"))

  invisible(r)
}




