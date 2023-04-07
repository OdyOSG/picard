#' Function that initializes a picard project used for an OHDSI study
#' @param projectName the name of the project
#' @param directory the directory to create the project
#' @param openProject should the project be opened if created
#' @export
picardProject <- function(projectName,
                          directory = here::here(),
                          openProject = TRUE) {

  # Step 1: create project directory
  cli::cat_bullet("Step 1: Creating Directory",
                  bullet_col = "yellow", bullet = "info")
  dir_path <- fs::path(directory, projectName)
  dir_path %>% fs::dir_create()

  # Step 2: Add .RProj to Directory
  cli::cat_bullet("Step 2: Add .RProj to Directory",
                  bullet_col = "yellow", bullet = "info")
  usethis::create_project(dir_path, open = FALSE)
  #remove r folder
  fs::path(dir_path, "R") %>% fs::dir_delete()

  # Step 3: add picard directory structure folders
  cli::cat_bullet("Step 3: Adding Picard Project Folders",
                  bullet_col = "yellow", bullet = "info")

  cohortFolders <- c('01_studyPop', '02_strata', '03_covariates',
                     '04_target', '05_comparator', '06_outcome',
                     '07_exposure', '08_diagnostics', '09_other')
  folders <- c(
    paste('input/cohortsToCreate', cohortFolders, sep = "/"),
    'output', 'extras', 'analysis', 'log'
  )

  fs::path(dir_path, folders) %>%
    fs::dir_create(recurse = TRUE)

  if (openProject) {
    cli::cat_bullet("Opening project in new session",
                    bullet_col = "yellow", bullet = "info")
    rstudioapi::openProject(dir_path, newSession = TRUE)
  }

  invisible(dir_path)

}

#' Function to check if the directory is a picard project
#' @param basePath the path of the directory
#' @export
isPicard <- function(basePath) {

  ff <- fs::dir_ls(basePath, type = "directory") %>%
    basename()
  check <- all(c("analysis", "extras", "input", "log", "output") %in% ff)

  if (check) {
    cli::cat_bullet(
      crayon::red(basename(basePath), " is a picard project"),
      bullet = "info", bullet_col = "yellow"
    )
  } else{
    cli::cat_bullet(
      crayon::red(basename(basePath), " is not a picard project"),
      bullet = "info", bullet_col = "yellow"
    )

  }

  invisible(check)

}
