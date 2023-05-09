#' Function that initializes a new ohdsi study project environment
#' @param projectName the name of the project
#' @param author the name of the study lead
#' @param type the type of study either Characterization, PLP or PLE
#' @param directory the directory to create the project
#' @param openProject should the project be opened if created
#' @export
newOhdsiStudy <- function(projectName,
                          author,
                          type,
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
  path <- usethis:::user_path_prep(dir_path)
  pName <- fs::path_file(fs::path_abs(dir_path))
  usethis:::local_project(dir_path, force = TRUE)
  usethis:::use_rstudio()

  # Step 3: add picard directory structure folders
  cli::cat_bullet("Step 3: Adding Picard Project Folders",
                  bullet_col = "yellow", bullet = "info")

  cohortFolders <- c('01_target', '02_strata', '03_covariates')
  folders <- c(
    paste('cohortsToCreate', cohortFolders, sep = "/"),
    'results', 'extras', 'analysis', 'log'
  )

  fs::path(dir_path, folders) %>%
    fs::dir_create(recurse = TRUE)

  # Step 4: create _picard.yml file
  cli::cat_bullet("Step 4: Adding _study.yml file",
                  bullet_col = "yellow", bullet = "info")
  makeStudyMeta(author = author, type = type,
                projectPath = dir_path, open = FALSE)

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
