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
  addDefaultFolders(projectPath = dir_path)

  # Step 4: create _picard.yml file
  addStudyMeta(author = author,
               type = type,
               projectPath = dir_path)


  # Step 5: create gitignore
  cli::cat_bullet("Step 5: Add to .gitignore file",
                  bullet_col = "yellow", bullet = "info")
  ignores <- c("results/","logs/", "_study.yml")
  usethis:::write_union(fs::path(dir_path, ".gitignore"), ignores)


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
isOhdsiStudy <- function(basePath) {

  # check if diretory has _study.yml file
  check1 <- fs::file_exists("_study.yml") %>% unname()

  #check if has subfolders
  folders <- c("analysis", "cohortsToCreate", "results", "logs", "extras")

  ff <- fs::dir_ls(basePath, type = "directory") %>%
    basename()
  check2 <- all(folders %in% ff)

  if (check1 & check2) {
    cli::cat_bullet(
      crayon::cyan(basename(basePath)), " is a picard project",
      bullet = "tick", bullet_col = "green"
    )
  } else{
    cli::cat_bullet(
      crayon::cyan(basename(basePath)), " is not a picard project",
      bullet = "cross", bullet_col = "red"
    )

  }

  invisible(check1)

}


addDefaultFolders <- function(projectPath) {
  cli::cat_bullet("Step 3: Adding OHDSI Study Project Folders",
                  bullet_col = "yellow", bullet = "info")

  cohortFolders <- c('01_target')
  analysisFolders <- c("settings", "studyTasks", "private")
  folders <- c(
    paste('cohortsToCreate', cohortFolders, sep = "/"),
    paste('analysis', analysisFolders, sep = "/"),
    'results',
    'extras',
    'logs',
    'documentation'
  )

  pp <- fs::path(projectPath, folders) %>%
    fs::dir_create(recurse = TRUE)
  invisible(pp)
}

addStudyMeta <- function(author,
                         type = c("Characterization", "Population-Level Estimation", "Patient-Level Prediction"),
                         projectPath){

  cli::cat_bullet("Step 4: Adding _study.yml file",
                  bullet_col = "yellow", bullet = "info")


  projName <- basename(projectPath) %>%
    snakecase::to_title_case()
  date <- lubridate::today()

  data <- rlang::list2(
    'Title' = projName,
    'Author' = author,
    'Type' = type,
    'Date' = date
  )

  template_contents <- usethis:::render_template("_study.yml",
                                                 data = data,
                                                 package = "picard")
  save_as <- fs::path(projectPath, "_study.yml")
  new <- usethis:::write_utf8(save_as, template_contents)
  invisible(new)


}
