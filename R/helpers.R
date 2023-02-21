
create_proj_dir <- function(projectSpecs) {
  # Step 1: create project directory
  cli::cat_bullet("Step 1: Creating Directory",
                  bullet_col = "green", bullet = "info")
  dir_path <- fs::path(projectSpecs$location, projectSpecs$projectName)
  dir_path %>% fs::dir_create()
  invisible(dir_path)
}

create_r_proj <- function(projectSpecs) {
  cli::cat_bullet("Step 2: Initializing Project",
                  bullet_col = "green", bullet = "info")
  dir_path <- fs::path(projectSpecs$location, projectSpecs$projectName)
  usethis::create_project(dir_path, open = FALSE)
  invisible(dir_path)
}


create_proj_folders <- function(projectSpecs) {

  cli::cat_bullet("Step 3: Adding Default Project Folders",
                  bullet_col = "green", bullet = "info")
  projectSpecs$location %>%
    fs::path(projectSpecs$projectName,projectSpecs$folders) %>%
    fs::dir_create(recurse = TRUE)
}

open_new_proj <- function(projectSpecs) {
  cli::cat_bullet("Step 7: Open project in new session",
                  bullet_col = "green", bullet = "info")
  dir_path <- fs::path(projectSpecs$location, projectSpecs$projectName)
  rstudioapi::openProject(dir_path, newSession = TRUE)
  invisible(dir_path)
}
