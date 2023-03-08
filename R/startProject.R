#' Start an ohdsi project
#'
#' @param projectSpecs either character string specifying the name of the project or a picard specs object
#' created using the projectSpecs function. If the character string is supplied then default specs
#' are generated for the project.
#' @export
startOhdsiProject <- function(projectSpecs) {

  check <- class(projectSpecs)

  if (check == "character") {
    projectSpecs <- projectSpecifications(projectSpecs)
  }

  # Step 1: create project directory
  create_proj_dir(projectSpecs)

  # Step 2: add project to directory
  add_r_proj(projectSpecs)

  # Step 3: add folders
  create_proj_folders(projectSpecs)

  #Step 4: create config file
  create_config_file(projectSpecs)

  #Step 5: add config blocks
  add_config_blocks(projectSpecs)

  # Step 6: initialize cohort tables
  initialize_cohort_tables(projectSpecs)

  # Step 7: add news file to project
  add_news_file(projectSpecs)

  #Step 8: open project
  open_new_proj(projectSpecs)


  invisible(projectSpecs)

}

#' Start demo picard project
#' @param path the path of where to place the demo project
#' @param configBlock a character string for a configBlock to use
#' @export
startDemoProject <- function(path, configBlock) {

  #Step 0: Set project specs for demo

  projectSpecs <- picard::projectSpecifications(
    projectName = "demo",
    path = path,
    configBlocks = configBlock,
    addDiagnostics = TRUE
  )


  # Step 1: create project directory
  create_proj_dir(projectSpecs)

  # Step 2: add project to directory
  add_r_proj(projectSpecs)

  # Step 3: add folders
  create_proj_folders(projectSpecs)

  #Step 4: create config file
  create_config_file(projectSpecs)

  #Step 5: add config blocks
  add_config_blocks(projectSpecs)

  # Step 6: initialize cohort tables
  initialize_cohort_tables(projectSpecs)

  # Step 7: add news file to project
  add_news_file(projectSpecs)

  # Step 8: add test cohorts to input/cohortsToCreate
  instCohortsPaths <- fs::path_package("picard", "demo/cohortsToCreate") %>%
    fs::dir_ls(recurse = TRUE, type = "file")
  demoCohorts <- gsub(".*cohortsToCreate/", "", instCohortsPaths)
  demoCohortPaths <- fs::path(path, "demo/input/cohortsToCreate", demoCohorts)
  fs::file_copy(
    path = instCohortsPaths,
    new_path = demoCohortPaths,
    overwrite = TRUE
  )
  cli::cat_bullet("Step 8: Add demo cohorts to picard project",
                  bullet_col = "green", bullet = "info")

  # Step 9: Open project
  open_new_proj(projectSpecs)

  invisible(projectSpecs)
}
