

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

  # Step 8: add cohort json
  if(length(projectSpecs$cohortJson) > 0) {
    add_cohorts(projectSpecs)
  }

  #Step 9: open project
  open_new_proj(projectSpecs)


  invisible(projectSpecs)

}


