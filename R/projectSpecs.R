


defaultCohortTableNames <- function(projectName) {
  list(
    analysisCohorts = projectName,
    diagnosticsCohorts = paste(projectName, "diagnostics", sep = "_")
  )
}

defaultFolders <- function() {
  c('input/cohortsToCreate', 'input/conceptSets', 'output', 'extras')
}

#' Function to create project specifications
#' @param projectName the name of the project
#' @param path the location of the folder, defaults to current directory
#' @param configBlocks specify any config blocks to add
#' @param cohortTableNames specify the cohort table names
#' @param folders specify the set of folders to create in the project
#' @param setCredentials specify whether to set Credentials
#' @export
projectSpecifications <- function(projectName,
                                  path = here::here(),
                                  configBlocks = NULL,
                                  cohortTableNames = NULL,
                                  folders = NULL,
                                  setCredentials = FALSE) {

  if (is.null(cohortTableNames)) {
    cohortTableNames <- defaultCohortTableNames(projectName)
  }

  if (is.null(folders)) {
    folders <- defaultFolders()
  }

  if (is.null(configBlocks)) {
    configBlocks <- c()
  }

  structure(
    list(
      projectName = projectName,
      location = fs::path_expand_r(path),
      cohortTableNames = cohortTableNames,
      folders = folders,
      configBlocks = configBlocks,
      setCredentials = setCredentials
    ),
    class = "picardSpecs"
  )

}

