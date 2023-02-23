


defaultCohortTableNames <- function(projectName, addDiagnostics = FALSE) {
  if (addDiagnostics) {
    list(
      analysisCohorts = projectName,
      diagnosticsCohorts = paste(projectName, "diagnostics", sep = "_")
    )
  } else{
    list(
      analysisCohorts = projectName
    )
  }
}

defaultFolders <- function() {
  cohortFolders <- c('01_studyPop', '02_strata', '03_covariates',
                     '04_target', '05_comparator', '06_outcome',
                     '07_exposure', '08_diagnostics', '09_other')
  folders <- c(
    paste('input/cohortsToCreate', cohortFolders, sep = "/"),
    'output', 'extras'
  )
  return(folders)
}

#' Function to create project specifications
#' @param projectName the name of the project
#' @param path the location of the folder, defaults to current directory
#' @param configBlocks specify any config blocks to add
#' @param cohortTableNames specify the cohort table names
#' @param folders specify the set of folders to create in the project
#' @param setCredentials specify whether to set Credentials
#' @param addDiagnostics specify whether analysis includes diagnostics
#' @export
projectSpecifications <- function(projectName,
                                  path = here::here(),
                                  configBlocks = NULL,
                                  cohortTableNames = NULL,
                                  folders = NULL,
                                  setCredentials = FALSE,
                                  addDiagnostics = FALSE) {

  if (is.null(cohortTableNames)) {
    cohortTableNames <- defaultCohortTableNames(projectName, addDiagnostics)
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
      setCredentials = setCredentials,
      addDiagnostics = addDiagnostics
    ),
    class = "picardSpecs"
  )

}


