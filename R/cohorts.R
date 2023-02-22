#' Add cohorts to ohdsi project
#' @description This function adds either cohort json files or a list of capr cohorts
#' to a subfolder in input/cohortsToCreate. The subfolder is specified by the type.
#' @param type the type of subfolder to add for cohorts in the project
#' @param files a set of json files to move into the input/cohortsToCreate folder of the project
#' @param capr a list of capr objects to upload into the input/cohortsToCreate folder of the project
#' @param cohortFolder the input/cohortsToCreate folder to save the cohorts, defaults to the folder
#' within the active project binding
#' @export
addCohorts <- function(type = c('studyPop', 'strata', 'covariates',
                                'target', 'comparator', 'outcome',
                                'exposure', 'diagnostics', 'other'),
                       files = NULL,
                       capr = NULL,
                       cohortFolder = here::here("input/cohortsToCreate")) {

  type <- checkmate::matchArg(type, c('studyPop', 'strata', 'covariates',
                                      'target', 'comparator', 'outcome',
                                      'exposure', 'diagnostics', 'other'))
  folderName <- switch(
    type,
    studyPop = fs::path(cohortFolder, "01_studyPop"),
    strata = fs::path(cohortFolder, "02_strata"),
    covariates = fs::path(cohortFolder, "03_covariates"),
    target = fs::path(cohortFolder, "04_target"),
    comparator = fs::path(cohortFolder, "05_comparator"),
    outcome = fs::path(cohortFolder, "06_outcome"),
    exposure = fs::path(cohortFolder, "07_exposure"),
    diagnostics = fs::path(cohortFolder, "08_diagnostics"),
    other = fs::path(cohortFolder, "09_other")
  )


  new_loc <- fs::path_expand_r(folderName)

  #Scenario 1: all json files
  if (!is.null(files)) {
    ff <- basename(files)
    ext <- tools::file_ext(files)
    if (length(ext) > 0 & all(ext)) {
      new_loc2 <- fs::path(new_loc, ff)
      fs::file_move(path = files, new_path = new_loc2)
      cli::cat_bullet("Cohorts added to ", crayon::cyan(new_loc))
      cli::cat_line("\t-  ",crayon::green(ff))
    } else{
      stop("Not all files added are .json")
    }
  }

  # Scenario 2: capr cohorts to add
  #TODO add option for capr cohorts

  invisible(new_loc)

}

