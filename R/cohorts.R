#' Add cohorts that are json files to ohdsi project
#' @description This function adds cohort json files to a subfolder in input/cohortsToCreate.
#' The subfolder is specified by the type.
#' @param type the type of subfolder to add for cohorts in the project
#' @param files a set of json files to move into the input/cohortsToCreate folder of the project
#' @param cohortFolder the input/cohortsToCreate folder to save the cohorts, defaults to the folder
#' within the active project binding
#' @export
addCohortsFiles<- function(type = c('studyPop', 'strata', 'covariates',
                                    'target', 'comparator', 'outcome',
                                    'exposure', 'diagnostics', 'other'),
                           files,
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

  #Scenario 1: all json files
  ff <- basename(files)
  ext <- tools::file_ext(files)
  if (length(ext) > 0 & all(ext == "json")) {
    new_loc <- fs::path(folderName, ff)
    fs::file_copy(path = files, new_path = new_loc, overwrite = TRUE)
    cli::cat_bullet("Cohorts added to ", crayon::cyan(folderName), bullet = "tick", bullet_col = "green")
    cli::cat_line("\t-  ",crayon::green(ff))
  } else{
    stop("Not all files added are .json")
  }

  invisible(new_loc)

}


#' Add capr cohorts to an ohdsi project
#' @description This function adds capr cohorts to a subfolder in input/cohortsToCreate.
#' The subfolder is specified by the type.
#' @param type the type of subfolder to add for cohorts in the project
#' @param caprList a list of capr objects to upload into the input/cohortsToCreate folder of the project
#' @param cohortFolder the input/cohortsToCreate folder to save the cohorts, defaults to the folder
#' within the active project binding
#' @export
addCohortsCapr<- function(type = c('studyPop', 'strata', 'covariates',
                                   'target', 'comparator', 'outcome',
                                   'exposure', 'diagnostics', 'other'),
                          caprList,
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


  #Scenario 2: capr objects to load into a project
  check <- purrr::map_chr(caprList, ~methods::is(.x))
  are_all_capr <- all(check == "Cohort")

  if (are_all_capr) {
    #get names for capr cohorts
    nmm <- names(caprList)
    if (is.null(nmm)) {
      nmm <- paste("Capr Cohort", seq_along(caprList))
      cli::cat_bullet("Cohorts do not have names so using generic 'Capr Cohort' naming",
                      bullet = "warning", bullet_col = "yellow")
    }
    #create file path
    caprFile <- fs::path(folderName, nmm, ext = "json")

    #add files to cohort folder
    purrr::walk2(caprList, caprFile,
                 ~Capr::writeCohort(.x, .y))
    #consule message saying what happend
    cli::cat_bullet("Cohorts added to ", crayon::cyan(folderName), bullet = "tick", bullet_col = "green")
    cli::cat_line("\t-  ",crayon::green(fs::path(nmm, ext = "json")))
  } else{
    stop("Not all objects are a Capr Cohort class")
  }

  invisible(caprFile)

}

getWebApiCohortJson <- function(cohortId, baseUrl) {
  cohortDefinition <- ROhdsiWebApi::getCohortDefinition(cohortId = cohortId,
                                                        baseUrl = baseUrl)$expression
  RJSONIO::toJSON(x = cohortDefinition, digits = 23, pretty = TRUE)
}


getWebApiCohortInfo <- function(cohortId, baseUrl) {
  cohortDefinition <- ROhdsiWebApi::getCohortDefinition(cohortId = cohortId,
                                                        baseUrl = baseUrl)
  paste(cohortDefinition$name, paste0("id", cohortDefinition$id), sep = "_")
}

#' Add cohorts from webapi to ohdsi project
#' @description This function adds cohort jsons from ATLAS to a subfolder in input/cohortsToCreate.
#' The subfolder is specified by the type.
#' @param type the type of subfolder to add for cohorts in the project
#' @param cohortIds a vector of cohorts ids used to identify the cohorts to upload from webapi
#' into the input/cohortsToCreate folder of the project
#' @param webApiBaseUrl the base URL to access webapi, defaults to a system envrionment variable
#' @param webApiAuthMethod the webApi Authentication method, defaults to a system envrionment variable
#' @param webApiUser the user name used to access WebApi, defaults to a system envrionment variable
#' @param webApiPassword the password used to access webApi, defaults to a system envrionment variable
#' @param cohortFolder the input/cohortsToCreate folder to save the cohorts, defaults to the folder
#' within the active project binding
#' @export
addCohortsWebApi <- function(type = c('studyPop', 'strata', 'covariates',
                                      'target', 'comparator', 'outcome',
                                      'exposure', 'diagnostics', 'other'),
                             cohortIds,
                             webApiBaseUrl = Sys.getenv("WEBAPI_URL"),
                             webApiAuthMethod = Sys.getenv("WEBAPI_AUTHMETHOD"),
                             webApiUser = Sys.getenv("WEBAPI_USERNAME"),
                             webApiPassword = Sys.getenv("WEBAPI_PASSWORD"),
                             cohortFolder = here::here("input/cohortsToCreate")) {

  # Check WebApi Credentials
  if (webApiBaseUrl == "") {
    stop("Need to set environement variable for webapi url")
  }

  if (webApiAuthMethod == "") {
    stop("Need to set environement variable for webapi auth method")
  }

  if (webApiUser == "") {
    stop("Need to set environement variable for webapi user name")
  }

  if (webApiPassword == "") {
    stop("Need to set environement variable for webapi password")
  }

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


  #authorize webapi
  ss <- purrr::safely(ROhdsiWebApi::authorizeWebApi)(webApiBaseUrl,
                                                     authMethod = webApiAuthMethod,
                                                     webApiUsername = webApiUser,
                                                     webApiPassword = webApiPassword)
  if (is.null(ss$error)) {
    cli::cat_bullet("Authorized Connection to WebApi", bullet = "tick",
                    bullet_col = "green")

    cohortNames <- purrr::map_chr(cohortIds, ~getWebApiCohortInfo(.x, baseUrl = webApiBaseUrl)) %>%
      gsub("\\s", "_", .)
    json <- purrr::map(cohortIds,
                       ~getWebApiCohortJson(cohortId = .x, baseUrl = webApiBaseUrl))

    cohortFile <- fs::path(folderName, cohortNames, ext = "json")
    purrr::walk2(json, cohortFile,
                 ~readr::write_file(.x, file = .y))
    #consule message saying what happend
    cli::cat_bullet("Cohorts added to ", crayon::cyan(folderName), bullet = "tick", bullet_col = "green")
    cli::cat_line("\t-  ",crayon::green(fs::path(cohortNames, ext = "json")))
  } else{
    stop("Could not upload cohorts to folders. Check if webapi connection is correct")
  }
  invisible(cohortFile)

}
