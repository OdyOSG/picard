#' Function to construct cohort manifest
#' @param inputPath path of input folder defaults to 'input/cohortsToCreate' in
#' project directory
#' @return a tibble with the list of cohorts created based on the project
#' @export
cohortManifest <- function(inputPath = here::here("input/cohortsToCreate")) {

  #get cohort file paths
  cohortFiles <- fs::dir_ls(inputPath, recurse = TRUE, type = "file")
  #get cohort names
  cohortNames <- fs::path_file(cohortFiles) %>%
    fs::path_ext_remove()
  #get cohort type
  cohortType <- fs::path_dir(cohortFiles) %>%
    basename() %>%
    gsub(".*_", "", .)

  #future addition of hash
  hash <- purrr::map(cohortFiles, ~readr::read_file(.x)) %>%
    purrr::map_chr(~digest::digest(.x, algo = "sha1")) %>%
    unname()

  #return tibble with info
  tb <- tibble::tibble(
    name = cohortNames,
    type = cohortType,
    hash = hash,
    file = cohortFiles %>% as.character()
  ) %>%
    dplyr::mutate(
      id = dplyr::row_number(), .before = 1
    )
  return(tb)
}


flattenStudyYml <- function(projectPath = here::here()) {
  yml <- fs::path(projectPath, "_study.yml") %>%
    yaml::read_yaml()

  studyYml <- yml$Study
  names(studyYml) <- paste0("Study", names(studyYml))

  linksYml <- yml$Links
  names(linksYml) <- paste0("Links", names(linksYml))

  yml2 <- c(studyYml, linksYml)
  return(yml2)
}


getStudyDetails <- function(item, projectPath = here::here()) {

  yml <- flattenStudyYml(projectPath = projectPath)
  yml[[item]]

}


findStepNumber <- function(dir = c("cohortsToCreate", "analysis/studyTasks"), projectPath = here::here()) {

  dir <- match.arg(dir, choices = c("cohortsToCreate", "analysis/studyTasks"))

  if (dir == "cohortsToCreate") {
    items <- fs::path(projectPath, dir) %>%
      fs::dir_ls(type = "directory") %>%
      basename()
  }

  if (dir == "analysis/studyTasks") {
    items <- fs::path(projectPath, dir) %>%
      fs::dir_ls(type = "file") %>%
      basename()
  }

  if (length(items) == 0) {
    step <- 1L
  } else {
    lastNumber <- gsub("_.*", "", items) %>%
      as.integer() %>%
      max()
    step <- lastNumber + 1L
  }

  return(step)

}
