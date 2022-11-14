#' load  cohorts
#' @param from_path path where cohort json files are found
#' @param to_path path where cohort json files are saved
#' @include helpers.R
#' @export
load_cohorts <- function(from_path, to_path){
  path <- fs::path_expand_r(to_path) %>% as.character()
  fs::dir_create(to_path)
  ParallelLogger::logInfo(
    "Loading cohorts into ", crayon::blue(to_path), "from ", crayon::red(from_path)
  )
  r <- move_files(
    from_path = from_path,
    to_path = to_path
  )

  invisible(r)
}

boo_cohorts_path <- function() {
  fs::path_package("picard", "templates/demo_cohorts/boo")
}

gi_cohorts_path <- function() {
  fs::path_package("picard", "templates/demo_cohorts/gi")
}

#' create file for cohort tracking
#' @param cohort_path path to directory holding cohorts
#' @export
create_cohort_tracking <- function(cohort_path) {

  #get project directory
  path <- here::here("input")
  project <- stringr::str_to_title(basename(here::here()))

  template_path <- fs::path_package("picard", "templates/cohort_tracking_template.md")

  template <- readr::read_lines(template_path)

  #create the whisker parameters
  cohort_files <- list.files(cohort_path) %>%
    tools::file_path_sans_ext() %>%
    SqlRender::camelCaseToTitleCase() %>%
    tibble::tibble(cohort = .)
  date <- tibble::tibble(date = rep(as.character(lubridate::today()),
                                    length(cohort_files)))
  dat <- dplyr::bind_cols(cohort_files, date) %>%
    whisker::rowSplit()
  template <- whisker::whisker.render(template)
  new <- readr::write_lines(template, file = file.path(path, "cohort_tracking.md"))
}


#' create input directory
#' @param cohort_path to directory holding cohorts
#' @export
input_directory <- function(cohort_path) {

  # create directory
  input_path <- here::here("input")
  ParallelLogger::logInfo("Creating input directory: ", crayon::blue(input_path))
  fs::dir_create(input_path)

  #load cohorts to new directory
  cohorts_to_create <- here::here("input/cohorts_to_create")
  fs::dir_create(cohorts_to_create)
  load_cohorts(from_path = cohort_path,
               to_path = cohorts_to_create)


  #initialize cohort_tracking.md
  create_cohort_tracking(cohort_path = cohort_path)
  ParallelLogger::logInfo("Initializing cohort_tracking.md")


  invisible(cohort_path)

}

#' create output directory
#' @param output_folders_names to directory holding cohorts
#' @export
output_directory <- function(output_folders_names) {
  ParallelLogger::logInfo("Creating output folders")

  output_path <- here::here("output")

  paths <- purrr::map_chr(output_folders_names,
                      ~fs::path(output_path, .x)) %>%
    purrr::map_chr(~fs::path_expand_r(.x)) %>%
    purrr::map(~fs::dir_create(.x))

  purrr::walk(paths,
              ~ParallelLogger::logInfo(
                "\t- ",
                crayon::red(fs::path_file(.x)),
                " path: ",
                crayon::green(.x)
              ))

  invisible(paths)

}
