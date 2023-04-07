#' Function to create a README file
#' @param projectPath the path to the project
#' @param author the name of the person writing the study synopsis
#' @param open toggle on whether the file should be opened
#' @export
makeReadMe <- function(projectPath = here::here(), author = NULL, open = TRUE) {

  projName <- basename(projectPath) %>%
    snakecase::to_title_case()
  date <- lubridate::today()

  if (is.null(author)) {
    author <- "[Add Name of Author]"
  }


  data <- rlang::list2(
    'Project' = projName,
    'Author' = author,
    'Date' = date
  )

  usethis::use_template(
    template = "README.md",
    data = data,
    open = open,
    package = "picard")

  invisible(data)

}

#' Function to create a NEWS file
#' @param projectPath the path to the project
#' @param open toggle on whether the file should be opened
#' @export
makeNews <- function(projectPath = here::here(), open = TRUE) {

  projName <- basename(projectPath) %>%
    snakecase::to_title_case()


  data <- rlang::list2(
    'Project' = projName,
  )

  usethis::use_template(
    template = "NEWS.md",
    data = data,
    open = open,
    package = "picard")

  invisible(data)

}

#' Function to create a config.yml file
#' @param projectPath the path to the project
#' @param open toggle on whether the file should be opened
#' @export
makeConfig <- function(projectPath = here::here(), open = TRUE) {

  projName <- basename(projectPath) %>%
    snakecase::to_snake_case()


  data <- rlang::list2(
    'Project' = projName,
    'Cohort' = paste(projName, "cohorts", sep = "_")
  )

  usethis::use_template(
    template = "config.yml",
    data = data,
    open = open,
    package = "picard")

  usethis::use_git_ignore(ignores = "config.yml")

  invisible(data)
}


#' Function to create a Synopsis file
#' @param projectPath the path to the project
#' @param author the name of the person writing the study synopsis
#' @param title a title for the synopsis
#' @param open toggle on whether the file should be opened
#' @export
makeSynopsis <- function(projectPath = here::here(), author = NULL, title = projectPath, open = TRUE) {

  if (title == projectPath) {
    title <- basename(projectPath) %>%
      snakecase::to_title_case()
  }

  if (is.null(author)) {
    author <- "[Add Name of Author]"
  }



  data <- rlang::list2(
    'Title' = title,
    'Author' = author,
    'Date' = lubridate::today()
  )

  usethis::use_template(
    template = "StudySynopsis.qmd",
    data = data,
    open = open,
    package = "picard")

  invisible(data)

}

#' Function to create a pipeline task as a Rmd file
#' @param taskName The name of the task that is being done
#' @param step the number that the task is run as a step in a pipeline
#' @param projectPath the path to the project
#' @param author the name of the person writing the study synopsis
#' @param open toggle on whether the file should be opened
#' @export
makeRmdTask <- function(taskName, step = NULL, projectPath = here::here(), author = NULL, open = TRUE) {

  if (is.null(step)) {
    taskFileName <- taskName
  } else{
    step <- scales::label_number(prefix = "0")(step)
    taskFileName <- paste(step, taskName, sep = "_")
  }

  if (is.null(author)) {
    author <- "[Add Name of Author]"
  }

  data <- rlang::list2(
    'Task' = snakecase::to_title_case(taskName),
    'Author' = author,
    'Date' = lubridate::today()
  )


  usethis::use_template(
    template = "PipelineTask.Rmd",
    save_as = fs::path("analysis", taskFileName, ext = "Rmd"),
    data = data,
    open = open,
    package = "picard")

  invisible(data)
}

#' Function to create a pipeline task as an R file
#' @param taskName The name of the task that is being done
#' @param step the number that the task is run as a step in a pipeline
#' @param projectPath the path to the project
#' @param author the name of the person writing the study synopsis
#' @param open toggle on whether the file should be opened
#' @export
makeScriptTask <- function(taskName, step = NULL, projectPath = here::here(), author = NULL, open = TRUE) {

  if (is.null(step)) {
    taskFileName <- taskName
  } else{
    step <- scales::label_number(prefix = "0")(step)
    taskFileName <- paste(step, taskName, sep = "_")
  }

  if (is.null(author)) {
    author <- "[Add Name of Author]"
  }

  data <- rlang::list2(
    'Task' = snakecase::to_title_case(taskName),
    'Author' = author,
    'Date' = lubridate::today()
  )


  usethis::use_template(
    template = "PipelineTask.R",
    save_as = fs::path("analysis", taskFileName, ext = "R"),
    data = data,
    open = open,
    package = "picard")

  invisible(data)
}
#' Function to create a pipeline task as an R file
#' @param internalsName The name of the internals file that is being created
#' @param projectPath the path to the project
#' @param author the name of the person writing the study synopsis
#' @param open toggle on whether the file should be opened
#' @export
makeInternals <- function(internalsName, projectPath = here::here(), author = NULL, open = TRUE) {

  intFileName <- paste0("_", internalsName)
  intPath <- fs::path(projectPath, "analysis/R") %>%
    fs::dir_create()



  if (is.null(author)) {
    author <- "[Add Name of Author]"
  }

  data <- rlang::list2(
    'Task' = snakecase::to_title_case(internalsName),
    'Author' = author,
    'Date' = lubridate::today()
  )


  usethis::use_template(
    template = "Internals.R",
    save_as = fs::path("analysis/R", intFileName, ext = "R"),
    data = data,
    open = open,
    package = "picard")

  invisible(data)

}
