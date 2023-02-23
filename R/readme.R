#' Create a new README file for the project
#' @param title the name of your project
#' @param ... a list of additional inputs used to describe your project. Names of inputs
#' must appear in snakecase. For example study_type = "Characterization"
#' @param path the path to write the readme file, default is active project
#' @export
newReadMe <- function(title,
                      ...,
                      path = here::here()) {
  heading <- glue::glue("\n# {title}")
  svg_status <- studyStatus(status = "Active")
  svg_phase <- studyPhase(phase = "Initialized")
  svg_cdm <- cdmVersion(version = "5.3")
  svg_vocab <- vocabularyVersion(version = "5.0")
  meta <- projectMeta(...)
  txt <- c(
    #SVG tags
    svg_status, svg_phase, svg_cdm, svg_vocab,
    "",
    # Title
    heading,
    "",
    #Meta
    "## Project Meta\n", "",
    meta,
    "",
    # Description
    "## Project Description\n", "",
    "*Add a project decription to your README*",
    "",
    # Requirements
    "## Project Requirements\n", "",
    defaultProjectRequirements(),
    "",
    # How to Setup
    "## Setup Project\n", "",
    "*Add setup instructions to your README*",
    "",
    # How to execute study
    "## Project Execution\n", "",
    "*Add instructions for project execution to your README*",
    "",
    # Data Transfer
    "## Results Exchange\n", "",
    "*Add instructions for how to transfer results to your README*",
    ""
    )
  filePath <- fs::path(path, "README.md")

  readr::write_lines(txt, file = filePath)
  invisible(txt)
}


studyStatus <- function(status = c("Active", "Paused", "Inactive")) {
  status <- checkmate::matchArg(status, c("Active", "Paused", "Inactive"))
  color <- switch(status,
                  Active = "limegreen",
                  Paused = "yellow",
                  Inactive = "firebrick")

  src <- glue::glue("https://img.shields.io/badge/Study%20Status-{status}-{color}.svg")
  alt <- glue::glue("Study Status: {status}")

  tag <- htmltools::img(src = src, alt = alt)
  hh <- as.character(tag)
  return(hh)
}

studyPhase <- function(phase) {
  src <- glue::glue("https://img.shields.io/badge/Study%20Phase-{phase}-gainsboro.svg")
  alt <- glue::glue("Study Phase: {phase}")

  tag <- htmltools::img(src = src, alt = alt)
  hh <- as.character(tag)
  return(hh)
}

cdmVersion <- function(version) {
  src <- glue::glue("https://img.shields.io/badge/CDM%20Version-{version}-dodgerblue.svg")
  alt <- glue::glue("CDM Version: {version}")

  tag <- htmltools::img(src = src, alt = alt)
  hh <- as.character(tag)
  return(hh)
}

vocabularyVersion <- function(version) {
  src <- glue::glue("https://img.shields.io/badge/Vocabulary%20Version-{version}-rosybrown.svg")
  alt <- glue::glue("Vocabulary Version: {version}")

  tag <- htmltools::img(src = src, alt = alt)
  hh <- as.character(tag)
  return(hh)
}

defaultProjectRequirements <- function() {
  c("-   [R](https://cloud.r-project.org/) installed, (version 4.0 or greater)",
    "-   [R Studio](https://posit.co/download/rstudio-desktop/) installed",
    "-   On Windows: [RTools](https://cran.r-project.org/bin/windows/Rtools/) installed",
    "-   [Java](https://www.java.com/en/) installed",
    "-   Suggested at least 25GB of free disk space")
}


projectMeta <- function(...) {
  ll <- rlang::list2(...)
  nm <- snakecase::to_title_case(names(ll))
  val <- as.character(ll)
  paste("-   ", nm, ": ", val, sep = "")
}
