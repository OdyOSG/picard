#picard_scripts

buildHeader <- function(...){

  ll <- rlang::list2(...)
  nm <- snakecase::to_title_case(names(ll))
  val <- as.character(ll)
  info <- paste("## ", nm, ": ", val, sep = "")

  txt <- c(
    "# Header --------------------\n",
    info,
    "\n"
  )
  return(txt)
}


buildDependcies <- function(dependencies) {

  if (length(dependencies) > 0) {
    dependencies <- glue::glue("library({dependencies})")
  }

  txt <- c(
    "# Dependencies --------------------\n",
    dependencies, "\n"
  )
  return(txt)
}

#' Create a new R script for the project
#' @param scriptName the name of your project script
#' @param ... a list of additional inputs used to annotate your script. Names of inputs
#' must appear in snakecase. For example study_contact = "John Smith"
#' @param path the path to write the R file, default is the R folder of the active project
#' @export
newRScript <- function(scriptName,
                       ...,
                       dependencies = c(),
                       path = here::here("R")) {


  txt <- c(
    buildHeader(...),
    buildDependcies(dependencies),
    "# Script --------------------\n", ""
  )
  fileName <- paste0(scriptName, ".R")
  filePath <- fs::path(path, fileName)

  readr::write_lines(txt, file = filePath)
  cli::cat_bullet(crayon::cyan(fileName), " generated at ", path,
                  bullet = "tick", bullet_col = "green")
  #open file
  rstudioapi::navigateToFile("~/R/tests/my_ohdsi_project/R/BuildCohorts.R")
  invisible(txt)
}
