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
#' @param dependencies the list of dependencies for the r script
#' @param openFile a T/F toggle specifying whether to open file in Rstudio
#' @param path the path to write the R file, default is the R folder of the active project
#' @export
newRScript <- function(scriptName,
                       ...,
                       dependencies = c(),
                       openFile = TRUE,
                       path = here::here("R")) {


  txt <- c(
    buildHeader(...),
    buildDependcies(dependencies),
    "# Script --------------------\n", ""
  )
  fileName <- paste0(scriptName, ".R")
  filePath <- fs::path(path, fileName)

  readr::write_lines(txt, file = filePath)
  cli::cat_bullet(crayon::green(fileName), " generated at ", crayon::cyan(path),
                  bullet = "tick", bullet_col = "green")
  #open file
  if (openFile) {
    rstudioapi::navigateToFile(filePath)
  }

  invisible(txt)
}


#' Create a new pipeline task
#' @description This function creates a R script and sourced script for functions and a
#' folder in the output folder. The r script is labeled with the step in the pipeline
#' @param scriptName the name of your project script
#' @param taskNumber the task number in the pipeline
#' @param ... a list of additional inputs used to annotate your script. Names of inputs
#' must appear in snakecase. For example study_contact = "John Smith"
#' @param dependencies the list of dependencies for the r script
#' @param openFile a T/F toggle specifying whether to open file in Rstudio
#' @param path the path to write the R file, default is the active project
#' @export
newPipelineTask <- function(scriptName,
                            taskNumber,
                            ...,
                            dependencies = c(),
                            openFile = TRUE,
                            path = here::here()) {

  #label
  taskNumber <- scales::label_number(prefix = "0")(taskNumber)
  pipelineNm <- paste(taskNumber, paste0(scriptName, ".R"), sep = "_")

  #create internals file
  sourceScriptFile <- paste0("_", scriptName, ".R")
  r_int_dir <- fs::path(path, "R/internals")
  if (!fs::dir_exists(r_int_dir)) {
    addFolder("internals", directory = "R")
  }
  fs::path(r_int_dir, sourceScriptFile) %>%
    fs::file_create()
  cli::cat_bullet("Created File: ", crayon::green(fs::path(r_int_dir, sourceScriptFile)),
                  bullet = "tick",
                  bullet_col = "green")



  # create output folder
  picard::addFolder(folderName = paste(taskNumber, scriptName, sep = "_"), directory = "output")

  #create the source file for script
  sourceFile <- fs::path("R/internals", sourceScriptFile)
  source_internals <- glue::glue("source('{sourceFile}')")

  #create outputFolder assignment for script
  outputFolder_assign <- fs::path("output", paste(taskNumber, scriptName, sep = "_"))

  txt <- c(
    buildHeader(...),
    buildDependcies(dependencies),
    source_internals, "\n",
    "# Script --------------------\n",
    glue::glue("outputFolder <- '{outputFolder_assign}' "),
    "\n"
  )

  filePath <- fs::path(path, "R", pipelineNm)
  readr::write_lines(txt, file = filePath)
  cli::cat_bullet(crayon::green(pipelineNm), " generated at ", crayon::cyan(fs::path(path, "R")),
                  bullet = "tick", bullet_col = "green")
  #open file
  if (openFile) {
    rstudioapi::navigateToFile(filePath)
  }
  invisible(txt)


}
