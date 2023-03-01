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

  #create internal file
  sourceScriptFile <- paste0("_", scriptName, ".R")
  txt_internals <- glue::glue("# Internal functions for {scriptName} -------")
  sourceFilePath <- fs::path(path, "R", sourceScriptFile)
  readr::write_lines(txt_internals, file = sourceFilePath)
  cli::cat_bullet(
    crayon::green(sourceScriptFile), " generated at ", crayon::cyan(dirname(sourceFilePath)),
    bullet = "tick", bullet_col = "green"
  )

  # create output folder
  outputFolder <- fs::path(path, "output", paste(taskNumber, scriptName, sep = "_"))
  fs::dir_create(outputFolder)
  cli::cat_bullet("Created Directory: ", crayon::cyan(outputFolder),
                  bullet = "tick",
                  bullet_col = "green")

  #create the source file for script
  sourceFile <- fs::path("R/", sourceScriptFile)
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

#' check pipeline order
#' @param path the path to write the R file, default is the active project
#' @export
checkPipelineOrder <- function(path = here::here()) {
  ff <- fs::path(path, "R") %>%
    fs::dir_ls()
  check <- startsWith(basename(ff), "_")
  ff2 <- ff[!check]
  basename(ff2)
}

#' Import pipeline task using vault
#' @param taskNumber the task number in the pipeline
#' @param vaultString a string for the vault specified as '<org>/<repo>'
#' @param item the vault item to import into the project
#' @param path the path to write the R file, default is the active project
#' @export
importPipelineTask <- function(taskNumber,
                               vaultString,
                               item,
                               path = here::here()) {
  taskNumber <- scales::label_number(prefix = "0")(taskNumber)
  pipelineNm <- paste(taskNumber, paste0(item, ".R"), sep = "_")
  savePath <- fs::path(path, "R")
  vv <- vault::vault(vaultString, savePath = savePath)
  tt <- purrr::quietly(vault::checkout)(vv, item = item, openFile = FALSE)
  oldFile <- fs::path(savePath, tt, ext = "R")
  newFile <- fs::path(savePath, pipelineNm)
  fs::file_move(oldFile, newFile)
  cli::cat_bullet("Imported ", crayon::green(pipelineNm), " from ",
                  crayon::magenta(vaultString),
                  bullet = "tick", bullet_col = "green")
  cli::cat_bullet("Saved ", crayon::green(pipelineNm), " to ",
                  crayon::cyan(savePath),
                  bullet = "tick", bullet_col = "green")
}
