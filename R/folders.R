
#' Function to create a cohort folder in input/cohortsToCreate
#' @param folderName The name of the new folder
#' @export
makeCohortFolder <- function(folderName) {

  folderNumber <- findStepNumber(dir = "cohortsToCreate")

  if (folderNumber < 10L) {
    folderNumber <- scales::label_number(prefix = "0")(folderNumber)
  }

  folderName <- snakecase::to_upper_camel_case(folderName)

  fullName <- paste(folderNumber, folderName, sep = "_")

  cli::cat_bullet("Creating new cohort folder ", crayon::cyan(fullName), " in path ", crayon::cyan(dir_path),
                  bullet = "tick", bullet_col = "green")

  fs::path(dir_path, fullName)%>%
    fs::dir_create()

  invisible(fullName)

}
