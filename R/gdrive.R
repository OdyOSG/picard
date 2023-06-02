get_drive <- function(name) {
  googledrive::with_drive_quiet(
    googledrive::drive_get(name)
  )
}

check_study_drive_dne <- function(studyName) {
  tt <- get_drive(studyName)
  check <- nrow(tt) == 0
  return(check)
}

#' Check if the project exists on gdrive
#' @param name a project name to check if it exists in gdrive, default is active directory name
#' @export
checkProjectDrive <- function(name = NULL) {

  if (is.null(name)) {
    name <- basename(here::here())
  }

  check <- check_study_drive_dne(name)

  if (check) {
    cli::cat_bullet("Project GDrive does not exists. Please create using `makeProjectDrive()`",
                    bullet = "info", bullet_col = "blue")
  } else {
    cli::cat_bullet("Project GDrive already exists", bullet = "info", bullet_col = "blue")
    rr <- get_drive(name)$id
    cli::cat_line("Uses drive_id: ", crayon::red(rr))
  }

  invisible(check)
}


#' Make project of gdrive
#' @param projectPath the path to the project
#' @export
makeProjectDrive <- function(projectPath = here::here()) {

  name <- basename(projectPath)
  check <- check_study_drive_dne(name)

  if (check) {
    cli::cat_bullet("Creating Study Folder on Google Drive: ", crayon::cyan(studyName),
                    bullet = "info", bullet_col = "blue")

    googledrive::with_drive_quiet(
      googledrive::drive_mkdir(name = name)
    )
  } else {
    cli::cat_bullet("Project GDrive ", crayon::cyan(name), " already exists",
                    bullet = "info", bullet_col = "blue")
    rr <- get_drive(name)
    cli::cat_line("\t-Uses drive_id: ", crayon::red(rr$id))
    cli::cat_line("\t-Uses drive_path: ", crayon::cyan(rr$path))
  }
  invisible(check)


}

#' upload file to project drive
#' @param file the file to upload to gdrive
#' @param projectPath the path to the project, default is active directory
#' @export
uploadToProjectDrive <- function(file, projectPath = here::here()) {

  name <- basename(projectPath)
  check <- check_study_drive_dne(name)
  fname <- basename(file)
  if (check) {
    cli::cat_bullet("Project GDrive does not exists. Please create using `makeProjectDrive()`",
                    bullet = "info", bullet_col = "blue")
  } else{
    cli::cat_bullet("Uploaded file ", crayon::green(fname), " to ", crayon::cyan(name), " project on GDrive",
                    bullet = "info", bullet_col = "blue")
    googledrive::with_drive_quiet(
      googledrive::drive_put(
        media = file,
        path = get_drive(name)$id,
        name = fname
        )
    )
  }
  invisible(check)
}


