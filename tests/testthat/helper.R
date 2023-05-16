## Taken from usethis

## If session temp directory appears to be, or be within, a project, there
## will be large scale, spurious test failures.
## The IDE sometimes leaves .Rproj files behind in session temp directory or
## one of its parents.
## Delete such files manually.
session_temp_proj <- proj_find(path_temp())
if (!is.null(session_temp_proj)) {
  Rproj_files <- fs::dir_ls(session_temp_proj, glob = "*.Rproj")
  ui_line(c(
    "Rproj file(s) found at or above session temp dir:",
    paste0("* ", Rproj_files),
    "Expect this to cause spurious test failures."
  ))
}

create_local_study <- function(dir = fs::file_temp(pattern = "testproj"),
                                 env = parent.frame()) {

  if (fs::dir_exists(dir)) {
    ui_stop("Target {ui_code('dir')} {ui_path(dir)} already exists.")
  }

  old_project <- usethis:::proj_get_() # this could be `NULL`, i.e. no active project
  old_wd <- getwd()          # not necessarily same as `old_project`

  withr::defer(
    {
      ui_done("Deleting temporary project: {ui_path(dir)}")
      fs::dir_delete(dir)
    },
    envir = env
  )

  usethis::ui_silence(
    newOhdsiStudy(projectName = "test",
                  author = "Jean-Luc Picard",
                  type = "Characterization",
                  directory = dir,
                  openProject = FALSE)
  )

  withr::defer(proj_set(old_project, force = TRUE), envir = env)

  usethis:::proj_set(dir)
  withr::defer(
    {
      ui_done("Restoring original working directory: {ui_path(old_wd)}")
      setwd(old_wd)
    },
    envir = env
  )
  setwd(usethis:::proj_get())

  invisible(usethis:::proj_get())
}
