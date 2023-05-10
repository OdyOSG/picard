#' Function to download an ohdsi study repo
#' @param org the name of the organization hosting the repo, for example 'ohdsi-studies'
#' @param repo the name of the study repository on github
#' @param savePath the location to save down the repository
#' @export
downloadStudy <- function(org, repo, savePath) {


  cli::cat_rule("Downloading OHDSI Study")
  txt <- glue::glue("{org}/{repo}")
  cli::cat_bullet("Using Study Repo: ", crayon::green(txt),
                  bullet = "info", bullet_col = "blue")

  repo <- tolower(repo)
  zip <- gh::gh("GET /repos/{owner}/{repo}/zipball/main",
         owner = org,
         repo = repo)
  url <- attr(zip, "request")$url
  cd <- attr(zip, "response")$`content-disposition` %>%
    cleanContentDisposition(org = org)

  tmp <- fs::file_temp("zip-download-")
  curl::curl_download(url = url, destfile = tmp, handle = curl::new_handle())
  zipFile <- fs::file_move(path = tmp, new_path = fs::path(savePath, cd))
  modUnzip(zipFile, open = TRUE)
  invisible(zipFile)
}

# Helpers ---------------------
cleanContentDisposition <- function(cd, org) {
  firstPattern <- glue::glue("attachment; filename={org}-")
  cd1 <- stringr::str_replace(cd, pattern = firstPattern, replacement = "")
  cd2 <- stringr::str_replace(cd1, pattern = "-.*", replacement = "")
  cd3 <- paste0(cd2, ".zip")
  return(cd3)
}

#This funciton is a modification of usethis:::tidyunzip
modUnzip <- function(zipFile, open = TRUE) {
  projName <- basename(zipFile) %>% fs::path_ext_remove()
  base_path <- fs::path_dir(zipFile)
  filenames <- utils::unzip(zipFile, list = TRUE)[["Name"]]
  filenames <- filenames[filenames != "/"]
  filenames <- filenames[usethis:::keep_lgl(filenames)]
  td <- usethis:::top_directory(filenames)

  target <- fs::path(base_path, td)
  clean <- fs::path(base_path, projName)

  utils::unzip(zipFile, files = filenames, exdir = base_path)
  cli::cat_bullet("Unzipping contents from ", crayon::cyan(basename(zipFile)),
                  bullet = "tick", bullet_col = "green")
  fs::dir_copy(path = target,
               new_path = clean,
               overwrite = TRUE)
  cli::cat_bullet("Clean up from unzip",
                  bullet = "tick", bullet_col = "green")
  fs::file_delete(zipFile)
  fs::dir_delete(target)

  if (open) {
    cli::cat_bullet("Opening project in Rstudio",
                    bullet = "tick", bullet_col = "green")
    rstudioapi::openProject(clean, newSession = TRUE)
  }
  invisible(clean)
}
