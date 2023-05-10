studyBadge <- function(badgeName, src) {
  path <- usethis:::find_readme()
  if (is.null(path)) {
    cli::cat_bullet("Unable to find a README in picard project. Run `picard::makeReadMe()` to initialize.",
                    bullet = "cross", bullet_col = "red")
    path <- "README"

  }
  usethis:::block_replace(
    glue::glue("{crayon::green(badgeName)} badge"),
    glue::glue("![{badgeName}]({src})"),
    path = path,
    block_start = "<!-- studyStatus: start -->",
    block_end = "<!-- studyStatus: end -->"
  )


}

#' Function to update the study status
#' @param status the study status for the project
#' @export
studyStatusBadge <- function(status = c("Repo Created", "Started", "Design Finalized",
                                         "Results Available", "Complete", "Suspended")) {

  color <- switch(status,
                  `Repo Created` = "lightgray",
                  Started = "blue",
                  `Design Finalized` = "limegreen",
                  Complete = "orange",
                  Suspended = "red")

  src <- glue::glue("https://img.shields.io/badge/Study%20Status-Repo%20{status}-{color}.svg")
  tag <- glue::glue("Study Status: {status}")
  studyBadge(badgeName = tag, src = src)
}

#' Function to add cdm badge
#' @param version the cdm version for the project
#' @export
cdmBadge <- function(version = c("5.3", "5.4")) {

  vv <- switch(version,
               `5.3` = "53",
               `5.4` = "54")

  src <- glue::glue("https://img.shields.io/badge/CDM%20Version-{version}-lemonchiffon.svg")
  href <- glue::glue("https://ohdsi.github.io/CommonDataModel/cdm{vv}.html")
  usethis::use_badge("CDM Version", href, src)

}

#' Function to add vocab badge
#' @param version the vocab version for the project
#' @export
vocabBadge <- function(version = c("5.0")) {

  src <- glue::glue("https://img.shields.io/badge/Vocabulary%20Version-{version}-rosybrown.svg")
  href <- glue::glue("https://github.com/OHDSI/Vocabulary-v5.0")
  usethis::use_badge("Vocabulary Version", href, src)

}

