#' create study readme
#'
#' @param author name of author
#' @param study_type type of study
#'
#' @export
create_study_readme <- function(author, study_type) {

  data <- list(
    Project = stringr::str_to_title(basename(here::here())),
    author = author,
    study_type = study_type
  )

  new <- usethis:::use_template(template = "readme_template.md",
                                save_as = "README.md",
                                data = data,
                                open = TRUE,
                                package = "picard")
  invisible(new)

}


#' Initializes a github repo
#'
#' @param author name of author
#' @param study_type type of study
#' @param organization name of the organization to create the repo
#' @param private toggle to determine if repo should be a private repository
#'
#' @export
start_github_repo <- function(author,
                              study_type = "Characterization",
                              organization,
                              private = TRUE) {


  #create basic readme
  if (!fs::file_exists("README.md")) {
    create_study_readme(author = author, study_type = study_type)
  }

  #initialize git
  usethis::use_git(message = "Initialize OHDSI Project")
  #create github repo
  usethis::use_github(organisation = organization,
                      private = private)
  invisible(author)
}
