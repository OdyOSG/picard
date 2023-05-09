# Study Files ----------------------------

#make _study.yml file
makeStudyMeta <- function(author,
                          type = c("Characterization", "Population-Level Estimation", "Patient-Level Prediction"),
                          projectPath = here::here()) {


  projName <- basename(projectPath) %>%
    snakecase::to_title_case()
  date <- lubridate::today()

  data <- rlang::list2(
    'Title' = projName,
    'Author' = author,
    'Type' = type,
    'Date' = date
  )

  template_contents <- usethis:::render_template("_study.yml",
                                                 data = data,
                                                 package = "picard")
  save_as <- fs::path(projectPath, "_study.yml")
  new <- usethis:::write_utf8(save_as, template_contents)
  invisible(new)
}


#' Function to create a README file
#' @param projectPath the path to the project
#' @param open toggle on whether the file should be opened
#' @export
makeReadMe <- function(projectPath = here::here(), open = TRUE) {


  data <- rlang::list2(
    'Project' = getStudyDetails("StudyTitle"),
    'Author' = getStudyDetails("StudyLead"),
    'StudyType' = getStudyDetails("StudyType"),
    'StartDate' = getStudyDetails("StudyStartDate"),
    'EndDate' = getStudyDetails("StudyEndDate"),
    'StudyTags' = getStudyDetails("StudyTags"),
    'Protocol' = getStudyDetails("LinksProtocol"),
    'Publications' = getStudyDetails("LinksPublication"),
    'Dashboard' = getStudyDetails("LinksDashboard")
  )

  usethis::use_template(
    template = "README.md",
    data = data,
    open = open,
    package = "picard")

  invisible(data)

}

#' Function to create a NEWS file
#' @param projectPath the path to the project
#' @param open toggle on whether the file should be opened
#' @export
makeNews <- function(projectPath = here::here(), open = TRUE) {

  projName <- basename(projectPath) %>%
    snakecase::to_title_case()


  data <- rlang::list2(
    'Project' = projName,
  )

  usethis::use_template(
    template = "NEWS.md",
    data = data,
    open = open,
    package = "picard")

  invisible(data)

}

#' Function to create a config.yml file
#' @param block the name of the config block
#' @param database the name of the database for the block
#' @param projectPath the path to the project
#' @param open toggle on whether the file should be opened
#' @param type the type of config style to use
#' @export
makeConfig <- function(block, database, projectPath = here::here(), open = TRUE) {


  projName <- basename(projectPath) %>%
    snakecase::to_snake_case()


  data <- rlang::list2(
    'Project' = projName,
    'Cohort' = paste(projName, database, sep = "_"),
    'Block' = block,
    'Database' = database
  )

  usethis::use_template(
    template = "config.yml",
    data = data,
    open = open,
    package = "picard")

  usethis::use_git_ignore(ignores = "config.yml")

  invisible(data)
}


# Cohort Files --------------------------

#' Function to create a cohort details file
#' @param projectPath the path to the project
#' @param open toggle on whether the file should be opened
#' @export
makeCohortDetails <- function(projectPath = here::here(), open = TRUE) {

  projName <- basename(projectPath) %>%
    snakecase::to_title_case()


  data <- rlang::list2(
    'Study' = projName,
  )

  usethis::use_template(
    template = "CohortDetails.md",
    save_as = fs::path("cohortsToCreate", "cohortDetails.md"),
    data = data,
    open = open,
    package = "picard")

  invisible(data)

}

#' Function to create a cohort folder in input/cohortsToCreate
#' @param foldernName The name of the new folder
#' @param projectPath the path to the project
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

# Analysis Files --------------------------
#' Function to create an example OHDSI script
#' @param fileName The name of for the file
#' @param savePath the path to save the file, defaults to active directory
#' @param open toggle on whether the file should be opened
#' @export
makeExample <- function(fileName, savePath = here::here(), open = TRUE) {


  data <- rlang::list2(
    Example = snakecase::to_sentence_case(fileName),
    Date = lubridate::today(),
    FileName = paste(fileName, "ex", sep = "_")
  )


  template_contents <- usethis:::render_template("ExampleScript.R",
                                                 data = data,
                                                 package = "picard")

  save_as <- fs::path(savePath, fileName, ext = "R")
  new <- usethis:::write_utf8(save_as, template_contents)
  rstudioapi::navigateToFile(save_as)
  invisible(new)

}

#' Function to create a pipeline task as a Rmd file
#' @param scriptName The name of the analysis script
#' @param open toggle on whether the file should be opened
#' @param oldConnectionStyle a toggle indicating if the script should be automated with the old connection style
#' @export
makeAnalysisScript <- function(scriptName, open = TRUE, oldConnectionStyle = TRUE) {


  taskNum <- findStepNumber(dir = "analysis/studyTasks")
  step <- scales::label_number(prefix = "0")(taskNum)
  scriptFileName <- paste(step, scriptName, sep = "_")

  data <- rlang::list2(
    'Name' = snakecase::to_title_case(scriptName),
    'Author' = getStudyDetails("StudyLead"),
    'Date' = lubridate::today(),
    'FileName' = scriptFileName,
    'old' = oldConnectionStyle
  )

  usethis::use_template(
    template = "AnalysisScript",
    save_as = fs::path("analysis/studyTasks", scriptFileName, ext = "R"),
    data = data,
    open = open,
    package = "picard")

  invisible(data)


}

#
# makeRmdTask <- function(taskName, step = NULL, projectPath = here::here(), author = NULL, open = TRUE) {
#
#   if (is.null(step)) {
#     taskFileName <- taskName
#   } else{
#     step <- scales::label_number(prefix = "0")(step)
#     taskFileName <- paste(step, taskName, sep = "_")
#   }
#
#   if (is.null(author)) {
#     author <- "[Add Name of Author]"
#   }
#
#   data <- rlang::list2(
#     'Task' = snakecase::to_title_case(taskName),
#     'Author' = author,
#     'Date' = lubridate::today(),
#     'FileName' = taskFileName
#   )
#
#
#   usethis::use_template(
#     template = "PipelineTask.Rmd",
#     save_as = fs::path("analysis", taskFileName, ext = "Rmd"),
#     data = data,
#     open = open,
#     package = "picard")
#
#   invisible(data)
# }


#' Function to create a pipeline task as an R file
#' @param internalsName The name of the internals file that is being created
#' @param projectPath the path to the project
#' @param open toggle on whether the file should be opened
#' @export
makeInternals <- function(internalsName, projectPath = here::here(), open = TRUE) {

  intFileName <- paste0("_", internalsName)
  intPath <- fs::path(projectPath, "analysis/private")


  data <- rlang::list2(
    'Task' = snakecase::to_title_case(internalsName),
    'Author' = getStudyDetails("StudyLead"),
    'Date' = lubridate::today(),
    'FileName' = intFileName
  )


  usethis::use_template(
    template = "Internals.R",
    save_as = fs::path("analysis/private", intFileName, ext = "R"),
    data = data,
    open = open,
    package = "picard")

  invisible(data)

}
# Extra Files --------------------------------


#' Function to create a config.yml file
#' @param projectPath the path to the project
#' @param open toggle on whether the file should be opened
#' @param secret a keyword to use as the keyring password to access credentials
#' @export
makeKeyringSetup <- function(projectPath = here::here(), open = TRUE, secret = NULL) {

  keyringName <- snakecase::to_snake_case(getStudyDetails("StudyTitle"))
  if (is.null(secret)) {
    keyringPassword <- keyringName
  }

  data <- rlang::list2(
    'Name' = keyringName,
    'Secret'= keyringPassword
  )

  usethis::use_template(
    template = "KeyringSetup.R",
    save_as = fs::path("extras", "KeyringSetup.R"),
    data = data,
    open = open,
    package = "picard")

  usethis::use_git_ignore(ignores = "extras/KeyringSetup.R")

  invisible(data)


}

#' Function to create a Synopsis file
#' @param projectPath the path to the project
#' @param open toggle on whether the file should be opened
#' @export
makeStudySynopsis <- function(projectPath = here::here(), open = TRUE) {

  data <- rlang::list2(
    'Title' = getStudyDetails("StudyTitle"),
    'Author' = getStudyDetails("StudyLead"),
    'Date' = lubridate::today()
  )

  usethis::use_template(
    template = "StudySynopsis.qmd",
    data = data,
    open = open,
    package = "picard")

  invisible(data)

}




#' Email asking to initialize an ohdsi-studies repo
#' @param senderName the name of the person sending the email
#' @param projectPath the path to the picard project
#' @param recipientName the name of the person receiving the email
#' @param open toggle on whether the file should be opened
#' @export
requestStudyRepo <- function(senderName, projectPath = here::here(), recipientName = NULL, open = TRUE) {
  #get study name
  studyName <- basename(projectPath) %>%
    snakecase::to_upper_camel_case()


  if (is.null(recipientName)) {
    recipientName <- "[Add Name of Recipient]"
  }

  data <- rlang::list2(
    'Study' = studyName,
    'Sender' = senderName,
    'Recipient' = recipientName
  )

  usethis::use_template(
    template = "RepoRequestEmail.txt",
    save_as = fs::path("extras", "RepoRequestEmail.txt"),
    data = data,
    open = open,
    package = "picard")


  usethis::use_git_ignore(ignores = "extras/StudyRepoRequestEmail.txt")

  invisible(data)

}


#' Email asking to participant in an ohdsi study
#' @param senderName the name of the person sending the email
#' @param projectPath the path to the picard project
#' @param recipientName the name of the person receiving the email
#' @param open toggle on whether the file should be opened
#' @export
requestStudyParticipation <- function(senderName, projectPath = here::here(), recipientName = NULL, open = TRUE) {
  #get study name
  studyName <- basename(projectPath) %>%
    snakecase::to_upper_camel_case()


  if (is.null(recipientName)) {
    recipientName <- "[Add Name of Recipient]"
  }

  data <- rlang::list2(
    'Study' = studyName,
    'Sender' = senderName,
    'Recipient' = recipientName
  )

  usethis::use_template(
    template = "ParticipationRequestEmail.txt",
    save_as = fs::path("extras", "ParticipationRequestEmail.txt"),
    data = data,
    open = open,
    package = "picard")


  usethis::use_git_ignore(ignores = "extras/ParticipationRequestEmail.txt")

  invisible(data)

}
