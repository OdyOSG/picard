# Study Files ----------------------------


#' Function to create a README file
#' @param projectPath the path to the project
#' @param open toggle on whether the file should be opened
#' @export
makeReadMe <- function(projectPath = here::here(), open = TRUE) {


  data <- rlang::list2(
    'Project' = getStudyDetails("StudyTitle", projectPath = projectPath),
    'Author' = getStudyDetails("StudyLead", projectPath = projectPath),
    'StudyType' = getStudyDetails("StudyType", projectPath = projectPath),
    'StartDate' = getStudyDetails("StudyStartDate", projectPath = projectPath),
    'EndDate' = getStudyDetails("StudyEndDate", projectPath = projectPath),
    'StudyTags' = getStudyDetails("StudyTags", projectPath = projectPath),
    'Protocol' = getStudyDetails("LinksProtocol", projectPath = projectPath),
    'Publications' = getStudyDetails("LinksPublication", projectPath = projectPath),
    'Dashboard' = getStudyDetails("LinksDashboard", projectPath = projectPath)
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

  data <- rlang::list2(
    'Project' = getStudyDetails("StudyTitle", projectPath = projectPath)
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
#' @export
makeConfig <- function(block, database, projectPath = here::here(), open = TRUE) {

  projName <- getStudyDetails("StudyTitle", projectPath = projectPath) %>%
    snakecase::to_lower_camel_case()

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


  data <- rlang::list2(
    'Study' = getStudyDetails("StudyTitle", projectPath = projectPath)
  )

  usethis::use_template(
    template = "CohortDetails.md",
    save_as = fs::path("cohortsToCreate", "cohortDetails.md"),
    data = data,
    open = open,
    package = "picard")

  invisible(data)

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
  new <- write_utf8(save_as, template_contents)
  rstudioapi::navigateToFile(save_as)
  invisible(new)

}

#' Function to create a pipeline task as a Rmd file
#' @param scriptName The name of the analysis script
#' @param configBlock the name of the config block to use for the script
#' @param open toggle on whether the file should be opened
#' @export
makeAnalysisScript <- function(scriptName,
                               configBlock = NULL,
                               projectPath = here::here(),
                               open = TRUE) {


  taskNum <- findStepNumber(dir = "analysis/studyTasks")
  step <- scales::label_number(prefix = "0")(taskNum)
  scriptFileName <- paste(step, scriptName, sep = "_")

  if (is.null(configBlock)) {
    configBlock <- "[Add config block]"
  }

  data <- rlang::list2(
    'Name' = snakecase::to_title_case(scriptName),
    'Author' = getStudyDetails("StudyLead", projectPath = projectPath),
    'Date' = lubridate::today(),
    'FileName' = scriptFileName,
    'Block' = configBlock
  )

  usethis::use_template(
    template = "AnalysisScript.R",
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
    'Author' = getStudyDetails("StudyLead", projectPath = projectPath),
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

# Documentation Files -----------------------

#' Function to create a SAP
#' @param projectPath the path to the project
#' @param open toggle on whether the file should be opened
#' @export
makeStudySAP <- function(projectPath = here::here(), open = TRUE) {

  title <- getStudyDetails("StudyTitle", projectPath = projectPath) %>%
    snakecase::to_title_case()

  data <- rlang::list2(
    'Study' = title,
    'Author' = getStudyDetails("StudyLead", projectPath = projectPath),
    'Date' = lubridate::today()
  )

  usethis::use_template(
    template = "StudySAP.qmd",
    save_as = fs::path("documentation", "StudySAP.qmd"),
    data = data,
    open = open,
    package = "picard")

  invisible(data)

}

#' Function to create a Synopsis file
#' @param org the name of the organization hosting the repo, for example 'ohdsi-studies'.
#' If null defaults to a dummy text
#' @param repo the name of the study repository on github, defaults to the study title
#' @param projectPath the path to the project
#' @param open toggle on whether the file should be opened
#' @export
makeHowToRun <- function(org = NULL, repo = NULL,
                         projectPath = here::here(),
                         open = TRUE) {

  if (is.null(repo)) {
    repo <- snakecase::to_snake_case(getStudyDetails("StudyTitle", projectPath = projectPath))
  }

  if (is.null(org)) {
    org <- "[ORG]"
  }

  data <- rlang::list2(
    'Study' = getStudyDetails("StudyTitle", projectPath = projectPath),
    'Url' = glue::glue("https://github.com/{org}/{repo}"),
    'Org' = org,
    'Repo' = repo
  )


  usethis::use_template(
    template = "HowToRun.md",
    save_as = fs::path("documentation", "HowToRun.md"),
    data = data,
    open = open,
    package = "picard")

  invisible(data)

}

#' R Markdown file to make the study protocol
#' @param projectPath the path to the project
#' @param open toggle on whether the file should be opened
#' @export
makeStudyProtocol <- function(projectPath = here::here(),
                              open = TRUE) {

  data <- rlang::list2(
    'Study' =  getStudyDetails(item = "StudyTitle", projectPath = projectPath),
    'Date' = lubridate::today()
  )

  fileName <- snakecase::to_upper_camel_case(getStudyDetails(item = "StudyTitle", projectPath = projectPath)) %>%
    paste0("Protocol")

  dir_path <- fs::path("documentation", "Protocol") %>%
    fs::dir_create()

  usethis::use_template(
    template = "StudyProtocol.Rmd",
    save_as = fs::path(dir_path, fileName, ext = "Rmd"),
    data = data,
    open = open,
    package = "picard")

  #get Protocol Components and move to folder
  fs::path_package("picard", "templates/Protocol-Components") %>%
    fs::dir_copy(new_path = dir_path, overwrite = TRUE)


  invisible(data)
}

#' R Markdown file to make the contribution guidelines
#' @param projectPath the path to the project
#' @param open toggle on whether the file should be opened
#' @export
makeContribution <- function(projectPath = here::here(),
                             open = TRUE) {

  data <- rlang::list2(
    'Study' = getStudyDetails("StudyTitle", projectPath = projectPath),
    'Lead' = getStudyDetails('StudyLead', projectPath = projectPath)
  )


  usethis::use_template(
    template = "ContributionGuidelines.md",
    save_as = fs::path("documentation", "ContributionGuidelines.md"),
    data = data,
    open = open,
    package = "picard")

  invisible(data)

}

#' Quarto file to make a results report
#' @param projectPath the path to the project
#' @param open toggle on whether the file should be opened
#' @export
makeResultsReport <- function(projectPath = here::here(),
                             open = TRUE) {

  data <- rlang::list2(
    'Title' = getStudyDetails("StudyTitle", projectPath = projectPath),
    'Author' = getStudyDetails('StudyLead', projectPath = projectPath),
    'Date' = lubridate::today()
  )


  usethis::use_template(
    template = "ResultsReport.qmd",
    save_as = fs::path("documentation", "ResultsReport.qmd"),
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

  keyringName <- snakecase::to_snake_case(getStudyDetails("StudyTitle", projectPath = projectPath))
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


#' Function to create a meeting minutes file
#' @param projectPath the path to the project
#' @param open toggle on whether the file should be opened
#' @export
makeMeetingMinutes <- function(projectPath = here::here(), open = TRUE) {

  data <- rlang::list2(
    'Study' = getStudyDetails("StudyTitle", projectPath = projectPath),
    'Author' = getStudyDetails("StudyLead", projectPath = projectPath),
    'Date' = lubridate::today()
  )

  saveName <- glue::glue("minutes_{lubridate::today()}") %>%
    snakecase::to_snake_case()

  usethis::use_template(
    template = "MeetingMinutes.qmd",
    save_as = fs::path("extras/minutes", saveName, ext = "qmd"),
    data = data,
    open = open,
    package = "picard")

  invisible(data)

}
