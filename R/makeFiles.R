#' Function to create a README file
#' @param author the name of the person conducting the study
#' @param type what type of study is this, there are three option
#' @param projectPath the path to the project
#' @param open toggle on whether the file should be opened
#' @export
makeStudyMeta <- function(author,
                          type = c("Characterization", "Population-Level Estimation", "Patient-Level Prediction"),
                          projectPath = here::here(),
                          open = TRUE) {


  projName <- basename(projectPath) %>%
    snakecase::to_title_case()
  date <- lubridate::today()

  data <- rlang::list2(
    'Title' = projName,
    'Author' = author,
    'Type' = type,
    'Date' = date
  )

  template_contents <- usethis:::render_template("StudyMeta.yml",
                                                 data = data,
                                                 package = "picard")
  save_as <- fs::path(projectPath, "_study.yml")
  new <- usethis:::write_utf8(save_as, template_contents)
  invisible(new)
  # usethis::use_template(
  #   template = ,
  #   save_as = "_picard.yml",
  #   data = data,
  #   open = open,
  #   package = "picard")
  #
  # invisible(data)

}


#' Function to create a README file
#' @param projectPath the path to the project
#' @param author the name of the person writing the study synopsis
#' @param open toggle on whether the file should be opened
#' @export
makeReadMe <- function(projectPath = here::here(), open = TRUE) {


  data <- rlang::list2(
    'Project' = getPicard("StudyTitle"),
    'Author' = getPicard("StudyLead"),
    'StudyType' = getPicard("StudyType"),
    'StartDate' = getPicard("StudyStartDate"),
    'EndDate' = getPicard("StudyEndDate"),
    'StudyTags' = getPicard("StudyTags"),
    'Protocol' = getPicard("LinksProtocol"),
    'Publications' = getPicard("LinksPublication"),
    'Dashboard' = getPicard("LinksDashboard")
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
makeConfig <- function(block, database, projectPath = here::here(), open = TRUE, type = c("keyring", "fillin")) {

  configType <- match.arg(type, c("keyring", "fillin"))

  projName <- basename(projectPath) %>%
    snakecase::to_snake_case()


  data <- rlang::list2(
    'Project' = projName,
    'Cohort' = paste(projName, database, sep = "_"),
    'Block' = block,
    'Database' = database
  )

  configType <- switch(configType,
                      keyring = "config_keyring.yml",
                      fillin = "config_fillin.yml")

  usethis::use_template(
    template = configType,
    save_as = "config.yml",
    data = data,
    open = open,
    package = "picard")

  usethis::use_git_ignore(ignores = "config.yml")

  invisible(data)
}


#' Function to create a Synopsis file
#' @param projectPath the path to the project
#' @param open toggle on whether the file should be opened
#' @export
makeSynopsis <- function(projectPath = here::here(), open = TRUE) {

  data <- rlang::list2(
    'Title' = getPicard("StudyTitle"),
    'Author' = getPicard("StudyLead"),
    'Date' = lubridate::today()
  )

  usethis::use_template(
    template = "StudySynopsis.qmd",
    data = data,
    open = open,
    package = "picard")

  invisible(data)

}

#' Function to create a pipeline task as a Rmd file
#' @param taskName The name of the task that is being done
#' @param step the number that the task is run as a step in a pipeline
#' @param projectPath the path to the project
#' @param author the name of the person writing the study synopsis
#' @param open toggle on whether the file should be opened
#' @export
makeRmdTask <- function(taskName, step = NULL, projectPath = here::here(), author = NULL, open = TRUE) {

  if (is.null(step)) {
    taskFileName <- taskName
  } else{
    step <- scales::label_number(prefix = "0")(step)
    taskFileName <- paste(step, taskName, sep = "_")
  }

  if (is.null(author)) {
    author <- "[Add Name of Author]"
  }

  data <- rlang::list2(
    'Task' = snakecase::to_title_case(taskName),
    'Author' = author,
    'Date' = lubridate::today(),
    'FileName' = taskFileName
  )


  usethis::use_template(
    template = "PipelineTask.Rmd",
    save_as = fs::path("analysis", taskFileName, ext = "Rmd"),
    data = data,
    open = open,
    package = "picard")

  invisible(data)
}

#' Function to create a pipeline task as an R file
#' @param taskName The name of the task that is being done
#' @param step the number that the task is run as a step in a pipeline
#' @param projectPath the path to the project
#' @param author the name of the person writing the study synopsis
#' @param open toggle on whether the file should be opened
#' @export
makeScriptTask <- function(taskName, step = NULL, projectPath = here::here(), author = NULL, open = TRUE) {

  if (is.null(step)) {
    taskFileName <- taskName
  } else{
    step <- scales::label_number(prefix = "0")(step)
    taskFileName <- paste(step, taskName, sep = "_")
  }

  if (is.null(author)) {
    author <- "[Add Name of Author]"
  }

  data <- rlang::list2(
    'Task' = snakecase::to_title_case(taskName),
    'Author' = author,
    'Date' = lubridate::today(),
    'FileName' = taskFileName
  )


  usethis::use_template(
    template = "PipelineTask.R",
    save_as = fs::path("analysis", taskFileName, ext = "R"),
    data = data,
    open = open,
    package = "picard")

  invisible(data)
}
#' Function to create a pipeline task as an R file
#' @param internalsName The name of the internals file that is being created
#' @param projectPath the path to the project
#' @param author the name of the person writing the study synopsis
#' @param open toggle on whether the file should be opened
#' @export
makeInternals <- function(internalsName, projectPath = here::here(), author = NULL, open = TRUE) {

  intFileName <- paste0("_", internalsName)
  intPath <- fs::path(projectPath, "analysis/R") %>%
    fs::dir_create()



  if (is.null(author)) {
    author <- "[Add Name of Author]"
  }

  data <- rlang::list2(
    'Task' = snakecase::to_title_case(internalsName),
    'Author' = author,
    'Date' = lubridate::today(),
    'FileName' = intFileName
  )


  usethis::use_template(
    template = "Internals.R",
    save_as = fs::path("analysis/R", intFileName, ext = "R"),
    data = data,
    open = open,
    package = "picard")

  invisible(data)

}

#' Function to create a cohort folder in input/cohortsToCreate
#' @param foldernName The name of the new folder
#' @param projectPath the path to the project
#' @export
makeCohortFolder <- function(folderName, projectPath = here::here()) {

  dir_path <- fs::path(projectPath, "input/cohortsToCreate")

  cohortsToCreateFolders <- dir_path %>%
    fs::dir_ls(type = "directory") %>%
    basename()

  lastNumber <- gsub("_.*", "", cohortsToCreateFolders) %>%
    as.integer() %>%
    max()

  folderNumber <- lastNumber + 1L

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
