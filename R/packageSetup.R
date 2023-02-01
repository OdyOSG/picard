# Create readme, update readme on new project status


#' generates a ReadMe.md file in the project's home directory. Uses readme_template.md.
#' Asks for user's input
#' @export
addReadMe <- function(){

  readline(prompt="You're going to be asked a few questions about the study.
           Your answers will be used to generate a ReadMe.md file. Press enter to proceed.")
  study_title = readline(prompt="Who is the lead of this study?")
  study_type = readline(prompt="What is the study design type?")
  study_start = readline(prompt="What is the planned/actual starting date?")
  study_publication = readline(prompt="Link to the publication")
  results_demo = readline(prompt="Link to the results demo")
  author = readline(prompt="Who is the lead of this study?")
  collaborators = readline(prompt="Names of other collaborators:")

}


#' updates parameters studyStatusBadge, studyStatusName, recruitStatusBadge, recruitStatusName in the template readme_template file
#' @export
updateProjectStatus <- function(){

  currentStudyStatus = readline(prompt="What is the current status of your study? (Choose one digit and press enter)\n
           1: Initialized\n
           2: Started\n
           3: Suspended\n
           4: Completed\n
           5: Terminated\n")


  currentRecruitmentStatus = readline(prompt="What is your recruitment status? (Choose one digit and press enter)\n
           1: Not yet recruiting\n
           2: Recruiting\n
           3: Enrolling by invitation\n
           4: Study active, not recruiting\n
           5: Skip this question / Don't update my recruitment status\n")


  updateReadMeStatus(currentStudyStatus, currentRecruitmentStatus)

}





updateReadMeStatus <- function(userInputStudy, userInputRecruitment){

    studyStatusTable = read.table('./inst/templates/ohdsi_study/extras/formatSetup/studyStatusBadges.txt',sep=',',header = TRUE)
    studyRecruitTable = read.table('./inst/templates/ohdsi_study/extras/formatSetup/studyRecruitmentBadges.txt',sep=',',header = TRUE)

    studyStatusInfo = studyStatusTable[studyStatusTable['userInput']==userInputStudy]
    studyRecruitInfo = studyRecruitTable[studyRecruitTable['userInput']==userInputRecruitment]

    studyStatusBadge = studyStatusInfo[2]
    studyStatusName = studyStatusInfo[3]
    recruitStatusBadge = studyRecruitInfo[2]
    recruitStatusName = studyRecruitInfo[3]

    if (!is.null(studyStatusBadge)){
      studyStatusUpdate = whisker.render(readLines('./inst/templates/readme_template.md'),
                     list(studyStatusBadge = studyStatusBadge,
                          studyStatusName = studyStatusName
                          )
                    )
      writeLines(studyStatusUpdate, file('ReadMeNew.md'))
    }

    if (!is.null(recruitStatusBadge)){
      recruitStatusUpdate = whisker.render(readLines('./inst/templates/readme_template.md'),
                     list(studyStatusBadge = studyStatusBadge,
                          studyStatusName = studyStatusName,
                          recruitStatusBadge = recruitStatusBadge,
                          recruitStatusName = recruitStatusName
                     )
      )
      writeLines(recruitStatusUpdate, file('ReadMeNew.md'))
    }
}



