library(magrittr)
library(usethis)
tmpDir <- tempdir()

#make a new study
dirPath <- newOhdsiStudy(projectName = "test",
                         author = "Jean-Luc Picard",
                         type = "Characterization",
                         directory = tmpDir,
                         open = FALSE)


test_that("make readme works", {

  create_local_project(dir = tmpDir)

  kk <- makeReadMe(projectPath = dirPath, open = FALSE)



})
