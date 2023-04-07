
test_that("Can build picard project", {

  picardProject(projectName = projectName,
                directory = directory, author = author,
                openProject = FALSE)
  ff <- fs::dir_ls(directory, recurse = TRUE) %>% as.character()
  # directory matches
  expect_equal(ff[1], as.character(fs::path(directory, projectName)))

  # directory has analysis folder
  idx <- which(grepl("analysis", ff))
  expect_match(ff[idx], "analysis")

  # directory has input/cohortsToCreate/01_studyPop folder
  idx4 <- which(grepl("input/cohortsToCreate/01_studyPop", ff))
  expect_match(ff[idx4], "input/cohortsToCreate/01_studyPop")

})
