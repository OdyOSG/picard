test_that("Ohdsi project initializes", {

  #check that yml file is creates
  tst <- fs::dir_ls(proj_root, glob = "*.yml")
  expect_true(grepl("config", tst))

  #TODO check that Rproj works
  # nm <- paste0(basename(proj_root), ".Rproj")
  # expect_true(file.exists(file.path(proj_root, nm)))


})
