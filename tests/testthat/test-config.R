test_that("Add config", {

  yml <- file.path(proj_root, "config.yml")
  picard::addConfigBlock("test", config_file = yml)
  config_yml <- yaml::yaml.load_file(yml, eval.expr = FALSE)
  tst <- names(config_yml)[names(config_yml) == "test"]
  expect_equal(tst, "test")

})


test_that("get credentials", {
  yml <- file.path(proj_root, "config.yml")
  executionSettings <- executionSettings("Eunomia", file = yml)
  expect_equal(executionSettings$cdm_schema, "main")

  expect_equal(executionSettings$connectionDetails$dbms, "sqlite")
})
