test_that("Can build a picard project", {

  projectSpecs <- projectSpecifications(projectName = "test",
                                        path = fs::file_temp(),
                                        configBlocks = "test")

  # Step 1: create project directory
  create_proj_dir(projectSpecs)
  dir_test <- fs::path(projectSpecs$location, projectSpecs$projectName)
  expect_true(fs::dir_exists(dir_test))

  # Step 3: add folders
  create_proj_folders(projectSpecs)
  dir_test <- fs::path(projectSpecs$location, projectSpecs$projectName, "input")
  expect_true(fs::dir_exists(dir_test))

  #Step 4: create config file
  create_config_file(projectSpecs)
  config_test <- fs::path(projectSpecs$location, projectSpecs$projectName, "config.yml")
  expect_equal("test", config::get("projectName", file = config_test))

})
