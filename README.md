
# picard

<!-- badges: start -->
<!-- badges: end -->

picard is a package that helps develop an OHDSI study project. It draws inspiration from [`usethis`](https://usethis.r-lib.org/) but specializing in OHDSI projects. 

## Installation

You can install the development version of picard from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("OdyOSG/picard")
```

## How to Use

picard can be used to initialize an OHDSI project, setup database connections, create input and output files for the project and create a github repository. 

An example of the workflow can be seen here:

``` r

# Step 1: create a new ohdsi project
picard::ohdsi_project("<path_to_directory")
# if done outside of RStudio need to also create project: usethis::create_project()

# Step 2: Edit config.yml file
picard::edit_config()

# Step 3: set keyring credentials
config_block <- "<config_block>"
picard::set_credentials(config_block)
picard::check_credentials(config_block)

# Step 4: create input directory
path_to_cohorts <- "<path_to_cohorts>" # a directory with circe json files
picard::input_directory(cohort_path = path_to_cohorts)

# Step 5: create output directory
outpur_folder_names <- c("cohort_build", "diagnostics", "baseline")
picard::output_directory(output_folder_names = output_folder_names)

# Step 6: Execute codeToRun.R File

# Step 7: Publish project to github repository

#before running this step remember to setup github access in Rstudio

picard::picard::start_github_repo(
  author = "Martin Lavallee",
  study_type = "Characterization",
  organization = "OdyOSG",
  private = TRUE
)

```

## Future Features

- Functions to support `renv`
- Setting up different ohdsi study types
- More support to writing config.yml file
- Functions to set up results exchange
