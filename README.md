# picard

<!-- badges: start -->

<!-- badges: end -->

picard is a workflow package that helps initialize and facilitate an OHDSI project by automating start-up tasks and setting basic organization. It draws inspiration from [`usethis`](https://usethis.r-lib.org/) but designed specifically for OHDSI projects.

## Installation

You can install the development version of picard from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("OdyOSG/picard")
```

## How To Use

### Getting Started

`picard` is used to initialize a picard project, a special type of project used for OHDSI studies. A picard project is defined by its directory structure. It contains a .RProj, a config.yml and the following directories: R, input, output and extras. Additionally `picard` will supply a NEWS.md file to maintain changes in the project over time. To start a picard project using `picard` one can follow the example code below:

``` r

library(picard)

#startup 1 - no information
startOhdsiProject("my_ohdsi_project")


# start up 2 - some information
projectSpecs <- picard::projectSpecifications(projectName = "my_ohdsi_project",
                                              path = "~/R/sub_folder",
                                              configBlocks = c("test"))
startOhdsiProject(projectSpecs)
```

### Adding to the Project

The `startOhdsiProject` function is verbose providing information as to what has been created in the OHDSI project. Once the project is created you can begin to add a README.md file, R scripts and cohorts.

#### ReadMe Files

To create a new README file for the project, one can using the following code. This initializes the README with svg tags specifying the project status, phase, cdm version and vocabulary version. It also leaves guidence text to fill out other aspects of the README file. The `newReadMe` function contains dots (...) that take on meta information for the project. The dots input must be supplied in snakecase (i.e. study_lead).

``` r
newReadMe(
  title = "My OHDSI Project",
  study_lead = "John Smith",
  study_type = "Characterization",
  study_start_date = "02/24/2023"
)
```

We can also update the tags in the readme file by using an update function. 
``` r
updateStatus("Paused")
updatePhase("Recruiting")
updateCdmVersion("5.4")
updateVocabularyVersion("5.1")
```


#### R Scripts

To create a new R script, one can use the following example code. This creates a new script for your project with a consistent organization. The first section provides meta information about the script. Similar to the README function it contains dots that take on meta information, which must be supplied in snakecase. The user can also supply a list of dependency packages needed for the script. These will be placed in the "Dependencies" section of the R script. The last section of the R script is the "Script" section where the user supplies the code.

``` r
newRScript(
  scriptName = "BuildCohorts",
  author = "John Smith",
  contact = "john.smith@email.com",
  date = "02/24/2023",
  dependencies = c("tidyverse", "CohortGenerator")
)
```

Sometimes tasks can be part of a pipeline where a single script is not enough. `picard` supplies a function to add an element to your pipeline, shown below. This function takes on the name of the script, the task number, meta information and dependencies. It will create the R script, labelled in a pipelining format (i.e. 01_BuildCohorts.R). It will also create an R script for internal functions (labeled as \_BuildCohorts.R, for example) and a subfolder in the output directory (labelled in a pipeline format output/01_BuildCohorts).

``` r
newPipelineTask(
  scriptName = "BuildCohorts",
  taskNumber = 1,
  author = "John Smith",
  contact = "john.smith@email.com",
  date = "02/24/2023",
  dependencies = c("tidyverse", "CohortGenerator")
)
```
#### Cohorts

We can also add cohort definitions into the picard project, which are needed to run an ohdsi study. For the purposes of `picard` a cohort definition is defined as a valid circe json file that can be serialized to ohdsisql. The `picard` package can upload these cohorts in three ways: 1) via an existing file, 2) as a Capr object or 3) using the cohort Id of a cohort definition from WebApi. The cohorts are uploaded to the pre-created 'input/cohortsToCreate' folder, which contains 9 sub-folders identifying different potential cohort types. Below we show an example of uploading the three kinds of cohorts to a picard project. 

1) Uploading cohort json files

``` r
cohortFiles <- fs::dir_ls("folder_with_cohort_json")
addCohortsFiles(
  type = "covariates",
  files = cohortFiles
)
```

2) Uploading Capr Cohorts

``` r

# Capr template to build cohorts
library(Capr)
library(tidyverse)
condition_covariates <- function(concept_path) {
  
  #set up
  name <- tools::file_path_sans_ext(basename(concept_path))
  conceptSet <- Capr::readConceptSet(path = concept_path, name = name)
  #capr template
  cd <- cohort(
    entry = entry(
      condition(conceptSet),
      observationWindow = continuousObservation(0, 0)
    ),
    exit = exit(
      endStrategy = observationExit()
    )
  )
  return(cd)
}

concept_path <- as.character(fs::dir_ls(here::here("input/conceptSets")))
nm <- tools::file_path_sans_ext(basename(concept_path))
# be sure to name to cohorts in the capr list
caprList <- purrr::map(concept_path, ~condition_covariates(.x)) %>%
  purrr::set_names(nm = nm)

picard::addCohortsCapr(
  type = "other",
  caprList = caprList)
```

3) Uploading cohorts from WebApi

Note: you must set the webApi credentials in your system variables using `usethis::edit_r_environ`. Check within your organization about your WebAPI set up to use this properly.

``` r
cohortIds <- c(384, 386:389)
picard::addCohortsWebApi(
  type = "diagnostics",
  cohortIds = c(384, 386:389),
  webApiBaseUrl = Sys.getenv("WEBAPI_URL"),
  webApiAuthMethod = Sys.getenv("WEBAPI_AUTHMETHOD"),
  webApiUser = Sys.getenv("WEBAPI_USERNAME"),
  webApiPassword = Sys.getenv("WEBAPI_PASSWORD"))
```


#### Add new sub-folders

A picard project is composed of 4 directories: R, input, output and extras. Users can create additional subfolders within this structure. A function is provided to assist in this operation

``` r
addFolder(folderName = "conceptSets", directory = "input")
```
