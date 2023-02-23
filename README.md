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

`picard` is used to initialize an OHDSI project. An OHDSI project is defined by its directory structure. It contains a .RProj, a config.yml and the following directories: R, input, output and extras. Additionally `picard` will supply a NEWS.md file to maintain changes in the project over time. To start an OHDSI project using `picard` one can follow the example code below:

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

To create a new README file for the project, one can using the following code. This initializes the README with svg tags specifying the project status, phase, cdm version and vocabulary version. It also leaves guidence text to fill out other aspects of the README file. The `newReadMe` function contains dots (...) that take on meta information for the project. The dots input must be supplied in snakecase (i.e. study_lead).

``` r
newReadMe(
  title = "My OHDSI Project",
  study_lead = "John Smith",
  study_type = "Characterization",
  study_start_date = "02/24/2023"
)
```

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
