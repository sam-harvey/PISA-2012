# Introduction

The Programme for International Student Assessment (PISA) is a series of global surveys measuring student performance.
This repo includes code used to process PISA 2012 results provided by the OECD. 
The output data is provided via [kaggle](https://www.kaggle.com/samuelh/pisa-2012).

# Installation

This uses R 3.6.3. Install these versions of [R](https://cran.r-project.org/bin/windows/base/old/3.6.3/) and [Rtools](https://cran.r-project.org/bin/windows/Rtools/history.html). 
Then activate the renv to install dependencies.

```
source('renv/activate.R')
```

To rerun the process used to create the kaggle dataset run.

```
source('setup.R')
source('main.R')
```