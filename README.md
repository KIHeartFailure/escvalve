# R code for the project Prevalence, clinical characteristics and outcomes of heart failure patients with or without isolated or combined mitral and tricuspid regurgitation: insight from the ESC-HFA EORP Heart Failure Long-Term Registry

The aim of this R code is to be transparent and document the data handling 
and statistical analyses performed for the project.

## Language 

English. 

## Data

The data consists of Swedish individual patient data and is not public, 
and therefore no data is stored in this repository. 

## Instructions

The project uses the R package projectTemplate, http://projecttemplate.net/ and 
is structured (and run) accordingly. 
Renv, https://rstudio.github.io/renv/articles/renv.html, is used for 
management of package dependencies.

Since the data is not available the code can not be run as is. 

Workflow: 

1. Run src/load_munge_cache.R or set loading and munging options in 
ProjectTemplate::reload.project() to TRUE in 
reports/Statistical_report_XX.Rmd

2. Knit reports/Statistical_report_XX.Rmd

## Publication

http://doi.org/10.1002/ejhf.2929
