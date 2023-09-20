#'---
#' title: "Data Extraction"
#' author: 'Author One ^1^, Author Two ^1^'
#' abstract: |
#'  | Import data from an online source for use by the remaining scripts in this
#'  | project.
#' documentclass: article
#' description: 'Manuscript'
#' clean: false
#' self_contained: true
#' number_sections: false
#' keep_md: true
#' fig_caption: true
#' output:
#'  html_document:
#'    toc: true
#'    toc_float: true
#'    code_folding: show
#' ---
#'
#+ init, echo=FALSE, message=FALSE, warning=FALSE
# init ----
# This part does not show up in your rendered report, only in the script,
# because we are using regular comments instead of #' comments
debug <- 0;
knitr::opts_chunk$set(echo=debug>-1, warning=debug>0, message=debug>0);

library(ggplot2); # visualisation
library(GGally);
library(rio);# simple command for importing and exporting data
library(pander); # format tables
library(printr); # automatically invoke pander when tables are detected
library(broom); # standardized, enhanced views of various objects
library(dplyr); # table manipulation
library(fs);    # file system operations

options(max.print=42);
panderOptions('table.split.table',Inf); panderOptions('table.split.cells',Inf);
# download data
if(!file.exists('data.R.rdata')){
  #' # Import the data
  Input_Data <- 'https://physionet.org/static/published-projects/mimic-iv-demo/mimic-iv-clinical-database-demo-1.0.zip';
  dir.create('data',showWarnings = FALSE);
  Zipped_Data <- file.path("data",'tempdata.zip');
  download.file(Input_Data,destfile = Zipped_Data);
  Unzipped_Data <- unzip(Zipped_Data,exdir = 'data') %>% grep('gz$',.,val=T);
  Table_Names <- path_ext_remove(Unzipped_Data) %>% fs::path_ext_remove() %>% basename;
  for(ii in seq_along(Unzipped_Data)) assign(Table_Names[ii],import(Unzipped_Data[ii],format='csv'));
  #mapply(function(aa,bb) assign(aa,import(bb,format='csv'),inherits = T),Table_Names,Unzipped_Data)
  save(list=Table_Names,file='data.R.rdata');
  print('Downloaded')
}else{
  print('File exists')
}

load(file = "data.R.rdata")

democolumns = c('subject_id','insurance', 'marital_status', 'ethnicity')

length_unique = function(x) unique(x) %>% length()

unique_values = function(x) unique(x) %>% sort() %>% paste(sep = '_',collapse = ':')

# vectorize running it by col
sapply(admissions[,democolumns], function(x) x %>% unique() %>% length())


admissions %>% group_by(subject_id) %>%  summarise(across(any_of(democolumns), length_unique))
#" # Demongraphic table

demographics = admissions %>% group_by(subject_id) %>%
  summarise(across(any_of(democolumns), unique_values)
            ,decease = any(!is.na(deathtime))
            ,deathtime = max(deathtime, na.rm = T)
              ) %>%
  mutate(ethnicity_revised = str_replace(ethnicity, 'UNKNOWN;', ''),
         ethnicity_revised_gsub = gsub('UNKNOWN;', '', ethnicity),
         )

demographics[is.infinite(demographics$deathtime), "deathtime"] = NA

demographics %>% left_join(patients[,c("subject_id", "gender","anchor_age")])
