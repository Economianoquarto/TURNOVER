# Main script for reproducing the research fully
# Authors: Caio Matteucci, Gabriel Gaze
# Last script update: 2026-01-25

# install.packages('readxl', 'tidyverse', 'sidrar','plm','ivreg', 'tidyverse', 'writexl', 'readxl', 'purrr, 'tools')

# CLEAN UP WORKSPACE ------------
rm(list = ls())

# close all figure windows created 
graphics.off() 

#load packages (colocar library) --------------------------
library(readxl)
library(tidyverse)
library(sidrar)
library(plm)
library(tidyr)
library(ivreg)
library(tidyverse)
library(writexl)
library(readxl)
library(purrr)
library(tools)
library(broom)
library(fixest)
library(did)
library(etwfe)

# change directory (make sure you're using Rstudio)------------------------
my_dir <- dirname(rstudioapi::getActiveDocumentContext()$path) 
setwd(my_dir)

# list functions in 'R-Fcts' --------------------------
my_R_files <- list.files(path = 'R-Fcts',
                         pattern = '*.R',
                         full.names = TRUE)

#Load all functions in r --------------------------------
sapply(my_R_files, source) 

# run import data Script ------------------
source('01_import_Data.r') 

# run clean data script ----------------
source('02_clean_data.r')

# run models ----------------------------
source('03_build_report_models.r')
