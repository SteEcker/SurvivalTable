# ==== global.R ====
message("Doing application setup\n")

library(reactable)
library(shiny)
library(bs4Dash)
library(here)
library(survival)
library(survminer)
library(tidyverse)
library(purrr)

message("finished loading libraries \n")

source("./modules/tableModule.R")

source("./ui_elements/body.R")
source("./ui_elements/sidebar.R")



message("finished sourcing modules \n")



# ==== end global.R ====