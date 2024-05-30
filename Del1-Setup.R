#----------------------------------------------------------
# OPSÆTNING 
#----------------------------------------------------------
# Start med at cleare environment
rm(list = ls())

# Indlæs pakker
library(tidyverse)
library(ggplot2)
library(GGally)
library(Hmisc)
library(ltm)
library(cowplot)
library(mapDK)
library(scales)
library(stargazer)
library(xtable)
library(cjpowR)
library(tidybayes)
library(cregg)
library(marginaleffects)
library(survey)
library(ggeffects)

select = dplyr::select
summarize = dplyr::summarize
summarise = dplyr::summarise

# Set working directory
setwd(getwd())

#----------------------------------------------------------
# GLOBALE VARIABLE 
#----------------------------------------------------------

# Farvekoder til konsistent brug i specialet
c_red = "#ae282c"
c_green = "#4AA331"
c_lightblue = "#8ec1da"
c_blue = "#2066a8"
c_darkgrey = "grey30"
c_grey = "grey50"
c_lightgrey = "grey90"
c_black = "grey5"
c_purple = "#800074"
c_yellow = "#f2c45f"
c_ku_red = "#E6784B"
c_ku_darkred = "#AB4911"

