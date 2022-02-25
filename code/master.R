

################################################################# 
# Project: Estimating official COVID-19 and Excess mortality rates by country, age, sex and year 
# Author: Paul Gubbins 
# Date: February 19, 2022
################################################################# 

# Key parameters --------------------------------------------------------

# 1. Years to include in analysis database (stmf_db)
MIN_YEAR <- 2015
MAX_YEAR <- 2021

# 2. Age cutoffs to use for analysis 
AGE_CUTS <- c(0, 45, 55, 65, 75, 85)
# Corresponding to "0-44", "45-54", "55-64", "65-74", "75-84", "85+"
AGE_INT_LABELS <- c("[0,45)", "[45,55)", "[55,65)", "[65,75)", "[75,85)", "(85+]")

# Loading packages and functions
source("code/packages.R")

# Loading mapping between country codes and names
source("code/refs.R")

# Loading and preparing population data from UN WPP
source("code/wpp_inputs.R")

# Loading and preparing population data from World Development Indicators (GNI)
source("code/wdi_inputs.R")

# Loading and preparing STMF input data
source("code/stmf_inputs.R")

# Running excess death model
source("code/stmf_model.R")

# Compiling mortality rates based on Official sources & Excess deaths 
source("code/results.R")

