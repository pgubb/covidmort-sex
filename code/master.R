
################################################################# 
# Project: Estimating official COVID-19 and Excess mortality rates by country, age, sex and year 
# Author: Paul Gubbins 
# Date: February 19, 2022
################################################################# 

# Key parameters --------------------------------------------------------

# 1. Years to include in analysis database (stmf_db)
MIN_YEAR <- 2015
MAX_YEAR <- 2021

# 2. Defining age cutoffs to use for analysis
# Age cuts

AGE_CUTS <- list(
  # Primary Age cutoffs to use for countries in STMF database (this covers 30 countries)
  PRI = c(0, 45, 55, 65, 75, 85), 
  # Secondary Age cutoffs to use for analysis (this covers Australia, England and Wales)
  SEC = c(0, 45, 65, 75, 85), 
  # For individual countries 
  CAN = c(0, 45, 65, 85), 
  BRA = c(0, 40, 50, 60, 70, 80), 
  ECU = c(0, 45, 55, 65), 
  NZL = c(0, 65, 80), 
  KOR = c(0, 65, 75, 85), 
  ISR = c(0, 40, 50, 60, 70, 80)
)

AGE_LABELS <- list(
  PRI = c("[0,45)", "[45,55)", "[55,65)", "[65,75)", "[75,85)", "(85+]"), 
  SEC = c("[0,45)", "[45,65)", "[65,75)", "[75,85)", "(85+]"), 
  CAN = c("[0,45)", "[45,65)", "[65,85)",  "(85+]"), 
  BRA = c("[0,40)", "[40,50)", "[50,60)", "[60,70)", "[70,80)", "(80+]"), 
  ECU = c("[0,45)", "[45,55)", "[55,65)", "(65+]"), 
  NZL = c("[0,65)", "[65,80)", "(80+]"), 
  KOR = c("[0,65)", "[65,75)", "[75,85)", "(85+]"), 
  ISR = c("[0,40)", "[40,50)", "[50,60)", "[60,70)", "[70,80)", "(80+]")
)
  

# Loading packages and functions
source("code/packages.R")

# Loading mapping between country codes and names
source("code/refs.R")

# Loading and preparing population data from UN WPP
source("code/wpp_inputs.R")

# Loading and preparing population data from World Development Indicators (GNI)
source("code/wdi_inputs.R")

# Loading and preparing COVerAGE data
source("code/covdb_inputs.R")

# Loading and preparing Short Term Mortality Fluctuations data (STMF)
source("code/stmf_inputs.R")

# Loading and preparing data from additional countries: 

# --------
# COLOMBIA: 
# Source: DANE all-cause death tabulations ("defunciones no-fetales"; TABLE 12: Deaths by age, sex, department and cause) 
# Coverage: 2015-2020, 2021* (*Provisional and only up to October 2021)
# --------
# PERU
# Source: SINADEF microdata
# Coverage: 2017-2021
# --------
# ECUADOR
# Source: INEC (Instituto nacional de estadistica y censo) / Defunciones Generales https://www.ecuadorencifras.gob.ec/defunciones-generales/
# Coverage: 2017-2021
#---------
# BRAZIL
# Source: 
# Coverage: 2015-2020
#---------
# MEXICO
# Source: INEGI (2015-2020) Mortalidad. Estadísticas de Defunciones Registradas. https://www.inegi.org.mx/programas/mortalidad/#Microdatos
# 2021. 2015-2019 all cause deaths from INEGI microdata files, 2020 all cause deaths from RENAPO
# Coverage: 2015-2020
#---------
# PHILIPPINES
# Source: SINADEF microdata (2017-2021)
# Coverage: 2015-2020
#---------
# USA
# Source: CDC WONDER (underlying cause of death) tabulation tool
# Coverage: 2015-2020, 2021 data available through CDC/NCHS weekly reporting tool (via STMF also)
# NOTES: The mortality data available on CDC WONDER are national mortality and population data produced by National Center for Health Statistics (NCHS) at the Centers for Disease Control and Prevention (CDC). 
# Mortality information is collected by state registries and provided to the National Vital Statistics System. 
# Data are based on death certificates for U.S. residents. Each death certificate contains a single underlying cause of death, and demographic data.
#---------

source("code/col_inputs.R")
source("code/per_inputs.R")
source("code/ecu_inputs.R")
source("code/bra_inputs.R")
source("code/mex_inputs.R")
source("code/phl_inputs.R")
source("code/usa_inputs.R")

# Running excess death model
source("code/stmf_model.R")

# Compiling mortality rates based on official sources & excess deaths 
source("code/results.R")

