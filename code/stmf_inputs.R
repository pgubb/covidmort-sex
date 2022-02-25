
cat("Retreiving and Preparing HMD STMF Inputs data")

# Reading STMF Input files --------------------------------------------------------

files <- list.files(path = "data/STMFinput", pattern = ".csv", recursive = TRUE, full.names = TRUE)

clean_up <- function(df) {
  # Helper functions that reads in csv data
  cat(df)
  read_csv(df, guess_max = 1000000) %>% mutate(Week = as.character(Week), Deaths = as.character(Deaths))
}

stmf_inputs <- dplyr::bind_rows(map(files, clean_up)) %>% filter(Year >= MIN_YEAR & Year <= MAX_YEAR) 

# Identifying/ inspecting issues --------------------------------------------------------

# Sweden has character values in the Week column
# NLD has character values (.) in the Deaths column
# There is an "a" in the PopCode column that should be "NOR"

unique(pull(stmf_inputs %>% filter(PopCode == "SWE"), Week))
# Sweden has deaths that are unknown by week - for now ignoring as deaths will be agreggated to the year level 
View(stmf_inputs %>% filter(PopCode == "NLD"))

# Modifications/corrections --------------------------------------------------------

stmf_inputs %>% 
    mutate(
           # Coercing ("UNK") in Week to NA (These will be ignored since analysis will be done at year level)
           Week = as.numeric(ifelse(Week == "UNK", NA, Week)), 
           # Coercing (.) in "Deaths" to NA
           Deaths = as.numeric(Deaths), 
           # Fixing Norway PopCode
           PopCode = ifelse(PopCode == "a", "NOR", PopCode), 
           # Adding in country names
           Country = maps[["stmf_popcode_to_name"]][PopCode], 
           iso3c = maps[["stmf_popcode_to_iso3c"]][PopCode]
           ) -> stmf_inputs


# Creating code to define inclusion criteria for analysis database --------------------------------------------------------

# To be included in the database, countries have to have: 
# 1. death counts for the complete year total number of iso-weeks in a given year are excluded from the analysis 
# 2. age intervals at the exact boundaries specified in AGE_CUTS for both sexes and data before 2020 

# Creating summary table of time coverage (exposure) of deaths by country and year  

stmf_exposures <- stmf_inputs %>%  
  group_by(PopCode, Sex) %>% 
  mutate(max_year = max(Year, na.rm = TRUE)) %>%
  group_by(PopCode, Sex, Year) %>% 
  mutate(max_week = max(Week, na.rm = TRUE)) %>% 
  select(Country, Sex, Year, max_week) %>% 
  filter(row_number() == 1) %>% 
  ungroup() %>% 
  group_by(Year) %>% 
  mutate(max_week_inyr = max(max_week),
         exposure = max_week/max_week_inyr, 
         group = paste(Country, Year, Sex, sep = "-")) %>% ungroup()

# !! Notes: The USA does not have deaths by sex for 2015-2019

# Vector of observations that meet conditions for age_cut inclusion/exclusion for analysis
stmf_excluded_bc_exposure <- unique(pull(stmf_exposures %>% filter(exposure < 0.96), group))

sprintf("Countries without complete data for a given year, sex: %s", paste0(stmf_excluded_bc_exposure, collapse = "; "))

# Creating summary table of age cut points by country and sex  

PopCodes <- unique(stmf_inputs$PopCode)

age_levels <- c("0", "1", "5", "10", "15", "20", "25", "30", "35", "40", "45", "50", "55", "60", "65", "70", "75", "80", "85", "90", "95", "100", "TOT", "UNK") 
int_levels <- c("1", "4", "5", "10", "14", "15", "20", "25", "30", "45", "50", "65", "+", ".")

stmf_ageints <- stmf_inputs %>% 
  group_by(Country, Year, Sex) %>% 
  filter(Week == 1 & !is.na(AgeInterval)) %>% 
  filter(Age != "TOT") %>% 
  mutate(N = 1, 
         group = paste(Country, Sex, sep = "-"), 
         Age_fct = factor(Age, levels = age_levels, ordered = TRUE), 
         age_min = as.numeric(Age), 
         age_max = age_min + as.numeric(AgeInterval),
         AgeInt_fct  = factor(AgeInterval, levels = int_levels, ordered = TRUE), 
         min_age_int = min(as.integer(AgeInterval), na.rm = TRUE), 
         max_age_int = max(as.integer(AgeInterval), na.rm = TRUE),
         age_cutoffs = accumulate(Age, paste, sep=","), 
  ) 


##### CHARTS TO BE SAVED IN "CHARTS" SUBFOLDER
# Visualizing age intervals/groups available for each country/Sex 
source("code/stmf_viz_ageints.R")
#####

include <- stmf_ageints %>% 
  group_by(Country, Year, Sex) %>% 
  filter(row_number()==n()) %>% 
  filter(Sex != "b") %>% 
  select(Country, Year, Sex, min_age_int, max_age_int, age_cutoffs)

for (age in AGE_CUTS) {
  new_col_name <- paste0("cut_", age)
  include <- include %>% 
    mutate(!!sym(new_col_name) := str_detect(age_cutoffs, as.character(age)))
}

include %>% 
  mutate(
    allints = all(across(starts_with("cut_")))
  ) %>% 
  group_by(Country, Sex) %>% 
  add_count() %>% 
  mutate(
    alltrue = sum(allints), 
    min_year = min(Year), 
    max_year = max(Year)
  ) -> include

# Vector of countries that meet conditions for age_cut inclusion/exclusion for analysis
countries_in <- unique(pull(include %>% filter(alltrue == n & min_year < 2020), Country))
stmf_excluded_bc_agecutoffs <- unique(pull(include %>% filter(alltrue < n | min_year >= 2020), Country))

sprintf("Countries without desired age cut-offs: %s", paste0(stmf_excluded_bc_agecutoffs, collapse = "; "))

# Creating harmonized aggregated database (stmf_db) with all available data from 2015-2021 using age-cutoffs defined above --------------------------------------------------------

stmf_inputs %>% 
  # Keeping countries with death counts for 52 or 53 weeks of death counts per year
  inner_join(stmf_exposures %>% select(PopCode, Year, Sex, exposure), by = c("PopCode", "Year", "Sex")) %>% 
  filter(exposure > 0.96) %>% 
  # Keeping countries with death counts at desired age cut-offs
  filter(Country %in% countries_in) %>% 
  group_by(PopCode, Sex, Year, Week) %>%
  # Moving total & unknown deaths by age to new column 
  mutate(
    TOT = max(ifelse(Age == "TOT", Deaths, NA), na.rm = TRUE), 
    UNK = max(ifelse(Age == "UNK", Deaths, NA), na.rm = TRUE)
    ) %>%
  filter(Age != "TOT" & Age != "UNK") %>%
  mutate(
    UNK = ifelse(UNK > 0, UNK, 0), 
    dist = Deaths / sum(Deaths),
    # Allocating unknown deaths using observed age distribution within country, sex, year and week
    Deaths = Deaths + dist * UNK, 
    # Making sure total deaths corresponds to total deaths 
    dist = Deaths / sum(Deaths),
    Deaths = dist * TOT,
    Age = as.integer(Age), 
    Age_Int = cut(Age, breaks = c(AGE_CUTS, 999), labels = AGE_INT_LABELS, right = FALSE, include.lowest = FALSE, ordered_result = TRUE)
    #Age_Int = ifelse(is.na(Age_Int), "(85+]", Age_Int)
  ) -> stmf_db
    
# Aggregating deaths by Country, Sex and Year

stmf_db %>% 
  # Aggregating over  age intervals
  group_by(iso3c, PopCode, Country, Sex, Year, Age_Int) %>% 
  summarize(Deaths = sum(Deaths)) %>% 
  group_by(PopCode, Sex, Age_Int) %>% 
  mutate(
    year_num = row_number()
  ) %>% ungroup() -> stmf_db
  # Merging Peru and Colombia data
  #bind_rows(col_inp_ag0, per_inp_ag0, mex_inp_ag0)





