

# Downloading Official COVID-19 death data (Coverage DB) --------------------------------------------------------

# Dataset with daily official COVID-19 deaths, by Country and Sex at 5 year age groups: 
# Deaths in COVerAGE database are cumulative

osf_retrieve_file("7tnfh") %>%
  osf_download(conflicts = "overwrite")

cov_5 <-  read_csv("Output_5.zip",
                   skip = 3,
                   col_types = "ccccciiddd") %>%
  # Keeping only national-level data
  filter(Region == "All" | Region == "Hong Kong") %>% 
  # Harmonizing Hong Kong's country name with other datasets
  mutate(Country = ifelse(Country == "China" & Region == "Hong Kong", "Hong Kong SAR, China", Country))

# Countries in COVerAGE database 
cov_countries_all <- unique(cov_inputs$Country)
cov_countries_n <- length(cov_countries_all)

# Identifying countries without any death data (these will be excluded)
cov_inputs <- cov_5 %>% 
  group_by(Country, Sex) %>% 
  mutate(
    n_obs = n(), 
    n_deaths_miss = sum(ifelse(is.na(Deaths), 1, 0)), 
    all_deaths_miss = ifelse(n_obs == n_deaths_miss, 1, 0)
  ) %>% ungroup()

cov_excluded_bc_missdeaths = unique(pull(cov_inputs %>% filter(all_deaths_miss == 1), Country))

# Other modifications: Country codes, dates, time coverage
cov_inputs %>% 
  filter(all_deaths_miss != 1) %>% 
  mutate(
    iso2c = str_sub(Code, 1, 2), 
    iso2c = ifelse(Region == "Hong Kong", "HK", iso2c), 
    iso2c = ifelse(iso2c == "EQ", "EC", iso2c), 
    iso2c = ifelse(iso2c == "VT", "VN", iso2c), 
    Date = dmy(Date), 
    calYear = year(Date),
    Week = isoweek(Date), 
    Year = case_when(
      calYear == 2021 & Week == 53 ~ 2020, 
      calYear == 2022 & Week == 52 ~ 2021, 
      TRUE ~ calYear
    )) %>% 
    filter(Year >= MIN_YEAR & Year <= MAX_YEAR) -> cov_inputs

cov_exposures_long <- cov_inputs %>% 
  group_by(Country, Sex, Year) %>% 
  mutate(
    min_wk = min(Week, na.rm = TRUE),
    max_wk = max(Week, na.rm = TRUE), 
    min_date = min(Date), 
    max_date = max(Date), 
    ndays = max_date - min_date
  ) %>% 
  filter(row_number() == n()) %>% 
  ungroup() %>%
  select(Country, Sex, Year, min_wk, max_wk) %>% 
  group_by(Year) %>% 
  mutate(
         max_week_inyr = max(max_wk),
         exposure = ((max_wk - min_wk) + 1)/max_week_inyr,
         ) %>% 
  ungroup() %>% 
  select(Country, Sex, Year, min_wk, max_wk, exposure) 

cov_exposures_wide <- cov_exposures_long %>% 
  pivot_wider(
    id_cols = c(Country, Sex), names_from = Year, values_from = c(min_wk, max_wk, exposure)
  )

# Filling dates with missing deaths with deaths from top to bottom (most recent) (Assumes there were no deaths in interim period)
cov_inputs %>% 
  arrange(Country, Sex, Age, Date) %>%
  group_by(Country, Sex, Age) %>%
  fill(Deaths) -> cov_inputs

# Incorporating standardized country codes
cov_inputs %>% 
  # Merging in iso codes
  left_join(iso_codes, by = c("iso2c")) %>% 
  mutate(
    PopCode = maps[["stmf_iso3c_to_popcode"]][iso3c], 
    iso3c = ifelse(Country %in% c("Scotland"), "GBR_SCO", iso3c), 
    iso3c = ifelse(Country %in% c("Northern Ireland"), "GBR_NIR", iso3c), 
    iso3c = ifelse(Country %in% c("Haiti"), "HTI", iso3c), 
    iso3c = ifelse(Country %in% c("Island of Jersey"), "JEY", iso3c)
  ) -> cov_inputs

# Only keeping the last observation per country, year and age group (since deaths are cumulative) 
# and adding in Age Interval variable
cov_inputs %>%
  group_by(Country, Year, Sex, Age) %>% 
  filter(Date == max(Date)) %>% 
  ungroup() %>% 
  group_by(Country, Year) %>% 
  # Creating a variable with total deaths by country and year (across ages and sex)
  mutate(Deaths_Total = sum(Deaths)) %>%
  ungroup() %>% 
  mutate(
    # Creating age groups consistent with STMF data file 
    Age_Int = cut(Age, breaks = c(AGE_CUTS, 999), labels = AGE_INT_LABELS, right = FALSE, include.lowest = FALSE, ordered_result = TRUE)
  ) -> cov_inputs

# Removing countries with less than 1/2 year of data for 2020 and 1 full year of data for 2021 
cov_inputs <- cov_inputs %>% 
              left_join(cov_exposures, by = c("Country", "Sex")) %>% 
              filter(exposure_2020 > 0.5) %>% 
              filter(exposure_2021 > 0.96)

# Deaths aggregated by country, iso-year and sex  ----------------------------

aggoverage_covdb <- function(data, id, time, age, sex, popdata) {
  
  id <- sym(id)
  sex <- sym(sex)
  age <- sym(age)
  time <- sym(time)
    
    data %>% 
      group_by(!!id, !!time, !!sex, !!age) %>% 
      mutate(
        Deaths = sum(Deaths, na.rm = TRUE)
      ) %>%
      filter(row_number() == 1) %>%
      ungroup() %>%
      left_join(popdata %>% select(!!id, !!sex, !!age, Population, Age_Lower), by = c(as_string(id), as_string(sex), as_string(age))) %>% 
      dplyr::select(Country, iso2c, iso3c, PopCode, Year, !!sex, Age_Int = !!age, Age_Lower, Deaths, Population)

}

# Aggregating cumulative deaths over 4 different specification of age group
cov_db <- aggoverage_covdb(cov_inputs, "iso3c", "Year", "Age_Int", "Sex", populations_2020)

# Computing mortality rates by Sex and Age
cov_db <- cov_db %>% 
  left_join(cov_exposures_long, by = c("Year", "Country", "Sex")) %>%
  left_join(adj_factor_85p, by = c("iso3c", "Sex")) %>% 
  mutate(adjfct_85p = ifelse(Age_Lower != 85, 1, adjfct_85p)) %>% 
  mutate(
    #in_stmf = ifelse(Country %in% c(country_names) , 1, 0), 
    source = "Official COVID-19 deaths", 
    mr = (Deaths/(Population*exposure))*1e5,
    # Mortality rate adjusted at the open-age interval
    mra = mr*adjfct_85p
  ) %>% 
  group_by(iso3c, Sex) %>% 
  # Calculating overall mortality rate
  mutate(
    Population_total = sum(Population, na.rm = TRUE), 
    Deaths_total = sum(Deaths, na.rm = TRUE), 
    mr_total = (Deaths_total/(Population_total*exposure))*1e5
  ) %>% ungroup() 

