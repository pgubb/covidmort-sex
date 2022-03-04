
cat("Running model to estimate excess deaths by country, sex, age and year (2020, 2021)")

# Aggregating deaths by Country, Sex, Year and Age & incorporating data from Colombia, Peru, Mexico, Philippines, Brazil, Ecuador, USA

stmf_db %>% 
  
  # Aggregating over age intervals
  group_by(iso3c, PopCode, Country, Sex, Year, Age_Int) %>% 
  summarize(Deaths = sum(Deaths)) %>% 
  
  # Merging in additional data
  bind_rows(
    col_db %>% mutate(Age_Int = as.character(Age_Int)), 
    per_db %>% mutate(Age_Int = as.character(Age_Int)), 
    mex_db %>% mutate(Age_Int = as.character(Age_Int)), 
    phl_db %>% mutate(Age_Int = as.character(Age_Int)), 
    bra_db %>% mutate(Age_Int = as.character(Age_Int)), 
    ecu_db %>% mutate(Age_Int = as.character(Age_Int)),
    usa_db %>% mutate(Age_Int = as.character(Age_Int))
  ) %>% 
  group_by(iso3c, Sex, Age_Int) %>% 
  mutate(
    year_num = row_number()
  ) %>% ungroup() -> stmf_db

# Running regressions by country and age_group in a nested data frame  --------------------------------------------------------

# DEFINING KEY MODEL FUNCTIONS

# Model to run on each group defined by unique combination of country-sex-age
country_model <- function(df) { 
  lm(Deaths ~ year_num, data = df)
}

# Function of model fitting and predicting
predict_deaths <- function(train_df, test_df) {
  
  model <- country_model(train_df)
  #predict on full data (including 2020)
  preds <- as_tibble(predict.lm(model, test_df, interval = "prediction"))
  names(preds) <- c("fit", "lwr", "upr")
  return(preds)
  
}

# RUNNING MODEL BY COUNTRY, SEX & AGE

stmf_nested_model <- stmf_db %>% filter(Year < 2020) %>% 
  group_by(iso3c, Sex, Age_Int) %>% 
  nest() %>% 
  rename(train_df = data)

stmf_nested_full <- stmf_db %>% 
  group_by(iso3c, Sex, Age_Int) %>% 
  nest() %>% 
  rename(test_df = data)

stmf_preds <- full_join(stmf_nested_model, stmf_nested_full) %>%
  mutate(
    model = map(train_df, country_model),
    predictions = map2(train_df, test_df, predict_deaths)
  ) %>% unnest(c(test_df, predictions)) 

# Calculating excess and all-cause mortality rates  --------------------------------------------------------

# Merging in population data
stmf_excessd <- stmf_preds %>% 
  left_join(populations_2020_stmf, by = c("iso3c", "Age_Int", "Sex")) %>% 
  select(iso3c, Country, Sex, Age_Int, Age_Lower, Age_Int_cut, Year, Population, Deaths, fit, lwr, upr)

# Aggregating deaths over age by sex to get a "All ages" category by sex
stmf_excessd %>% 
  group_by(iso3c, Country, Year, Sex) %>% 
  summarize(Deaths = sum(Deaths), 
            fit = sum(fit), 
            lwr = sum(lwr), 
            upr = sum(upr), 
            Population = sum(Population)) %>% 
  mutate(Age_Int = "All ages", Age_Lower = NA) %>% 
  bind_rows(stmf_excessd) -> stmf_excessd

# Calculating all cause mortality rates (counter-factual for 2020 and 2021)
stmf_allcaused <- stmf_excessd %>% 
  left_join(adj_factor_85p, by = c("iso3c", "Sex")) %>% 
  mutate(adjfct_85p = ifelse(Age_Lower %not_in% c(80,85), 1, adjfct_85p)) %>% 
  mutate(
    # All cause deaths: 
    Deaths = fit, 
    # Estimated all cause mortality rate
    mr = (fit/Population)*1e5, 
    # Excess death Mortality rate (adjusting open ended age interval)
    mra = mr*adjfct_85p, 
    source = "All cause deaths"
  )

# Calculating excess death mortality rates 
stmf_excessd %>% 
  left_join(adj_factor_85p, by = c("iso3c", "Sex")) %>% 
  mutate(adjfct_85p = ifelse(Age_Lower %not_in% c(80,85), 1, adjfct_85p)) %>% 
  mutate(
    # Excess deaths
    Deaths = Deaths - fit, 
    # Excess death Mortality rate
    mr = (Deaths/Population)*1e5, 
    # Excess death Mortality rate (adjusting open ended age interval)
    mra = mr*adjfct_85p, 
    source = "Excess deaths"
  ) -> stmf_excessd


