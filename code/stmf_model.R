
cat("Running model to estimate excess deaths by country, sex, age and year (2020, 2021)")


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

# RUNNING MODEL 

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

# Incorporating population data 

stmf_preds %>% 
  left_join(populations_2020, by = c("iso3c", "Age_Int", "Sex")) %>%
  left_join(adj_factor_85p, by = c("iso3c", "Sex")) %>% 
  mutate(adjfct_85p = ifelse(Age_Lower %not_in% c(80,85), 1, adjfct_85p)) %>% 
  mutate(
    Deaths_2019 = max(ifelse(Year == 2019, Deaths, NA), na.rm = TRUE), 
    # Excess deaths
    ed_n = Deaths - fit, 
    # Excess death Mortality rate
    mr = (ed_n/Population)*1e5, 
    # Excess death Mortality rate (adjusting open ended age interval)
    mra = mr*adjfct_85p, 
    # All-cause mortality rate
    amr = (fit/Population)*1e5, 
    amr = amr*adjfct_85p, 
    source = "Vital statistics (Excess deaths)"
  ) -> stmf_preds



