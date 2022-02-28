

cat("Retreiving and Preparing WPP Population data")

# Downloading World Population Country Codes --------------------------------------------------------


wpp_loccodes <- read_csv("data/WPP/WPP_loc_codes.csv") %>% filter(LocTypeName == "Country/Area") %>% select(country_code = LocID, iso3c = ISO3_Code)

# Downloading World Population Prospect Files --------------------------------------------------------

data(popM)
data(popF)

popM_2020 <- popM %>% select(country_code, name, age, `2020`) %>% rename(pop_m = `2020`)
popF_2020 <- popF %>% select(country_code, name, age, `2020`)  %>% rename(pop_f = `2020`)

wpp_2020 <- inner_join(popM_2020, popF_2020, by = c("country_code", "name", "age")) %>% 
  mutate(
    pop_m = pop_m*1000, 
    pop_f = pop_f*1000,
    pop_b = pop_m + pop_f
  ) %>% 
  separate(age, sep = "-", into = c("Age_Lower", "Age_Upper")) %>% 
  mutate(
    Age_Lower = str_replace(Age_Lower, "[+]", ""), 
    Age_Lower = as.numeric(Age_Lower),
    Age_Int = cut(Age_Lower, breaks = c(AGE_CUTS, 999), labels = AGE_INT_LABELS, right = FALSE, include.lowest = FALSE, ordered_result = TRUE)
  ) %>% 
  pivot_longer(cols = starts_with("pop_"), values_to = "Population", names_to = "Sex", names_prefix = "pop_") %>% 
  group_by(Age_Int) %>%
  inner_join(wpp_loccodes, by = c("country_code")) %>% 
  select(iso3c, Sex, Age_Lower, Age_Int, Population) 

# Downloading United Kingdom Population Projections (2020) --------------------------------------------------------

ukpop_2020 <- read_csv("data/UK_pop/UK_pop_2020prj.csv") %>% 
  filter(!is.na(PopCode)) %>% 
  rename(pop_m = pop_male, pop_f = pop_female, pop_b = pop_total, Age_Lower = Age) %>% 
  mutate(
    pop_m = pop_m*1000, pop_f = pop_f*1000, pop_b = pop_b*1000,
    iso3c = PopCode,
    Age_Int = cut(Age_Lower, breaks = c(AGE_CUTS, 999), labels = AGE_INT_LABELS, right = FALSE, include.lowest = FALSE, ordered_result = TRUE)
  ) %>% 
  mutate(iso3c = PopCode) %>% 
  pivot_longer(cols = starts_with("pop_"), values_to = "Population", names_to = "Sex", names_prefix = "pop_") %>% 
  select(iso3c, Sex, Age_Lower, Age_Int, Population) 


# Creating joint population database by age for 2020  ---------------------

populations_2020 <- bind_rows(wpp_2020, ukpop_2020) %>%
  group_by(Age_Int) %>% 
  mutate(Age_Lower = min(Age_Lower)) %>% 
  group_by(iso3c, Sex, Age_Lower, Age_Int) %>% 
  summarize(Population = sum(Population)) %>% ungroup() 



# Getting age shares of total population by sex for USA for age standardization  ---------------------
# These will be used in a one to many merge to compute age-standardized mortality rates

popshares_byage_USA <- populations_2020 %>% 
  filter(iso3c == "USA") %>% 
  group_by(iso3c, Sex) %>% 
  mutate(
    Population = ifelse(Population == 0, 1, Population), 
    popshare_USA = Population/sum(Population)
  ) %>% 
  ungroup() %>%
  select(Sex, Age_Lower, popshare_USA) 

# Calculating the adjustment factor for the open-ended age interval by country and sex ------------

# Getting mortality rates for the US for age groups above the age of 85

# Mortality rates for 5 year age groups up to 100+ for USA, for period 2015-2020
us_mort_85p <- read_csv("data/WPP/WPP2019_Life_Table_Medium.csv") %>% 
               filter(Location == "United States of America") %>% 
               filter(Time == "2015-2020") %>% 
               filter(AgeGrpStart >= 85) %>% 
               mutate(
                      iso3c = "USA", 
                      Sex = ifelse(Sex == "Male", "m", ifelse(Sex == "Female", "f", "b"))
                      ) %>%
               select(iso3c, Age_Lower = AgeGrpStart, mx_usa = mx, Sex)

# Age shares of elderly populations by country and sex
pop_shares_85p <- wpp_2020 %>% 
         filter(Age_Lower >= 85) %>% 
         group_by(iso3c, Sex) %>% 
         mutate(
           Population = ifelse(Population == 0, 1, Population), 
           popshare = Population/sum(Population)
           ) %>% 
         ungroup() %>%
         select(iso3c, Sex, Age_Lower, popshare) 

# Age shares of elderly populations by sex for USA
pop_shares_85p_usa  <- pop_shares_85p %>% filter(iso3c == "USA") %>% rename(popshare_usa = popshare) 

# Calculating the adjustment factor

adj_factor_85p <- pop_shares_85p %>% 
  inner_join(pop_shares_85p_usa %>% select(-iso3c), by = c("Sex", "Age_Lower")) %>% 
  inner_join(us_mort_85p %>% select(-iso3c), by = c("Sex", "Age_Lower")) %>% 
  mutate(
    num = popshare_usa*mx_usa, 
    denom = popshare*mx_usa
    ) %>% 
  group_by(iso3c, Sex) %>% 
  summarize(
    num = sum(num), 
    denom = sum(denom), 
    adjfct_85p = num/denom
  ) %>% select(-num, -denom) 
