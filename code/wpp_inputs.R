

cat("Retreiving and Preparing WPP Population data")

# Downloading World Population Country Codes --------------------------------------------------------

wpp_loccodes <- read_csv("data/WPP/WPP_loc_codes.csv") %>% filter(LocTypeName == "Country/Area") %>% select(country_code = LocID, iso3c = ISO3_Code)

# Preparing United Kingdom Population Projections (2020) --------------------------------------------------------

ukpop_2020 <- read_csv("data/UK_pop/UK_pop_2020prj.csv") %>% 
  filter(!is.na(PopCode)) %>% 
  rename(pop_m = pop_male, pop_f = pop_female, pop_b = pop_total, Age_Lower = Age) %>% 
  mutate(
    pop_m = pop_m*1000, pop_f = pop_f*1000, pop_b = pop_b*1000,
    iso3c = PopCode
  ) %>% 
  mutate(iso3c = PopCode) %>% 
  pivot_longer(cols = starts_with("pop_"), values_to = "Population", names_to = "Sex", names_prefix = "pop_") %>% 
  select(iso3c, Sex, Age_Lower, Population) 

# Preparing World Population Projections (UN) Files --------------------------------------------------------

data(popM)
data(popF)

popM_2020 <- popM %>% select(country_code, name, age, `2020`) %>% rename(pop_m = `2020`)
popF_2020 <- popF %>% select(country_code, name, age, `2020`)  %>% rename(pop_f = `2020`)

wpp_2020 <- inner_join(popM_2020, popF_2020, by = c("country_code", "name", "age")) %>% 
            inner_join(wpp_loccodes, by = c("country_code")) %>% 
              mutate(
               pop_m = pop_m*1000, 
               pop_f = pop_f*1000,
               pop_b = pop_m + pop_f
             ) %>% 
              separate(age, sep = "-", into = c("Age_Lower", "Age_Upper")) %>% 
             mutate(
              Age_Lower = str_replace(Age_Lower, "[+]", ""), 
              Age_Lower = as.numeric(Age_Lower)
            ) %>% 
            pivot_longer(cols = starts_with("pop_"), values_to = "Population", names_to = "Sex", names_prefix = "pop_") %>% 
            select(iso3c, Sex, Age_Lower, Population) %>% 
            bind_rows(ukpop_2020)
  
# Function to prepare WPP population counts for desired countries and age groups 

wpp_prep <- function(cut_selection, data, age_cuts, age_labels, all = FALSE, custom_iso = NULL) { 
  
  if(!all) {
  iso_selection <- if (cut_selection == "PRI") { 
                       c(STMF_COUNTRIES_ISO3C, "USA", "PHL", "MEX", "COL", "PER")
                    } else if (cut_selection == "SEC") { 
                       c("GBRTENW", "AUS")
                    } else { 
                      cut_selection  
                    }
  } else {
  iso_selection <- unique(data$iso3c)
  } 
  
  if (!is.null(custom_iso)) { 
    iso_selection = custom_iso
  }
  
  data %>% 
    filter(iso3c %in% iso_selection) %>% 
    mutate(
      Age_Int = cut(Age_Lower, breaks = c(age_cuts[[cut_selection]], 999), labels = age_labels[[cut_selection]], right = FALSE, include.lowest = FALSE, ordered_result = TRUE), 
      Age_Int = as.character(Age_Int), 
      Age_Int_cut = cut_selection
    ) %>% 
    group_by(Age_Int) %>% 
    mutate(Age_Lower = min(Age_Lower)) %>% 
    group_by(iso3c, Sex, Age_Lower, Age_Int, Age_Int_cut) %>% 
    summarize(Population = sum(Population)) %>% 
    ungroup()
  
  }

populations_2020_stmf <- dplyr::bind_rows(map(names(AGE_CUTS), wpp_prep, data = wpp_2020, age_cuts = AGE_CUTS, age_labels = AGE_LABELS)) 
populations_2020_cov <- wpp_prep(cut_selection = "PRI", data = wpp_2020, age_cuts = AGE_CUTS, age_labels = AGE_LABELS, all = TRUE) 

# Getting age shares of total population by sex for USA for age standardization  ---------------------
# These will be used in a one to many merge to compute age-standardized mortality rates

popUSA_shares <- dplyr::bind_rows(map(names(AGE_CUTS), wpp_prep, data = wpp_2020, age_cuts = AGE_CUTS, age_labels = AGE_LABELS, custom_iso = "USA")) %>% 
  group_by(iso3c, Age_Int_cut, Sex) %>% 
  mutate(
    popshare_USA = Population/sum(Population)
  ) %>% 
  ungroup() %>%
  select(Sex, Age_Lower, Age_Int_cut, popshare_USA) 


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
