
####################################################################
# Preparing Brazil data to incorporate into STMF -------------------
####################################################################

# 2015-2019 Historic deaths by year, age and sex -------

bra_inputs_201519 <- read_csv("data/BRA/BRA_historic_deaths_byage_sex.csv") %>% 
  group_by(year, Age) %>% 
  summarize(
    deaths_m = sum(deaths_male, na.rm = TRUE),
    deaths_f = sum(deaths_female, na.rm = TRUE),
    deaths_ind = sum(deaths_ind, na.rm = TRUE)
    #deaths_b = sum(deaths_total, na.rm = TRUE)
  ) %>% 
  pivot_longer(cols = starts_with("deaths_"), names_prefix = "deaths_", names_to = "Sex", values_to = "Deaths") %>% 
  group_by(year, Sex) %>% 
  mutate(
    TOT = max(ifelse(Age == "TOT", Deaths, NA), na.rm = TRUE), 
    UNK = max(ifelse(Age == "UNK", Deaths, NA), na.rm = TRUE)
  ) %>% 
  filter(Age != "TOT" & Age != "UNK") %>%
  mutate(
    # Allocating deaths from unknown age groups to age-groups using known distribution
    UNK = ifelse(UNK > 0, UNK, 0), 
    dist = Deaths / sum(Deaths),
    Deaths = Deaths + dist * UNK
  ) %>% 
  select(-UNK, -dist) %>% 
  ungroup() %>% 
  group_by(year, Age) %>% 
  mutate(
    Age = as.integer(Age), 
    # Allocating deaths in unknown sex group (ind) to known sex groups within year and age using existing distribution
    IND = max(ifelse(Sex == "ind", Deaths, NA), na.rm = TRUE), 
    IND = ifelse(IND > 0, IND, 0), 
    IND_TOT = max(ifelse(Sex == "ind", TOT, NA), na.rm = TRUE)
  ) %>% 
  filter(Sex != "ind") %>% 
  mutate(
    dist = Deaths/sum(Deaths), 
    ind = dist*IND, 
    Deaths = Deaths + dist * IND
  ) %>% 
  group_by(year, Sex) %>% 
  mutate(
    # Making sure total deaths corresponds to total deaths 
    dist = Deaths / sum(Deaths),
    Deaths = dist * TOT
  ) %>% ungroup() %>% rename(Year = year) %>%
  select(-dist, -TOT, -IND, -IND_TOT, -ind)
  
# 2020 deaths by age & sex  -------

ingest_bra_data <- function(gender) { 
  
  file <- sprintf("data/BRA/BRA_2020_deaths_byage_%s.csv", gender)
  
  read_csv(file) %>% 
  rename(year = `ano (uid)`, 
         Sex = `sexo (nome)`, 
         deaths = Ano, 
         agegroup = `grupoetario (nome)`) %>% 
  select(year, Sex, agegroup, deaths) %>% 
  filter(year == "2020") %>% 
  separate(deaths, c("d1", "d2", "d3")) %>% 
  mutate(
    d2 = ifelse(is.na(d2), 0, d2), 
    d3 = ifelse(is.na(d3), 0, d3),
    d1 = as.numeric(d1), 
    d2 = as.numeric(d2), 
    d2 = ifelse(nchar(as.character(d2)) == 2, d2*10, d2), 
    d3 = as.numeric(d3), 
    Deaths = ifelse(d2 == 0, d1, ifelse(d3 == 0, d1*1e3 + d2, d1*1e6 + d2*1e3 + d3)), 
    Year = as.integer(year), 
    Deaths = as.numeric(str_replace_all(Deaths, "[.]", "")), 
    Age = str_sub(agegroup, 1, 2), 
    Age = ifelse(Age == "Br", "UNK", Age), 
    Age = ifelse(Age == "To", "TOT", Age), 
    Age = ifelse(Age == "In", "0", Age)
  ) %>% select(-agegroup, -d1, -d2, -d3) %>% 
  mutate(
    TOT = max(ifelse(Age == "TOT", Deaths, NA), na.rm = TRUE), 
    UNK = max(ifelse(Age == "UNK", Deaths, NA), na.rm = TRUE)
  ) %>% 
  filter(Age != "TOT" & Age != "UNK") %>%
  mutate(
    dist = Deaths / sum(Deaths),
    # Allocting uknown deaths to age-groups
    Deaths = Deaths + dist * UNK, 
    # Making sure total deaths corresponds to total deaths 
    dist = Deaths / sum(Deaths),
    Deaths = dist * TOT, 
    Age = as.integer(Age), 
    Age = ifelse(Age == 1, 0, Age), 
    Sex = ifelse(gender == "male", "m", ifelse(gender == "female", "f", "ind"))
  ) %>% select(-dist, -TOT, -UNK) %>% 
  group_by(Year, Sex, Age) %>% 
  summarize(Deaths = sum(Deaths)) %>% ungroup()

} 

genders <- c("male", "female", "ind")

bra_inputs_2020 <- bind_rows(
  map(genders, ingest_bra_data)
) %>% 
group_by(Age) %>% 
  mutate(
    IND = max(ifelse(Sex == "ind", Deaths, NA), na.rm = TRUE)
  ) %>% 
  filter(Sex != "ind") %>% 
  mutate(
    dist = Deaths / sum(Deaths),
    Deaths = Deaths + dist * IND
  ) %>% 
  select(-dist, -IND) 

# Combining 2015-19 and 2020 data, aggregating data to get "both sexes" group  -------

bra_db <- bra_inputs_201519 %>% 
  bind_rows(bra_inputs_2020) %>% 
  mutate(
    iso3c = "BRA", 
    PopCode = "BRA",
    Country = "Brazil", 
    Age = as.integer(Age), 
    Age_Int = cut(Age, breaks = c(AGE_CUTS[["BRA"]], 999), labels = AGE_LABELS[["BRA"]], right = FALSE, include.lowest = FALSE, ordered_result = TRUE)
  ) %>%
  group_by(iso3c, PopCode, Country, Year, Sex, Age_Int) %>% 
  summarize(Deaths = sum(Deaths)) %>% ungroup()

bra_db_b <- bra_db %>% 
              group_by(iso3c, PopCode, Country, Year, Age_Int) %>% 
              summarize(Deaths = sum(Deaths)) %>% 
              mutate(Sex = "b")

bra_db <- bra_db_b %>% bind_rows(bra_db)

rm(bra_db_b)

