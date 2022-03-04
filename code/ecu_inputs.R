
####################################################################
# Preparing Ecuador data to incorporate into STMF -------------------
####################################################################

# Reading in tabulated data on deaths 2015-2020 (Preliminary 2020 data)
# The data come from table 1.2.6, deaths by region, age group and sex. 
ecu_inputs <- read_csv("data/ECU/ecu_tab126_inputs.csv") %>% 
  filter(Region == "Total Nacional") %>% 
  t %>% as.data.frame() %>% 
  rownames_to_column() %>% 
  filter(rowname != "Region") %>% 
  rename(group = rowname, Deaths = V1) %>% 
  separate(group, sep = "_", into = c("Year", "Sex", "Age")) %>% 
  mutate(Year = str_replace(Year, "yr", ""), 
         Sex = str_replace(Sex, "sx", ""), 
         Age = str_replace(Age, "age", ""), 
         Age_c = case_when(
           Age %in% c("01", "14", "59","1014", "1519", "2024", "2529", "3034", "3539", "4044") ~ 0, 
           Age %in% c("4549", "5054") ~ 45, 
           Age %in% c("5559", "6064") ~ 55, 
           Age %in% c("65") ~ 65, 
         ), 
         Deaths = as.integer(Deaths)) %>% 
  group_by(Year, Sex) %>% 
  mutate(
    UNK = max(ifelse(Age == "UNK", Deaths, NA), na.rm = TRUE)
  ) %>% 
  filter(Age != "UNK") %>%
  mutate(
    # Allocating deaths in unknown age group to known age groups within year and sex using existing distribution
    UNK = ifelse(UNK > 0, UNK, 0), 
    dist = Deaths / sum(Deaths),
    Deaths = Deaths + dist * UNK
  ) %>% ungroup() %>% select(-dist, -UNK, -Age) %>% 
  rename(Age = Age_c)

ecu_db <- ecu_inputs %>%
  mutate(
    iso3c = "ECU", 
    PopCode = "ECU",
    Country = "Ecuador", 
    Age_Int = cut(Age, breaks = c(AGE_CUTS[["ECU"]], 999), labels = AGE_LABELS[["ECU"]], right = FALSE, include.lowest = FALSE, ordered_result = TRUE)
  ) %>%
  group_by(iso3c, PopCode, Country, Year, Sex, Age_Int) %>% 
  summarize(Deaths = sum(Deaths)) %>% ungroup()

ecu_db_b <- ecu_db %>% 
  group_by(iso3c, PopCode, Country, Year, Age_Int) %>% 
  summarize(Deaths = sum(Deaths)) %>% 
  mutate(Sex = "b")

ecu_db <- ecu_db_b %>% bind_rows(ecu_db) %>% mutate(Year = as.numeric(Year))

rm(ecu_db_b)



