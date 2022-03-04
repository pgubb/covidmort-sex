
#############################################################################################################################
# Preparing USA data to incorporate into STMF -------------------
# Source: https://wonder.cdc.gov/controller/datarequest/D76;jsessionid=683EDCB2D0FAA48E5EEC766155CB
# Downloaded all-cause death data for 2015-2020
# Note: Provisional All-cause death by sex and age is also available for 2020 and 2021 in the STMF dataset from: 
# https://data.cdc.gov/NCHS/Provisional-COVID-19-Deaths-by-Week-Sex-and-Age/vsak-wrfu
#############################################################################################################################


usa_inputs <- read_tsv("data/USA/Underlying Cause of Death, 1999-2020.txt") %>% 
# Removing rows that correspond to totals ("All ages") 
  filter(!is.na(`Five-Year Age Groups`)) %>% 
  rename(Age_Group = `Five-Year Age Groups Code`) %>% 
  mutate(
    Sex = ifelse(`Gender Code` == "M", "m", ifelse(`Gender Code` == "F", "f", "b")), 
    Sex = ifelse(is.na(Sex), "b", Sex), 
    Age = case_when(
      Age_Group %in% c("1", "1-4", "5-9", "10-14", "15-19", "20-24", "25-29", "30-34", "35-39", "40-44") ~ 0, 
      Age_Group %in% c("45-49", "50-54") ~ 45, 
      Age_Group %in% c("55-59", "60-64") ~ 55, 
      Age_Group %in% c("65-69", "70-74") ~ 65, 
      Age_Group %in% c("75-79", "80-84") ~ 75,
      Age_Group %in% c("85-89", "90-94", "95-99", "100+") ~ 85, 
      Age_Group == "NS" ~ NA_real_
    )
  ) %>% select(Year, Sex, Age, Deaths) %>% 
  # Allocating deaths with unknown ages to ages within Year and Sex
  group_by(Year, Sex) %>% 
  mutate(
    UNK = max(ifelse(is.na(Age), Deaths, NA), na.rm = TRUE)
  ) %>% 
  filter(!is.na(Age)) %>% 
  mutate(
    # Allocating deaths in unknown age group to known age groups within year and sex using existing distribution
    UNK = ifelse(UNK > 0, UNK, 0), 
    dist = Deaths / sum(Deaths),
    Deaths = Deaths + dist * UNK
  ) %>% ungroup() %>% select(-dist, -UNK)
  
usa_db <- usa_inputs %>% 
  mutate(
    iso3c = "USA", 
    PopCode = "USA", 
    Country = "USA", 
    Age_Int = cut(Age, breaks = c(AGE_CUTS[["PRI"]], 999), labels = AGE_LABELS[["PRI"]], right = FALSE, include.lowest = FALSE, ordered_result = TRUE)
  ) %>% 
  group_by(iso3c, PopCode, Country, Year, Sex, Age_Int) %>% 
  summarize(
    Deaths = sum(Deaths, na.rm = TRUE)
  )
  
  
  