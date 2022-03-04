####################################################################
# Preparing Philippines data to incorporate into STMF -------------------
####################################################################

phl_inputs <- read_csv("data/PHL/phl_inputs.csv") %>% 
  mutate(
    Deaths = as.integer(str_trim(Deaths)), 
    Age = case_when(
      Age_Group %in% c("Under 1", "1-4", "5-9", "10-14", "15-19", "20-24","25-29", "30-34", "35-39", "40-44") ~ 0, 
      Age_Group %in% c("45-49", "50-54") ~ 45, 
      Age_Group %in% c("55-59", "60-64") ~ 55, 
      Age_Group %in% c("65-69", "70-74") ~ 65, 
      Age_Group %in% c("75-79", "80-84") ~ 75, 
      Age_Group %in% c("85-89", "85 and over", "90-94",  "95 and over") ~ 85, 
      Age_Group %in% "Not Stated" ~ NA_real_
    )
    ) %>% 
    group_by(Year, Sex) %>% 
    mutate(
        UNK = max(ifelse(Age_Group == "Not Stated", Deaths, NA), na.rm = TRUE)
    ) %>% 
    filter(Age_Group != "Not Stated") %>%
    mutate(
        # Allocating deaths in unknown age group to known age groups within year and sex using existing distribution
        UNK = ifelse(UNK > 0, UNK, 0), 
        dist = Deaths / sum(Deaths),
        Deaths = Deaths + dist * UNK
    ) %>% select(-UNK, -dist, -Age_Group) %>% ungroup()


phl_db <- phl_inputs %>%   
  mutate(
    iso3c = "PHL", 
    PopCode = "PHL", 
    Country = "Philippines", 
    Age_Int = cut(Age, breaks = c(AGE_CUTS[["PRI"]], 999), labels = AGE_LABELS[["PRI"]], right = FALSE, include.lowest = FALSE, ordered_result = TRUE)
  ) %>%
  group_by(iso3c, PopCode, Country, Year, Sex, Age_Int) %>% 
  summarize(Deaths = sum(Deaths)) %>% ungroup()
