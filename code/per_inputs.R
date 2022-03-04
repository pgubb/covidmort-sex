
####################################################################
# Preparing Peru data to incorporate into STMF -------------------
####################################################################

per_inputs <- read_delim("data/PER/fallecidos_sinadef.csv", delim = "|", guess_max = 100000)

per_inputs %>% 
  mutate(
    PopCode = "PER", 
    iso3c = "PER", 
    Country = "Peru", 
    Sex = ifelse(SEXO == "FEMENINO", "f", ifelse(SEXO == "MASCULINO", "m", "ind")), 
    Age = ifelse(`TIEMPO EDAD` == "AÑOS", EDAD, 0), 
    Age = ifelse(`TIEMPO EDAD` %in% c("IGNORADO", "SIN REGISTRO"), NA, Age), 
    Age = as.integer(Age), 
    Age_Int = cut(Age, breaks = c(AGE_CUTS[["PRI"]], 999), labels = AGE_LABELS[["PRI"]], right = FALSE, include.lowest = FALSE, ordered_result = TRUE),
    Year = isoyear(FECHA), 
    Deaths = 1
  ) %>% 
  filter(Year >= 2017 & Year <= 2021) %>% 
  group_by(iso3c, PopCode, Country, Year, Sex, Age_Int) %>% 
  summarize(
    Deaths = sum(Deaths)
  ) %>% 
  # Allocating unknown deaths by age group to age groups within sex and year
  mutate(
    UNK = ifelse(is.na(Age_Int), Deaths, NA)
  ) %>% 
  group_by(Year, Sex) %>% 
  mutate(
    UNK = max(UNK, na.rm = TRUE)
    ) %>% 
  filter(!is.na(Age_Int)) %>% 
  mutate(
    UNK = ifelse(is.infinite(UNK), 0, UNK),
    dist = Deaths / sum(Deaths),
    Deaths = Deaths + dist * UNK
  ) %>% 
  # Allocating deaths in unknown sex group to known sex groups within year and age using existing distribution
  mutate(
    IND = max(ifelse(Sex == "ind", Deaths, NA), na.rm = TRUE), 
    IND = ifelse(IND > 0, IND, 0), 
  ) %>% 
  filter(Sex != "ind") %>% 
  mutate(
    dist = Deaths/sum(Deaths), 
    ind = dist*IND, 
    Deaths = Deaths + dist * IND
  ) %>% select(-dist, -ind, -IND, -UNK) -> per_db


per_db_b <- per_db %>% 
  group_by(iso3c, PopCode, Country, Year, Age_Int) %>% 
  summarize(Deaths = sum(Deaths)) %>% 
  mutate(Sex = "b")

per_db <- per_db_b %>% bind_rows(per_db)

rm(per_db_b)

