####################################################################
# Preparing Mexico data to incorporate into STMF -------------------
####################################################################

# Reading, processing and combining INEGI microdata for general deaths from 2015-2019 -------------------
# Defining functioning for reading in and processing the data
ingest_inegi_md <- function(year) {
  
# Loading 2015-2019 death microdata
yr2 <- int(year - 2000)

root <- "data/MEX/MEX_inegi/microdata/"
folder <- sprintf("defunciones_base_datos_%i_dbf/", year)
file <- sprintf("DEFUN%i.DBF", yr2)
path <- paste0(root, folder, file)

cat(path, "\n")

read.dbf(path, as.is = TRUE) %>% 
  # Keep only deaths occuring during calendar year for timer-period of interest
  filter(ANIO_OCUR %in% seq(2015, year, 1)) %>% 
  mutate(
    deaths = 1, 
    DIA_OCURR = ifelse(DIA_OCURR == 99, NA, DIA_OCURR), 
    MES_OCURR = ifelse(MES_OCURR == 99, NA, MES_OCURR), 
    ANIO_OCUR = ifelse(ANIO_OCUR == 9999, NA, ANIO_OCUR), 
    # Assigning the 15th as day of death if year and month are not missing
    DIA_OCURR = ifelse(!is.na(ANIO_OCUR) & !is.na(MES_OCURR) & is.na(DIA_OCURR), 15, DIA_OCURR), 
    # Creating a field for the date of death in ymd format
    Date = ymd(paste(ANIO_OCUR, MES_OCURR, DIA_OCURR, sep = "-")), 
    Week = isoweek(Date), 
    # Creating a field for Age in years at time of death
    yr = EDAD - 4000, 
    Age_int = ifelse(yr < 0, 0, yr), 
    Age_int = ifelse(yr == 998, NA, Age_int), 
    Age = floor(Age_int/5)*5, 
    Age = ifelse(Age >= 85, 85, Age), 
    sex = ifelse(SEXO == 1, "m", ifelse(SEXO == 2, "f", "ind")), 
    file_year = year,
    # Creating a year
    year = ifelse(ANIO_OCUR == 2016 & Week == 53, ANIO_OCUR - 1, ANIO_OCUR), 
  ) %>% select(file_year, sex, ANIO_OCUR, MES_OCURR, DIA_OCURR, Date, Week, EDAD, Age_int, Age, deaths, year)
}

years <- seq(2015, 2019, 1)
mex_inegi_md <- dplyr::bind_rows(map(years, ingest_inegi_md)) 
  

# Loading 2020 deaths from RENAPO -------------------
mex_renapo <- read_csv("data/MEX/MEX_renapo/mex_exmort_feb12.csv") %>% 
              mutate(
                     Week = isoweek(FECHA_DEFUNCION), 
                     year = year(FECHA_DEFUNCION), 
                     Age = floor(EDAD/5)*5, 
                     year = ifelse(year == 2021 & Week == 53, year - 1, year), 
                     sex = ifelse(SEXO == 2, "m", "f")
                     ) 

dim(mex_renapo)
# 1,043,693 deaths represented here
range(mex_renapo$FECHA_DEFUNCION)
# From January 1, 2020 to January 2 2021
unique(mex_renapo$EDAD)
dim(mex_renapo %>% filter(EDAD == 999))
# There are 7,613 deaths in 2020/21 with value of 999 (presumably these are missing ages?) 

# Aggregating deaths by year, sex and age -----------------------------------------------

#2015-19
mex_inegi_md %>% 
  group_by(year, sex, Age) %>% 
  summarize(
    deaths = sum(deaths, na.rm = TRUE)
  ) %>% 
  # Reassigning deaths with an unknown age to age group: 
  mutate(
    UNK_age = max(ifelse(is.na(Age), deaths, NA), na.rm = TRUE), 
    UNK_age = ifelse(is.infinite(UNK_age), 0, UNK_age)
  ) %>% 
  filter(!is.na(Age)) %>% 
  mutate(
    dist = deaths/sum(deaths), 
    deaths = deaths + dist*UNK_age
  ) %>% 
  select(-dist, -UNK_age) %>% 
  group_by(year, Age) %>% 
  mutate(
    # Distributing deaths that are indeterminate by sex to males and females 
    IND = max(ifelse(sex == "ind", deaths, NA), na.rm = TRUE)
  ) %>% 
  filter(sex != "ind") %>% 
  mutate(
    IND = ifelse(is.infinite(IND), 0, IND), 
    dist = deaths/sum(deaths), 
    deaths = deaths + dist * IND
  ) %>% select(-IND, -dist) %>% 
  # Dropping cases with unknown week or age of death (not a very significant number)
  filter(!is.na(year)) %>% 
  ungroup %>% 
  group_by(year, sex, Age) %>% 
  summarize(deaths = sum(deaths)) %>% ungroup -> mex_inputs_201519

#2020
mex_renapo %>% 
  mutate(
    Week = isoweek(FECHA_DEFUNCION), 
    year = ifelse(year == 2021 & Week == 53, year - 1, year), 
    UNK = ifelse(EDAD == 999, 1, 0), 
    deaths = ifelse(EDAD != 999, 1, 0), 
    Age = ifelse(Age >= 85, 85, Age)
    ) %>% 
  group_by(year, sex, Age) %>% 
  summarize(deaths = sum(deaths), 
            UNK = sum(UNK)) %>% 
  ungroup() %>% 
  mutate(UNK = max(UNK), 
         dist = deaths/sum(deaths), 
         deaths = deaths + dist*UNK) %>% select(-UNK, - dist) %>% 
  filter(year == 2020) -> mex_inputs_2020

# All years combined

mex_db <- bind_rows(mex_inputs_201519, mex_inputs_2020) %>% 
  mutate(
    iso3c = "MEX", 
    PopCode = "MEX", 
    Country = "Mexico", 
    Age_Int = cut(Age, breaks = c(AGE_CUTS[["PRI"]], 999), labels = AGE_LABELS[["PRI"]], right = FALSE, include.lowest = FALSE, ordered_result = TRUE)
  ) %>% 
  rename(Year = year, Sex = sex, Deaths = deaths) %>%
  group_by(iso3c, PopCode, Country, Year, Sex, Age_Int) %>% 
  summarize(Deaths = sum(Deaths)) 

mex_db_b <- mex_db %>% 
  group_by(iso3c, PopCode, Country, Year, Age_Int) %>% 
  summarize(Deaths = sum(Deaths)) %>% 
  mutate(Sex = "b")

mex_db <- mex_db_b %>% bind_rows(mex_db) 

rm(mex_db_b)

