
####################################################################
# Preparing Colombia data to incorporate into STMF -------------------
####################################################################

# Reading in tabulations from DANE (defunciones no fetales) for 2015 through 2020 --------

files <- list.files(path = "data/COL", pattern = ".csv", recursive = TRUE, full.names = TRUE)

read_colfiles <- function(file) { 
  year <- str_extract(file, "([0-9]+)")
  read_csv(file) %>% mutate(year = as.numeric(year)) %>% fill(depto)
}

col_inputs <- dplyr::bind_rows(map(files, read_colfiles)) %>% 
              mutate(
                     depto = ifelse(depto == "TOTAL NACIONAL", "Total Nacional", depto), 
                     depto = gsub('[[:digit:]]+', '', depto), 
                     depto = trimws(depto, "left"), 
                     codigo_defuncion = str_extract(causa_defuncion, '[[:digit:]]+')
                     ) %>% 
              filter(!is.na(causa_defuncion)) %>% 
              select(-total) 
  
col_inputs <- 
              col_inputs %>%
              pivot_longer(
                cols = starts_with("age"), 
                names_to = c("age", "sex"), 
                names_prefix = "age_",
                names_sep = "_", 
                values_to = "deaths"
              ) %>% 
             mutate(age_group_5y = ifelse( 
               age %in% c("1hr", "123hr", "16d", "727d", "2829d", "15m", "611m", "1", "24"), 
               "04", 
               age)
             ) %>% 
             group_by(depto, causa_defuncion, codigo_defuncion, age_group_5y, sex, year) %>% 
             summarize(
               deaths = sum(deaths, na.rm = TRUE)
             ) %>% ungroup()

# Preparing version of Colombia dataset to incorporate into STMF -------------------

col_inputs <- col_inputs %>% filter(depto == "Total Nacional" & causa_defuncion == "TOTAL") %>% 
               select(-depto, -causa_defuncion, -codigo_defuncion) %>% 
                # Aggregating by year and sex(men, females and indeterminate)
                group_by(year, age_group_5y, sex) %>% 
                summarize(deaths = sum(deaths, na.rm = TRUE)) %>% 
                ungroup() %>% 
                mutate(
                       Age = case_when(
                         age_group_5y == "04" ~ 0, 
                         age_group_5y == "59" ~ 5, 
                         age_group_5y == "1014" ~ 10, 
                         age_group_5y == "1519" ~ 15, 
                         age_group_5y == "2024" ~ 20, 
                         age_group_5y == "2529" ~ 25, 
                         age_group_5y == "3034" ~ 30, 
                         age_group_5y == "3539" ~ 35, 
                         age_group_5y == "4044" ~ 40, 
                         age_group_5y == "4549" ~ 45, 
                         age_group_5y == "5054" ~ 50, 
                         age_group_5y == "5559" ~ 55, 
                         age_group_5y == "6064" ~ 60, 
                         age_group_5y == "6569" ~ 65, 
                         age_group_5y == "7074" ~ 70, 
                         age_group_5y == "7579" ~ 75, 
                         age_group_5y == "8084" ~ 80, 
                         age_group_5y == "8589" ~ 85, 
                         age_group_5y == "9094" ~ 90, 
                         age_group_5y == "9599" ~ 95, 
                         age_group_5y == "100" ~ 100
                       )
                ) %>% group_by(year, sex) %>% 
                mutate(
                       TOT = max(ifelse(age_group_5y == "tot", deaths, NA), na.rm = TRUE), 
                       UNK = max(ifelse(age_group_5y == "unk", deaths, NA), na.rm = TRUE)
                      ) %>% 
                filter(Age != "TOT" & Age != "UNK") %>%
                mutate(
                  # Allocating deaths in unknown age group to known age groups within year and sex using existing distribution
                  UNK = ifelse(UNK > 0, UNK, 0), 
                  dist = deaths / sum(deaths),
                  deaths = deaths + dist * UNK
                ) %>% ungroup() %>% 
                group_by(year, Age) %>% 
                mutate(
                  # Allocating deaths in unknown sex group to known sex groups within year and age using existing distribution
                  IND = max(ifelse(sex == "ind", deaths, NA), na.rm = TRUE), 
                  IND = ifelse(IND > 0, IND, 0), 
                  IND_TOT = max(ifelse(sex == "ind", TOT, NA), na.rm = TRUE)
                ) %>% 
                filter(sex != "ind") %>% 
                mutate(
                  dist = deaths/sum(deaths), 
                  ind = dist*IND, 
                  deaths = deaths + dist * IND
                ) %>% 
                group_by(year, sex) %>% 
                mutate(
                  TOT = sum(ind) + TOT, 
                  # Making sure total deaths corresponds to total deaths 
                  dist = deaths / sum(deaths),
                  deaths = dist * TOT
                ) %>% ungroup() %>% 
                mutate(
                  Age = as.integer(Age), 
                  Age_Int = cut(Age, breaks = c(AGE_CUTS[["PRI"]], 999), labels = AGE_LABELS[["PRI"]], right = FALSE, include.lowest = FALSE, ordered_result = TRUE)
                ) 

col_db <- col_inputs %>%   
  mutate(
    iso3c = "COL", 
    PopCode = "COL", 
    Country = "Colombia"
  ) %>%
  rename(Year = year, Deaths = deaths, Sex = sex) %>%
  group_by(iso3c, PopCode, Country, Year, Sex, Age_Int) %>% 
  summarize(Deaths = sum(Deaths)) %>% ungroup()

col_db_b <- col_db %>% 
  group_by(iso3c, PopCode, Country, Year, Age_Int) %>% 
  summarize(Deaths = sum(Deaths)) %>% 
  mutate(Sex = "b")

col_db <- col_db_b %>% bind_rows(col_db)

rm(col_db_b)

