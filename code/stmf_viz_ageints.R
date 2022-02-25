

# Visualizing age intervals/groups available for each country/Sex --------------------------------------------------------

PopCodes <- unique(stmf_inputs$PopCode)

age_levels <- c("0", "1", "5", "10", "15", "20", "25", "30", "35", "40", "45", "50", "55", "60", "65", "70", "75", "80", "85", "90", "95", "100", "TOT", "UNK") 
int_levels <- c("1", "4", "5", "10", "14", "15", "20", "25", "30", "45", "50", "65", "+", ".")

age_ints <- stmf_inputs %>% 
  group_by(Country, Year, Sex) %>% 
  filter(Week == 1 & !is.na(AgeInterval)) %>% 
  filter(Age != "TOT") %>% 
  mutate(N = 1, 
         group = paste(Country, Sex, sep = "-"), 
         Age_fct = factor(Age, levels = age_levels, ordered = TRUE), 
         age_min = as.numeric(Age), 
         age_max = age_min + as.numeric(AgeInterval),
         AgeInt_fct  = factor(AgeInterval, levels = int_levels, ordered = TRUE), 
         min_age_int = min(as.integer(AgeInterval), na.rm = TRUE), 
         max_age_int = max(as.integer(AgeInterval), na.rm = TRUE),
         age_cutoffs = accumulate(Age, paste, sep=",")
  ) 
  
  p0 <- ggplot(data = age_ints %>% filter(Sex == "b"),
               aes(y = Year, color = AgeInt_fct)) +
  facet_wrap(~Country, ncol = 4) +
  geom_linerange(aes(xmin = age_min, xmax = age_max)) +
  geom_point(aes(x = age_min), fill = "white", size = 1.2) +
  geom_vline(xintercept = c(0, 45, 55, 65, 75, 85), color = "black") +
  guides(color = guide_legend(title = "Interval length\n(years)", nrow = 1, direction = "horizontal")) +
  scale_x_continuous(breaks = c(0, 45, 55, 65, 75, 85, 100)) +
  labs(x = NULL, y = NULL, title = "Age intervals by country in STMF dataset") +
  theme_custom(legend.position = "top")

p0 

# Creating chart by sex, to see if there are any differences in age intervals/data availability by sex 

p1 <- ggplot(data = age_ints %>% filter(PopCode %in% PopCodes[1:10]),
             aes(y = Year, color = AgeInt_fct)) +
  facet_grid(rows = vars(Country), cols = vars(Sex), switch = "y") +
  geom_linerange(aes(xmin = age_min, xmax = age_max)) +
  geom_point(aes(x = age_min), fill = "white", size = 1.2) +
  geom_vline(xintercept = c(0, 45, 55, 65, 75, 85), color = "black") +
  guides(color = guide_legend(title = "Interval length\n(years)", nrow = 1, direction = "horizontal")) +
  scale_x_continuous(breaks = c(0, 45, 55, 65, 75, 85, 100)) +
  labs(x = "Age", y = NULL) +
  theme_custom(legend.position = "bottom")

p1 

p2 <- ggplot(data = age_ints %>% filter(PopCode %in% PopCodes[11:20]),
             aes(y = Year, color = AgeInt_fct)) +
  facet_grid(rows = vars(Country), cols = vars(Sex), switch = "y") +
  geom_linerange(aes(xmin = age_min, xmax = age_max)) +
  geom_point(aes(x = age_min), fill = "white", size = 1.2) +
  geom_vline(xintercept = c(0, 45, 55, 65, 75, 85), color = "black") +
  guides(color = guide_legend(title = "Interval length\n(years)", nrow = 1, direction = "horizontal")) +
  scale_x_continuous(breaks = c(0, 45, 55, 65, 75, 85, 100)) +
  labs(x = "Age", y = NULL) +
  theme_custom(legend.position = "bottom")

p2

p3 <- ggplot(data = age_ints %>% filter(PopCode %in% PopCodes[21:30]),
             aes(y = Year, color = AgeInt_fct)) +
  facet_grid(rows = vars(Country), cols = vars(Sex), switch = "y") +
  geom_linerange(aes(xmin = age_min, xmax = age_max)) +
  geom_point(aes(x = age_min), fill = "white", size = 1.2) +
  geom_vline(xintercept = c(0, 45, 55, 65, 75, 85), color = "black") +
  guides(color = guide_legend(title = "Interval length\n(years)", nrow = 1, direction = "horizontal")) +
  scale_x_continuous(breaks = c(0, 45, 55, 65, 75, 85, 100)) +
  labs(x = "Age", y = NULL) +
  theme_custom(legend.position = "bottom")

p3

p4 <- ggplot(data = age_ints %>% filter(PopCode %in% PopCodes[31:length(PopCodes)]),
             aes(y = Year, color = AgeInt_fct)) +
  facet_grid(rows = vars(Country), cols = vars(Sex), switch = "y") +
  geom_linerange(aes(xmin = age_min, xmax = age_max)) +
  geom_point(aes(x = age_min), fill = "white", size = 1.2) +
  geom_vline(xintercept = c(0, 45, 55, 65, 75, 85), color = "black") +
  guides(color = guide_legend(title = "Interval length\n(years)", nrow = 1, direction = "horizontal")) +
  scale_x_continuous(breaks = c(0, 45, 55, 65, 75, 85, 100)) +
  labs(x = "Age", y = NULL) +
  theme_custom(legend.position = "bottom")

p4

sumtable <- age_ints %>% 
  group_by(Country, Sex) %>% 
  mutate(
    coverage = paste0(min(Year), "-", max(Year))
  ) %>% 
  group_by(Country, Year, Sex) %>% 
  mutate(
    min_age_int = min(as.integer(AgeInterval), na.rm = TRUE), 
    max_age_int = max(as.integer(AgeInterval), na.rm = TRUE),
    age_cutoffs = accumulate(Age, paste, sep=",")
  )  %>% filter(row_number()==n()) %>% 
  select(Country, coverage, min_age_int, max_age_int, age_cutoffs)
