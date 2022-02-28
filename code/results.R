

# Compiling official COVID-19, excess and all-cause mortality rates -----------------
# Components
# 1. stmf_excessd (from stmf_inputs.R & stmf_model.R)
# 2. cov_officiald (from covd_inputs.R)
# 3. stmf_allcaused (from stmf_inputs.R & stmf_model.R)

age_int_levels <- c("[0,45)", "[45,55)", "[55,65)", "[65,75)", "[75,85)", "(85+]", "All ages")

mortality_rates <- stmf_excessd %>% 
  bind_rows(cov_officiald, stmf_allcaused) %>%
  filter(Year >= 2020) %>% 
  dplyr::select(iso3c, PopCode, Year, Sex, Age_Int, Age_Lower, Deaths, Population, mr, mra, source) %>%
  mutate(
    Age_Int = factor(Age_Int, levels = age_int_levels, ordered = TRUE), 
    Sex_name = ifelse(Sex == "m", "Males", "Females"), 
    Year = factor(Year, levels = c("2020", "2021"), ordered = TRUE)
  ) %>% ungroup() 

write_csv(mortality_rates, file = "data/covidmort-sex-results.csv")

# Chart styling 

# Color palettes 

color_pal <- list(
                gender = c("Males" = "#560bad", "Females" = "#f72585"), 
                year = c("2021" = "#3a86ff", "2020" = "#ffbe0b")
)

# Experimental 

ggplot(data = mortality_rates %>% filter(source != "All cause deaths" & Year == 2020), 
       aes(x = Age_Int, 
           y = Country, 
           fill = mr)) +
  facet_grid(cols = vars(Sex_name), rows = vars(source), switch = "y") + 
  geom_tile() + 
  scale_fill_viridis_c() + 
  theme_custom()


# FIGURE 1/2: By country: Cumulative crude COVID-19 mortality rate for men and women (excess and official) ----------

acd <- mortality_rates %>% 
  filter(Age_Int == "All ages" & source == "All cause deaths" & Sex == "b") %>% 
  select(iso3c, Year, mr_allcauses = mr)
  
overall_mr <- mortality_rates %>% 
                filter(Age_Int == "All ages" & source != "All cause deaths" & Sex == "b") %>% select(iso3c, Year, mr_b = mr)

chart_df <- mortality_rates %>% 
            filter(Age_Int == "All ages" & source != "All cause deaths" & Sex != "b") %>% 
            inner_join(overall_mr, by = c("iso3c", "Year")) %>% 
            mutate(source = str_wrap(source, 20))

# 2020 

labs = list(
  mti = "2020", 
  x = "Crude mortality rate (per 100,000k)"
)

p1 <- ggplot(data = chart_df %>% filter(Year == 2020), 
       aes(x = mr, 
           y = fct_reorder(Country, mr_b), 
           color = Sex_name, 
           group = iso3c)) +
  facet_wrap(~source, ncol = 2) + 
  geom_line(color = "grey70", size = 0.5) + 
  geom_point(size = 2.75) + 
  guides(color = guide_legend(title = "")) + 
  scale_color_manual(values = color_pal[["gender"]]) +
  labs(
    y = NULL, 
    x = labs[["x"]], 
    title = labs[["mti"]]
  ) + 
  theme_custom() + 
  theme(legend.position = "top", legend.direction = "horizontal")

# 2021

labs = list(
  mti = "2021", 
  x = "Crude mortality rate (per 100k)"
)

p2 <- ggplot(data = chart_df %>% filter(Year == 2021), 
       aes(x = mr, 
           y = fct_reorder(Country, mr_b),  
           color = Sex_name, 
           group = iso3c)) +
  facet_wrap(~source, ncol = 2) + 
  geom_line(color = "grey70", size = 0.5) + 
  geom_point(size =2.75) + 
  guides(color = guide_legend(title = "")) + 
  scale_color_manual(values = color_pal[["gender"]]) +
  labs(
    y = NULL, 
    x = labs[["x"]], 
    title = labs[["mti"]]
  ) + 
  theme_custom() + 
  theme(legend.position = "top", legend.direction = "horizontal")


p1 + p2 + 
plot_annotation(
  title = 'Crude mortality rates',
  caption = "Source: Author's calculations using STMF and COVerAGE databases"
) + plot_layout(guides = 'collect') & theme(legend.position = "top") 

ggsave("charts/FIG_1_2_cmr_by_cntry_sex_year.png", width = 12, height = 7, dpi = 300)

# FIGURE 3/4: By country: Cumulative crude COVID-19 mortality rate, sex ratio --------

chart_df <- mortality_rates %>% 
  filter(Age_Int == "All ages" & source != "All cause deaths" & Sex != "b") %>% 
  pivot_wider(id_cols = c(iso3c, Country, Year, Age_Int, source), values_from = mr, names_from = Sex, names_prefix = "mr_") %>% 
  mutate(
    mr_sexratio = mr_m/mr_f
  ) %>% 
  group_by(iso3c, Year, source) %>% 
  mutate(
    mean_sexratio = mean(mr_sexratio, na.rm = TRUE)
  ) %>% ungroup()

labs = list(
  mti = "Sex ratio in crude 'all-ages' mortality rates", 
  x = "Male:Female ratio in mortality rates, by year", 
  caption = "Source: Author's calculations based on COVerAGE and STMF datasets."
)

ggplot(data = chart_df %>% filter(!is.na(mr_sexratio)), 
       aes(x = mr_sexratio, 
           y = fct_reorder(Country, mean_sexratio), 
           color = Year, 
           group = iso3c)) +
  facet_wrap(~source, ncol = 2, scales = "free_x") + 
  geom_path(color = "grey80", size = 0.5, 
            arrow = arrow(angle = 30, length = unit(0.1, "inches"), ends = "last", type = "open")) + 
  geom_point(size = 2.75) + 
  guides(color = guide_legend(title = "")) + 
  scale_color_manual(values = color_pal[["year"]]) +
  labs(
    y = NULL, 
    x = labs[["x"]], 
    title = labs[["mti"]]
  ) + 
  theme_custom() + 
  #coord_cartesian(xlim = c(0, 2.75)) + 
  theme(legend.position = "top", legend.direction = "horizontal")

ggsave("charts/FIG_3_4_cmrsexratio_by_cntry_year.png", width = 12, height = 7, dpi = 300)


# Alternative presentation (tile)

ggplot(data = chart_df, 
       aes(x = Year, 
           y = fct_reorder(Country, mean_sexratio), 
           fill = mr_sexratio)) +
  facet_grid(cols = vars(source), switch = "y") + 
  geom_tile() + 
  scale_fill_viridis_c() + 
  theme_custom()


# Scatter plot 

chart_df <-  mortality_rates %>% 
  filter(Age_Int == "All ages" & source != "All cause deaths" & Sex != "b") %>% 
  mutate(
    Year = as.character(Year),
    source_code = ifelse(source == "Excess deaths", "excd", "ofcv")
  ) %>% 
  pivot_wider(id_cols = c(iso3c, Country, Year, Age_Int, Sex_name), values_from = mr, names_from = source_code, names_prefix = "mr_") %>% 
  ungroup()


labs = list(
  mti = "Relationship between excess and official COVID-19 crude mortality rates", 
  x = "Excess mortality rate (per 100k)", 
  y = "Official COVID-19 mortality rate (per 100k)"
)

# Version 1 

ggplot(chart_df, 
       aes(x = mr_excd, y = mr_ofcv, color = Sex_name)) + 
  facet_wrap(~Year, nrow = 1, scales = "free") + 
  geom_smooth(method = "lm", se = FALSE) + 
  geom_point() + 
  geom_function(fun = ~.x, color = "grey80") + 
  scale_color_manual(values = color_pal[["gender"]]) +
  #coord_equal() + 
  labs( 
    x = labs[["x"]], 
    y = labs[["y"]]
    ) + 
  theme_custom()

# Version 2
ggplot(chart_df %>% filter(!is.na(mr_excd) & !is.na(mr_ofcv)), 
       aes(x = mr_excd, y = mr_ofcv, color = Year)) + 
  facet_wrap(~Sex_name, nrow = 1, scales = "fixed") + 
  geom_smooth(method = "lm", se = FALSE) + 
  geom_point() + 
  geom_function(fun = ~.x, color = "grey80") + 
  scale_color_manual(values = color_pal[["year"]]) +
  #coord_equal() + 
  labs( 
    title = labs[["title"]], 
    x = labs[["x"]], 
    y = labs[["y"]]
  ) + 
  theme_custom()

ggsave("charts/FIG_11_1v2_cmr_scatter.png", width = 12, height = 7, dpi = 300)

# FIGURE 5/6: Cumulative age-standardized COVID-19 mortality rate for men and women, and sex ratio -----------

asmr <- mortality_rates %>% 
              filter(Age_Int != "All ages" & source != "All cause deaths") %>% 
              left_join(popshares_byage_USA, by = c("Sex", "Age_Lower")) %>% 
              mutate(
                mr = mr*popshare_USA
              ) %>% 
              group_by(iso3c, Country, Year, Sex, source) %>% 
              summarize(
                mr = sum(mr)
              ) %>% 
              mutate(
                Sex_name = ifelse(Sex == "m", "Males", "Females")
              ) %>% ungroup()

overall_mr <- asmr %>% 
  filter(Sex == "b") %>% select(iso3c, Year, source, mr_b = mr)

asmr %>% 
  filter(Sex != "b") %>% 
  inner_join(overall_mr, by = c("iso3c", "Year", "source")) -> chart_df

# 2020 

labs = list(
  mti = "2020", 
  x = "Age standardized mortality rate (per 100k)"
)

p1 <- ggplot(data = chart_df %>% filter(Year == 2020), 
             aes(x = mr, 
                 y = fct_reorder(Country, mr_b), 
                 color = Sex_name, 
                 group = iso3c)) +
  facet_wrap(~source, ncol = 2) + 
  geom_line(color = "grey70", size = 0.5) + 
  geom_point(size = 2) + 
  guides(color = guide_legend(title = "")) + 
  scale_color_manual(values = color_pal[["gender"]]) +
  labs(
    y = NULL, 
    x = labs[["x"]], 
    title = labs[["mti"]]
  ) + 
  theme_custom() + 
  theme(legend.position = "top", legend.direction = "horizontal")

# 2021

labs = list(
  mti = "2021", 
  x = "Age standardized mortality rate (per 100k)"
)

p2 <- ggplot(data = chart_df %>% filter(Year == 2021), 
             aes(x = mr, 
                 y = fct_reorder(Country, mr_b), 
                 color = Sex_name, 
                 group = iso3c)) +
  facet_wrap(~source, ncol = 2) + 
  geom_line(color = "grey70", size = 0.5) + 
  geom_point(size = 2) + 
  guides(color = guide_legend(title = "")) + 
  scale_color_manual(values = color_pal[["gender"]]) +
  labs(
    y = NULL, 
    x = labs[["x"]], 
    title = labs[["mti"]]
  ) + 
  theme_custom() + 
  theme(legend.position = "top", legend.direction = "horizontal")



p1 + p2 + 
  plot_annotation(
    title = 'Age standardized mortality rates',
    caption = "Source: Author's calculations using STMF and COVerAGE databases"
  ) + plot_layout(guides = 'collect') & theme(legend.position = "top") 

ggsave("charts/FIG_5_6_asmr_by_cntry_sex_year.png", width = 12, height = 7, dpi = 300)

# Scatter plot 

chart_df <-  asmr %>% 
  filter(Sex != "b") %>% 
  mutate(
    Year = as.character(Year),
    source_code = ifelse(source == "Excess deaths", "excd", "ofcv")
  ) %>% 
  pivot_wider(id_cols = c(iso3c, Country, Year, Sex_name), values_from = mr, names_from = source_code, names_prefix = "mr_") %>% 
  ungroup()


labs = list(
  mti = "Relationship between excess and official COVID-19 age standardized mortality rates", 
  x = "Excess mortality rate (per 100k)", 
  y = "Official COVID-19 mortality rate (per 100k)"
)


# Version 2
ggplot(chart_df %>% filter(!is.na(mr_excd) & !is.na(mr_ofcv)), 
       aes(x = mr_excd, y = mr_ofcv, color = Year)) + 
  facet_wrap(~Sex_name, nrow = 1, scales = "fixed") + 
  geom_smooth(method = "lm", se = FALSE) + 
  geom_point() + 
  geom_function(fun = ~.x, color = "grey80") + 
  scale_color_manual(values = color_pal[["year"]]) +
  #coord_equal() + 
  labs( 
    title = labs[["title"]], 
    x = labs[["x"]], 
    y = labs[["y"]]
  ) + 
  theme_custom()

ggsave("charts/FIG_11_3v4_asmr_scatter.png", width = 12, height = 7, dpi = 300)



# FIGURE 7/8: By country: Cumulative crude COVID-19 mortality rate, sex ratio --------

chart_df <- asmr %>% 
  pivot_wider(id_cols = c(iso3c, Country, Year, source), values_from = mr, names_from = Sex, names_prefix = "mr_") %>% 
  mutate(
    mr_sexratio = mr_m/mr_f
  ) %>% 
  group_by(iso3c, Year, source) %>% 
  mutate(
    mean_sexratio = mean(mr_sexratio, na.rm = TRUE)
  ) %>% ungroup()


labs = list(
  mti = "Sex ratio in age standardized 'all-ages' mortality rates", 
  x = "Male:Female ratio in mortality rates, by year", 
  caption = "Source: Author's calculations based on COVerAGE and STMF datasets."
)

ggplot(data = chart_df %>% filter(!is.na(mr_sexratio)), 
       aes(x = mr_sexratio, 
           y = fct_reorder(Country, mean_sexratio), 
           color = Year, 
           group = iso3c)) +
  facet_wrap(~source, ncol = 2, scales = "free_x") + 
  geom_path(color = "grey80", size = 0.5, 
            arrow = arrow(angle = 30, length = unit(0.1, "inches"), ends = "last", type = "open")) + 
  geom_point(size = 2.75) + 
  guides(color = guide_legend(title = "")) + 
  scale_color_manual(values = color_pal[["year"]]) +
  labs(
    y = NULL, 
    x = labs[["x"]], 
    title = labs[["mti"]]
  ) + 
  theme_custom() + 
  #coord_cartesian(xlim = c(0, 2.75)) + 
  theme(legend.position = "top", legend.direction = "horizontal")

ggsave("charts/FIG_7_8_asmrsexratio_by_cntry_year.png", width = 12, height = 7, dpi = 300)

