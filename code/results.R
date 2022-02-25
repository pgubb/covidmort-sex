
# Compiling results 
mortality_rates <- stmf_preds %>% 
  dplyr::select(iso3c, PopCode, Year, Sex, Age_Int, Age_Lower, Deaths = ed_n, mr, mra, source) %>%
  bind_rows(cov_db %>% dplyr::select(iso3c, PopCode, Year, Sex, Age_Int, Age_Lower, Deaths, mr, mra, source)) %>% 
  mutate(
    zmr = ifelse(mra <= 0, 0.001, mra), 
    lmr = log(zmr)
  ) %>% filter(Year >= 2020)


# Visualizing excess death mortality rates by age and sex 
ggplot(data = mortality_rates %>% filter(Year == 2020 & Age_Lower > 0 & source == "Vital statistics (Excess deaths)"), 
       aes(x = Age_Lower, 
           y = lmr, 
           color = Sex, 
           group = Sex)) +
  facet_wrap(~iso3c) + 
  geom_point() + 
  geom_line() + 
  theme_custom()

# Looking at the ratio of male to female mortality rates 

chart_df <- mortality_rates %>%
            pivot_wider(id_cols = c(iso3c, Year, Age_Int, source), values_from = mra, names_from = Sex, names_prefix = "mra_") %>% 
            mutate(
              mr_sexratio = mra_m/mra_f
            )

# Heat map showing sex ratio of mortality rates, by country, year, age group and source

ggplot(chart_df %>% filter(source == "Official COVID-19 deaths"), 
       aes(x = Age_Int, y = iso3c, fill = mr_sexratio)) +
  facet_grid(cols = vars(Year)) + 
  geom_tile() + 
  scale_fill_viridis_c() + 
  theme_custom()

ggplot(chart_df %>% filter(source == "Official COVID-19 deaths"), 
       aes(x = Age_Int, y = mr_sexratio, group = iso3c)) +
  facet_grid(cols = vars(Year)) + 
  geom_line() + 
  geom_hline(yintercept = 1) + 
  scale_fill_viridis_c() + 
  theme_custom()