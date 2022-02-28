
# Downloading World Bank WDI income data (GNI per capita) --------------------------------------------------------


addons <- tibble(
  iso3c = c("TWN", "GBR_SCO", "GBRTENW", "GBR_NIR"), 
  gni_pcap_ppp = c(55078, 40000, 47880, 29700), 
  income = rep(c("High income"), 4)
)

gdppcap <- WDI(country = "all", indicator = c("gni_pcap_ppp" = "NY.GNP.PCAP.PP.CD"), extra = TRUE, start = 2019, end = 2019) %>% 
  mutate(iso3c = as.character(iso3c)) %>% 
  select(iso3c, gni_pcap_ppp, income) %>% 
  filter(income != "Aggregates") %>% 
  bind_rows(addons)


iso_codes <- gdppcap %>% select(iso2c, iso3c)

regions <- WDI(country = "all", indicator = c("gni_pcap_ppp" = "NY.GNP.PCAP.PP.CD"), extra = TRUE, start = 2019, end = 2019) %>% 
  mutate(iso3c = as.character(iso3c)) %>% 
  filter(region != "Aggregates") %>% 
  select(iso3c, region)
