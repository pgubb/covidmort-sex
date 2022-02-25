
# Downloading World Bank WDI income data (GNI per capita) --------------------------------------------------------

gdppcap <- WDI(country = "all", indicator = c("gni_pcap_ppp" = "NY.GNP.PCAP.PP.CD"), extra = TRUE, start = 2019, end = 2019) %>% 
  mutate(iso3c = as.character(iso3c)) %>% 
  select(iso3c, iso2c, gni_pcap_ppp, income)

iso_codes <- gdppcap %>% select(iso2c, iso3c)

regions <- WDI(country = "all", indicator = c("gni_pcap_ppp" = "NY.GNP.PCAP.PP.CD"), extra = TRUE, start = 2019, end = 2019) %>% 
  mutate(iso3c = as.character(iso3c)) %>% 
  filter(region != "Aggregates") %>% 
  select(iso3c, region)
