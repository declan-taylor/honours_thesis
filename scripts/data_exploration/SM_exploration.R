# To plot soil mositure data

SM <- read_csv(here("data/soil_moisture/2022_SM_ALLSITES.csv")) %>%
  filter(plot > 10) %>%
  mutate(plot = as.factor(plot))