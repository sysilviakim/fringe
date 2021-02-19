source(here::here("R", "utilities.R"))

# Load president, Senate, House candidate setup files ==========================
source(here("R", "pcan_setup_2020.R"))
source(here("R", "scan_setup_2020.R"))
source(here("R", "hcan_setup_2020.R"))

# Federal candidates (all), but just names =====================================
cand_names <- list(
  cand_pres_2020 %>% mutate(class = "pres"),
  cand_senate_2020 %>% mutate(class = "us senate"),
  cand_house_2020 %>% mutate(class = "us house")
) %>%
  map_dfr(
    ~ .x %>%
      select(first_name, last_name) %>%
      mutate(name_full = paste(first_name, last_name)) %>%
      dedup()
  ) %>%
  mutate(across(contains("names"), tolower)) %>%
  bind_rows(
    .,
    {.} %>%
      filter(name_full == "joseph biden") %>%
      mutate(name_full = "joe biden")
  )

rm(cand_pres_2020, cand_senate_2020, cand_house_2020)
