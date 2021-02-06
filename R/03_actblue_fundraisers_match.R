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
      select(first_name, last_name, class) %>%
      mutate(name_full = paste(first_name, last_name)) %>%
      dedup()
  ) %>%
  mutate(across(contains("name"), tolower)) %>%
  bind_rows(
    .,
    {.} %>%
      filter(name_full == "joseph biden") %>%
      mutate(name_full = "joe biden")
  )
rm(cand_pres_2020, cand_senate_2020, cand_house_2020)

# Read ActBlue entities, dedup. and match ======================================
actblue_entities <- read_fst(
  here("data/tidy/actblue_fundraisers_full.fst")
) %>%
  rename(url = fund_url) %>%
  # group by URL, determine whether joint and what type
  group_by(url, fund_title) %>%
  summarize(
    class_mode = Mode(class, na.rm = TRUE),
    class_n = n_distinct(class, na.rm = TRUE),
    entity_n = n(),
    year_race = year[n()]
  ) %>%
  group_by(url) %>%
  # If title/designation changed, most recent one
  slice(n())

# Read amount summary for ActBlue ==============================================
# actblue <- read_fst(here(paste0("data/tidy/actblue_2020.fst")))
actblue_summ <- read_fst(here("data/tidy/portfolio_summ_actblue.fst"))

# Join just on URLs
# Too risky to pool by name
actblue_all <- full_join(
  actblue_summ,
  actblue_entities %>%
    mutate(url = paste0("https://secure.actblue.com", url))
) %>%
  dedup() %>%
  mutate(name = trimws(tolower(name))) %>%
  arrange(name, url)
