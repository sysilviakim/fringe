source(here::here("R", "fec_cand_fundraiser_title.R"))

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
actblue_summ <- loadRData(
  here("data/tidy/portfolio_summ_actblue_incomplete.Rda")
)

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

# Use candidate dataframes to check for candidates =============================
temp <- actblue_all %>%
  filter(is.na(class_mode)) %>%
  rowwise() %>%
  mutate(
    class_mode = ifelse(
      is.na(class_mode) &
        grepl(cand_names$name_full %>% paste(collapse = "|"), name),
      cand_names$name_full %>%
        map(~ grepl(.x, name)) %>%
        unlist() %>%
        which() %>%
        cand_names[., ] %>%
        .$class,
      class_mode
    )
  )

actblue <- bind_rows(
  actblue_all %>%
    filter(!is.na(class_mode)),
  temp %>% ungroup()
) %>%
  group_by(across(names(actblue_summ))) %>%
  mutate(
    multi_entity = ifelse(n() > 1, TRUE, FALSE),
    multi_entity = sum(multi_entity)
  ) %>%
  slice(1) %>%
  filter(!is.na(year)) %>%
  rename(class = class_mode)

save(actblue, file = here("data/tidy/portfolio_summ_actblue.Rda"))
