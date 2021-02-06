source(here::here("R", "utilities.R"))

# Load data + sanity checks ====================================================
entities <- loadRData(here("data/raw/actblue_entities_2020.Rda"))
fundraisers <- loadRData(here("data/raw/actblue_fundraisers_2020.Rda"))

assert_that(all(fundraisers$fund_title == fundraisers$fund_title_hidden))
assert_that(all(grepl("/donate/", fundraisers$fund_url)))

# No duplicates
# URLs ---> some duplicates due to year differences or missing year/race
assert_that(any(duplicated(entities$url)))
assert_that(any(duplicated(fundraisers$fund_title)))
assert_that(nrow(entities[duplicated(entities), ]) == 0)
assert_that(nrow(fundraisers[duplicated(fundraisers), ]) == 0)

# Duplicates in fund_title because there are multi-entity fundraisers e.g., 
# "11 Swing District Democrats in Texas that need your help!"
# Because for 2020 I had not properly scraped the js output, (am doing for 2022)
# there is no way to see which one of them appeared first in a single
# fundraiser page e.g., https://secure.actblue.com/donate/wtptx

# Combine entities and fundraisers =============================================
fundraisers_full <- left_join(
  fundraisers,
  entities %>%
    select(entity_url = url, everything()) %>%
    group_by(entity_url) %>%
    mutate(
      year = dplyr::first(na.omit(year)),
      race = dplyr::first(na.omit(race))
    ) %>%
    dedup() %>%
    # If still duplicated due to entity name change, choose last entry
    slice(n())
) %>%
  select(-fund_title_hidden) %>%
  rename(entity_full = entity) %>%
  # entity_url remains the same
  # but entity name slightly changes e.g.
  # "ActBlue — Raynette Kennedy Weiss — Raynette Kennedy Weiss"
  # and "Raynette Kennedy Weiss" same entity
  # so ignore it and take the first non-NA race_year and race within group
  select(year, race, entity = names, fund_url, fund_title, everything()) %>%
  # Note that race/year can be empty
  # e.g., https://secure.actblue.com/entity/fundraisers/8345
  # Alabama Democratic Party - Federal Account
  # Race ---> simpler "class" e.g., ME-HD-147 ---> state house
  rowwise() %>%
  mutate(
    class = case_when(
      grepl("-Sen$", race) ~ "us senate",
      (grepl("-[0-9]{1,2}$", race) | grepl("-AL$", race)) ~ "us house",
      grepl("-SD-", race) ~ "state senate",
      grepl("-HD-", race) ~ "state house",
      grepl("^President$", race) ~ "pres",
      !is.na(race) ~ "misc",
      TRUE ~ "pac"
    )
  ) %>%
  ungroup() %>%
  select(year, class, everything()) %>%
  select(-race, everything())

# Sanity checks ================================================================
assert_that(nrow(fundraisers_full[duplicated(fundraisers_full), ]) == 0)
assert_that(
  fundraisers_full %>%
    group_by(fund_title, fund_url, entity_full, entity_url) %>%
    filter(n() > 1) %>%
    nrow() == 0
)

# Eventually, I would need to slice or summarize by fund_url
assert_that(any(duplicated(fundraisers_full$fund_url)))

write_fst(fundraisers_full, here("data/tidy/actblue_fundraisers_full.fst"))
