source(here::here("R", "utilities.R"))

# By platform ==================================================================
dl <- loadRData(here("data/tidy/portfolio_summ_platforms.Rda")) %>%
  map(
    ~ .x %>%
      ungroup() %>%
      filter(!is.na(url) & url != "") %>%
      select(-contains("name_full")) %>%
      # This prevents data collection errors (one-time misses) from taking over
      filter(!(is.na(amount) & min_date == max_date))
  )

# By race ======================================================================
dl <- loadRData(here("data/tidy/portfolio_summ_federal_final.Rda")) %>%
  map(~ .x %>% filter(!(is.na(amount) & min_date == max_date)))

