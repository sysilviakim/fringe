source(here::here("R", "utilities.R"))

# Import platform summarized data, create first/last/min/max ===================
categories <- c("winred", "rightus", "actblue")
dl <- categories %>%
  set_names(., .) %>%
  map(
    ~ loadRData(here(paste0("data/tidy/portfolio_summ_", .x, ".Rda"))) %>%
      mutate(
        amount = case_when(
          amount == "0-1-2-3" ~ NA_character_,
          amount == "-999" ~ NA_character_,
          TRUE ~ amount
        )
      )
  ) %>%
  map(summ_calc_fxn)

# Augment "class" variables (WinRed/Right.us) ==================================
# ActBlue's added in previous scripts
dl$winred <- dl$winred %>%
  mutate(
    class = case_when(
      grepl("-", race) & !grepl("-SEN", race) ~ "us house",
      grepl("-SEN", race) ~ "us senate",
      grepl("Party", race) ~ "party",
      grepl("President", race) ~ "pres"
    )
  )

dl$rightus <- dl$rightus %>%
  rowwise() %>%
  mutate(
    class = case_when(
      # grepl("Nominee", race) ~ "nominee",
      # grepl("Caucus", race) ~ "caucus",
      grepl("US House", race) ~ "us house",
      grepl("US Senate", race) ~ "us senate",
      grepl("State House", race) ~ "state house",
      grepl("State Senate", race) ~ "state senate",
      grepl("President", race) ~ "pres",
      grepl("National Republican|Party", race) ~ "party",
      grepl("Governor", race) ~ "governor",
      TRUE ~ "misc"
    ),
    # Distinguish it from NA values in WinRed
    url = ifelse(is.na(url), "", url)
  )

# Sanity check  ================================================================
# Need to unify non-federal candidate classifications
dl %>% map(~ table(.x$class, useNA = "ifany"))
save(dl, file = here("data/tidy/portfolio_summ_platforms.Rda"))

# Federal candidates ===========================================================
dl <- loadRData(here("data/tidy/portfolio_summ_federal_first_only.Rda")) %>%
  map(
    ~ .x %>%
      mutate(
        amount = case_when(
          amount == "0-1-2-3" ~ NA_character_,
          amount == "-999" ~ NA_character_,
          TRUE ~ amount
        )
      )
  ) %>%
  map(summ_calc_fxn) %>%
  # Clears out cases where amount wasn't scraped because
  # the scraper didn't work very well
  map(~ filter(.x, !(is.na(amount) & min_date == max_date)))
save(dl, file = here("data/tidy/portfolio_summ_federal_final.Rda"))
