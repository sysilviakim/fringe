source(here::here("R", "utilities.R"))

# No need to import raw data ===================================================
categories <- c("winred", "rightus", "actblue")

# Import summarized data, create first/last/min/max ============================
dl <- categories %>%
  set_names(., .) %>%
  map(~ read_fst(here(paste0("data/tidy/portfolio_summ_", .x, ".fst")))) %>%
  map(
    ~ .x %>%
      rename(min_date = min, max_date = max) %>%
      rowwise() %>%
      mutate(
        first = case_when(
          grepl("-", amount) ~ str_match_all(amount, "^([0-9]+)")[[1]][1, 2],
          TRUE ~ amount
        ),
        last = case_when(
          grepl("-", amount) ~ str_match_all(amount, "([0-9]+)$")[[1]][1, 2],
          TRUE ~ amount
        ),
        min = str_match_all(amount, "-([0-9]+)-") %>%
          map(function(x) x[, 2]) %>%
          unlist() %>%
          as.numeric() %>%
          min(),
        max = str_match_all(amount, "-([0-9]+)-") %>%
          map(function(x) x[, 2]) %>%
          unlist() %>%
          as.numeric() %>%
          max(),
        first = as.numeric(first),
        last = as.numeric(last),
        min = min(min, first, last, na.rm = TRUE),
        max = max(max, first, last, na.rm = TRUE)
      )
  )

# Augment "class" variables (WinRed/Right.us) ==================================
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
      grepl("President", race) ~ "president",
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
