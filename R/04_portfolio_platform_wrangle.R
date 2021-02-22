source(here::here("R", "utilities.R"))

# Define function for various summary stats ====================================
summ_fxn <- function(df) {
  df %>%
    rename(min_date = min, max_date = max) %>%
    rowwise() %>%
    mutate(
      first = amount_split(amount) %>% .[1],
      last = amount_split(amount) %>% .[length(.)],
      min = amount_split(amount) %>% min(., na.rm = TRUE),
      max = amount_split(amount) %>% max(., na.rm = TRUE),
      mean = amount_split(amount) %>% mean(., na.rm = TRUE),
      median = amount_split(amount) %>% median(., na.rm = TRUE),
      q1 = amount_split(amount) %>% summary() %>% .[["1st Qu."]],
      q3 = amount_split(amount) %>% summary() %>% .[["3rd Qu."]],
      choices = amount_split(amount) %>% length()
    )
}

# Import platform summarized data, create first/last/min/max ===================
categories <- c("winred", "rightus", "actblue")
dl <- categories %>%
  set_names(., .) %>%
  map(~ loadRData(here(paste0("data/tidy/portfolio_summ_", .x, ".Rda")))) %>%
  map(summ_fxn)

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
  map(summ_fxn) %>%
  map(
    ~ .x %>%
      mutate(
        amount = case_when(
          amount == "0-1-2-3" ~ NA,
          TRUE ~ amount
        )
      )
  )


