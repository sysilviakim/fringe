source(here::here("R", "utilities.R"))
load(here("data", "tidy", "mit-tidy.Rda"))
load(here("data", "tidy", "portfolio_summ_federal_first_only.Rda"))
load(here("data", "tidy", "fec_cand_summ_2020.Rda"))

# Missing House candidates from data collection ================================
# house_supp <- list(
#
# ) %>%
#   bind_rows() %>%
#   mutate(
#     min = as.Date(min),
#     max = as.Date(max),
#     last_name = tolower(last_name)
#   )

# Merge MIT data and scraped data ==============================================
house <- mit$house %>%
  mutate(
    candidate = trimws(gsub("\\s+", " ", candidate)),
    last_name = case_when(
      gsub(",|\\.", "", tolower(word(candidate, -1, -1))) %in%
        c("jr", "sr", "ii", "iii", "iv") ~
      gsub(",|\\.", "", tolower(word(candidate, -2, -2))),
      TRUE ~ gsub(",|\\.", "", tolower(word(candidate, -1, -1)))
    ),
    last_name = gsub("á", "a", last_name),
    last_name = gsub("ñ", "n", last_name),
    last_name = gsub("í", "i", last_name),
    state_cd = gsub("-00", "-0", state_cd)
  ) %>%
  select(last_name, everything()) %>%
  left_join(
    ., dl$house %>%
      mutate(
        year = as.numeric(year),
        last_name = tolower(last_name)
      ) %>%
      select(-year) # %>%
    # bind_rows(., house_supp)
  )

## Check for missing values
house %>%
  filter(is.na(url)) %>%
  View()

# Merge with FEC candidate summary =============================================
house <- left_join(
  house,
  fec_cand_summ_2020 %>%
    filter(office == "H") %>%
    mutate(last_name = trimws(tolower(word(cand_name, 1, 1, sep = ",")))) %>%
    rename(party_fec = party) %>%
    mutate(state_cd = gsub("-00", "-0", paste(state, cd, sep = "-")))
)

assert_that(house %>% filter(is.na(cd)) %>% nrow() == 0)

# Party mismatch resolve =======================================================
table(house$party, house$party_fec)

save(house, file = here("data", "tidy", "house-merged.Rda"))
