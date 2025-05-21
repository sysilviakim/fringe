source(here::here("R", "utilities.R"))
load(here("data", "tidy", "mit-tidy.Rda"))
load(here("data", "tidy", "portfolio_summ_federal_first_only.Rda"))
load(here("data", "tidy", "fec_cand_summ_2020.Rda"))

# Merge MIT data and scraped data ==============================================
senate <- mit$senate %>%
  mutate(
    candidate = trimws(gsub("\\s+", " ", candidate)),
    last_name = case_when(
      gsub(",|\\.", "", tolower(word(candidate, -1, -1))) %in%
        c("jr", "sr", "ii", "iii", "iv") ~
        gsub(",|\\.", "", tolower(word(candidate, -2, -2))),
      candidate == "CATHERINE CORTEZ MASTO" ~ "cortez masto",
      candidate == "MERAV BEN DAVID" ~ "ben-david",
      candidate == "CHRIS VAN HOLLEN" ~ "van hollen",
      TRUE ~ gsub(",|\\.", "", tolower(word(candidate, -1, -1)))
    )
  ) %>%
  select(last_name, everything()) %>%
  left_join(
    ., dl$senate %>%
      mutate(
        year = as.numeric(year),
        last_name = tolower(last_name)
      ) %>%
      select(-year) %>%
      ## correct error
      mutate(
        state = case_when(
          last_name == "jordan" & state == "CA" ~ "ID",
          last_name == "murphy" & state == "CA" ~ "LA",
          TRUE ~ state
        )
      )
  ) %>%
  ## Minor candidate; not logged
  filter(candidate != "ANNETTE DAVIS JACKSON")

## Check for missing values
senate %>%
  filter(is.na(url)) %>%
  View()

# Merge with FEC candidate summary =============================================
senate <- left_join(
  senate,
  fec_cand_summ_2020 %>%
    filter(office == "S") %>%
    mutate(last_name = trimws(tolower(word(cand_name, 1, 1, sep = ",")))) %>%
    rename(party_fec = party) %>%
    mutate(
      ## Manchin III ---> Manchin
      last_name = case_when(
        last_name == "manchin iii" ~ "manchin",
        last_name == "ben david" ~ "ben-david",
        last_name == "ben david" ~ "ben-david",
        last_name == "masto" ~ "cortez masto",
        TRUE ~ last_name
      )
    ) %>%
    filter(
      !(cand_name %in% c(
        "HARRIS, JAMES E.", "YOUNG, ALEEM LEFT MR", "JACKSON, JAMES",
        "SCOTT, LAWRENCE", "BROWN, WARREN P", "JACKSON, ANNETTE DAVIS",
        "HARRIS, EUGENE PATTERSON", "GRAYSON, DERRICK E REV"
        ## GRAYSON, DERRICK E REV: only 2016 record, merged to S0GA00658
      ))
    )
)

assert_that(senate %>% filter(is.na(state)) %>% nrow() == 0)

# Party mismatch resolve =======================================================
table(senate$party, senate$party_fec)

senate %>%
  filter(
    (party == "DEMOCRAT" & party_fec != "DEM") |
      (party == "REPUBLICAN" & party_fec != "REP")
  )

## Al Gross *is* non-partisan
senate <- senate %>%
  mutate(
    party = case_when(
      candidate == "AL GROSS" ~ "INDEPENDENT",
      TRUE ~ party
    )
  ) %>%
  select(-party_fec)

save(senate, file = here("data", "tidy", "senate-merged.Rda"))