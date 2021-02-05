source(here::here("R", "utilities.R"))

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
actblue_summ <- read_fst(here("data/tidy/portfolio_summ_actblue.fst"))

# Join just on URLs
temp <- full_join(
  actblue_summ,
  actblue_entities %>%
    mutate(url = paste0("https://secure.actblue.com", url))
) %>%
  dedup()

amount_missing <- temp %>%
  filter(is.na(amount))

# Supplementary data for missing amounts =======================================
js_supp <- amount_supp <- vector("list", nrow(amount_missing))
for (i in seq(nrow(amount_missing))) {
  tryCatch(
    {
      amount_supp[[i]] <- fundraising_actblue(amount_missing$url[i])
    },
    error = function(e) {
      amount_supp[[i]] <- NA
      message(e)
    }
  )
  Sys.sleep(5)
  tryCatch(
    {
      js_supp[[i]] <- actblue_js(amount_missing$url[i])
    },
    error = function(e) {
      js_supp[[i]] <- NA
      message(e)
    }
  )
  Sys.sleep(5)
  message(i)
}
save(js_supp, file = "data/raw/actblue_js_supp_2020.Rda")

js_supp %>%
  map(is.null) %>%
  unlist() %>%
  which() %>%
  amount_missing[., ] %>%
  View()
# 45 unavailable due to 404 error

# List to dataframe, save ======================================================
amount_supp <- seq(nrow(amount_missing)) %>%
  map_dfr(
    ~ {
      if (!is.null(amount_supp[[.x]])) {
        tibble(
          fundraiser = amount_missing$fund_title[.x],
          url = amount_missing$url[.x],
          date = Sys.Date(),
          portfolio = amount_supp[[.x]]
        )
      } else {
        NULL
      }
    }
  )

save(
  amount_supp,
  file = here("data/raw/actblue_amount_scraped_2020_supp.Rda")
)
