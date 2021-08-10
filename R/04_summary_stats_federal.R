source(here::here("R", "utilities.R"))
congress <- list(
  senate = loadRData(here("data", "tidy", "senate-merged.Rda")),
  house = loadRData(here("data", "tidy", "house-merged.Rda"))
)

