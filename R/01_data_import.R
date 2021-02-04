source(here::here("R", "utilities.R"))

# Import data ==================================================================
# Manually copied/renamed from solicitR data collection repository
# Seventeen .Rda files; no cleaning; altogether 400mb+

if (!file.exists(here("data/tidy/president_2020.fst"))) {
  categories <- c(
    "president", "senate", "house", "actblue", "rightus", "winred"
  )
  for (c in categories) {
    df_raw <- list.files("data/raw", pattern = c, full.names = TRUE) %>%
      keep(~ grepl("amount", .x)) %>%
      map(loadRData) %>%
      bind_rows() %>%
      as_tibble() %>%
      dedup()
    write_fst(df_raw, here(paste0("data/tidy/", c, "_2020.fst")))
  }
} else {
  # Allow specifying categories outside this script
  if (length(categories) > 1) {
    df_ls <- categories %>%
      set_names(., .) %>%
      map(
        ~ read_fst(here(paste0("data/tidy/", .x, "_2020.fst")))
      )
  } else if (length(categories) == 1) {
    df_raw <- read_fst(here(paste0("data/tidy/", categories, "_2020.fst")))
  }
}
