source(here::here("R", "utilities.R"))

# Import data ==================================================================
# Manually copied/renamed from solicitR data collection repository
# Seventeen .Rda files; no cleaning; altogether 400mb+

if (!file.exists(here("data/tidy/president_2020.fst"))) {
  categories <- c(
    "president", "senate", "house", "actblue", "rightus", "winred"
  )
  yrs <- seq(2020, 2022, by = 2)
  for (c in categories) {
    for (y in yrs) {
      df_raw <- list.files("data/raw", pattern = c, full.names = TRUE) %>%
        keep(~ grepl("amount", .x) & grepl(y, .x)) %>%
        map(loadRData) %>%
        bind_rows() %>%
        as_tibble() %>%
        dedup() %>%
        mutate(across(everything(), ~ trimws(gsub("\\s+", " ", .x))))
      
      if (c == "house") {
        df_raw <- df_raw %>%
          mutate(
            state = case_when(
              is.na(state) & !is.na(state_cd) ~ str_sub(state_cd, 1, 2),
              TRUE ~ state
            )
          )
      }
      
      write_fst(df_raw, here(paste0("data/tidy/", c, "_", y, ".fst")))
    }
  }
} else {
  # Allow specifying categories outside this script
  if (!exists("yrs")) yrs <- 2020
  if (length(categories) > 1) {
    df_ls <- categories %>%
      set_names(., .) %>%
      map(
        ~ read_fst(here(paste0("data/tidy/", .x, "_", yrs, ".fst")))
      )
  } else if (length(categories) == 1) {
    df_raw <- read_fst(
      here(paste0("data/tidy/", categories, "_", yrs, ".fst"))
    ) %>%
      mutate(across(everything(), ~ trimws(gsub("\\s+", " ", .x))))
  }
}
