source("R/utilities.R")

# Import data ==================================================================
# Manually copied/renamed from solicitR data collection repository
# Seventeen .Rda files; no cleaning; altogether 400mb+

if (!file.exists("data/tidy/president_2020.fst")) {
  categories <- c(
    "president", "senate", "house", "actblue", "rightus", "winred"
  )
  df_ls <- categories %>%
    set_names(., .) %>%
    map(
      ~ list.files(
        "data/raw",
        pattern = .x, full.names = TRUE
      ) %>%
        map(loadRData) %>%
        bind_rows() %>%
        as_tibble() %>%
        dedup()
    )
  
  categories %>%
    map(
      ~ write_fst(df_ls[[.x]], paste0("data/tidy/", .x, "_2020.fst"))
    )
} else {
  # Allow specifying categories outside this script
  df_ls <- categories %>%
    set_names(., .) %>%
    map(
      ~ read_fst(paste0("data/tidy/", .x, "_2020.fst"))
    )
}
