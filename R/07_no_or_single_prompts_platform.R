source(here::here("R", "utilities.R"))
if (!dir.exists(here("tab", "number"))) {
  dir.create(here("tab", "number"), recursive = TRUE)
}
options(xtable.sanitize.text.function = identity)

# No need to import raw data ===================================================
dl <- loadRData(here("data/tidy/portfolio_summ_platforms.Rda")) %>%
  map(
    ~ .x %>%
      ungroup() %>%
      filter(!is.na(url) & url != "") %>%
      select(-contains("name_full")) %>%
      # This prevents data collection errors (one-time misses) from taking over
      filter(!(is.na(amount) & min_date == max_date))
  )

# No-prompt/single prompt links for platform fundraisers =======================
temp <- list(
  `No Defaults` = dl %>%
    imap_dfr(~ tibble(Platform = .y, perc = sum(.x$choices == 0) / nrow(.x))),
  `None, Federal` = dl %>%
    map(~ .x %>% filter(grepl("us |pres", class))) %>%
    imap_dfr(~ tibble(Platform = .y, perc = sum((.x)$choices == 0) / nrow(.x))),
  `None, Others` = dl %>%
    map(~ .x %>% filter(!grepl("us |pres", class))) %>%
    imap_dfr(~ tibble(Platform = .y, perc = sum((.x)$choices == 0) / nrow(.x))),
  `One Default` = dl %>%
    imap_dfr(~ tibble(Platform = .y, perc = sum(.x$choices == 1) / nrow(.x))),
  `One, Federal` = dl %>%
    map(~ .x %>% filter(grepl("us |pres", class))) %>%
    imap_dfr(~ tibble(Platform = .y, perc = sum((.x)$choices == 1) / nrow(.x))),
  `One, Others` = dl %>%
    map(~ .x %>% filter(!grepl("us |pres", class))) %>%
    imap_dfr(~ tibble(Platform = .y, perc = sum((.x)$choices == 1) / nrow(.x)))
) %>%
  bind_rows(.id = "type") %>%
  mutate(perc = perc * 100) %>%
  platform_names() %>%
  pivot_wider(id_cols = "Platform", names_from = "type", values_from = "perc")

# Export to xtable =============================================================
print(
  xtable(
    temp %>% rename_with(~ gsub(" ", " \\\\newline ", .x)),
    digits = c(0, 0, rep(2, 6)),
    align = "llXXXXXX"
  ),
  file = here("tab", "no_single_prompts_platform.tex"),
  booktabs = TRUE, include.rownames = FALSE, floating = FALSE,
  tabular.environment = "tabularx", width = ".8\\textwidth"
)

dl %>% map_dbl(~ Mode(.x$choices))

# Import data for specific federal races =======================================
dl <- loadRData(here("data/tidy/portfolio_summ_federal_final.Rda")) %>%
  map(~ .x %>% filter(!(is.na(amount) & min_date == max_date)))

dl %>% map_dbl(~ Mode(.x$choices))

temp <- list(
  `No Defaults` = dl %>%
    imap_dfr(
      ~ tibble(Race = simple_cap(.y), perc = sum(.x$choices == 0) / nrow(.x))
    ),
  `One Default` = dl %>%
    imap_dfr(
      ~ tibble(Race = simple_cap(.y), perc = sum(.x$choices == 1) / nrow(.x))
    )
) %>%
  bind_rows(.id = "type") %>%
  mutate(perc = perc * 100) %>%
  pivot_wider(id_cols = "Race", names_from = "type", values_from = "perc")

# Export to xtable =============================================================
print(
  xtable(
    temp %>% rename_with(~ gsub(" ", " \\\\newline ", .x)),
    digits = c(0, 0, rep(2, 2)),
    align = "llXX"
  ),
  file = here("tab", "no_single_prompts_race.tex"),
  booktabs = TRUE, include.rownames = FALSE, floating = FALSE,
  tabular.environment = "tabularx", width = ".35\\textwidth"
)

dl[c("senate", "house")] %>%
  map_dbl(nrow) %>%
  sum()
dl[c("senate", "house")] %>%
  map_dbl(~ nrow(.x %>% filter(choices == 0))) %>%
  sum()
