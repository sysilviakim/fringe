source(here::here("R", "utilities.R"))
if (!dir.exists(here("tab", "number"))) {
  dir.create(here("tab", "number"), recursive = TRUE)
}

# No need to import raw data ===================================================
load(here("data/tidy/portfolio_summ_platforms.Rda"))
dl <- dl %>%
  map(
    ~ .x %>%
      ungroup() %>%
      filter(!is.na(amount) & !is.na(url) & url != "") %>%
      select(-contains("name_full"))
  )

# No-prompt/single prompt links for platform fundraisers =======================
temp <- list(
  `No Suggestions` = dl %>%
    imap_dfr(~ tibble(Platform = .y, perc = sum(.x$choices == 0) / nrow(.x))),
  `One Suggestion` = dl %>%
    imap_dfr(~ tibble(Platform = .y, perc = sum(.x$choices == 1) / nrow(.x))),
  `One, Federal` = dl %>%
    map(~ .x %>% filter(grepl("us |pres", class))) %>%
    imap_dfr(~ tibble(Platform = .y, perc = sum((.x)$choices == 1) / nrow(.x))),
  `One, Others` = dl %>%
    map(~ .x %>% filter(!grepl("us |pres", class))) %>%
    imap_dfr(~ tibble(Platform = .y, perc = sum((.x)$choices == 1) / nrow(.x)))
) %>%
  bind_rows(.id = "type") %>%
  mutate(across(contains("Suggestion") | contains("One"), ~ .x * 100)) %>%
  platform_names() %>%
  pivot_wider(id_cols = "Platform", names_from = "type", values_from = "perc")

# Export to xtable =============================================================
print(
  xtable(temp),
  file = here("tab", "no_single_prompts_platform.tex"),
  booktabs = TRUE, include.rownames = FALSE, floating = FALSE
)

# Import data for specific federal races =======================================
categories <- c("president", "senate", "house")
source(here("R", "01_data_import.R"))


