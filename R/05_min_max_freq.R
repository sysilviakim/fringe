source(here::here("R", "utilities.R"))
if (!dir.exists(here("tab", "number"))) {
  dir.create(here("tab", "number"), recursive = TRUE)
}

# No need to import raw data ===================================================
load(here("data/tidy/portfolio_summ_platforms.Rda"))
dl <- dl %>% map(~ .x %>% filter(!is.na(amount) & !is.na(url) & url != ""))

# No-prompt/single prompt links ================================================
temp <- list(
  `No Suggestions` = dl %>% 
    imap_dfr(
      ~ tibble(Platform = .y, perc = sum(.x$choices == 0) / nrow(.x) * 100)
    ),
  `One Suggestion` = dl %>%
    imap_dfr(
      ~ tibble(Platform = .y, perc = sum(.x$choices == 1) / nrow(.x) * 100)
    ),
  `One, Federal` = dl %>%
    map(~ .x %>% filter(grepl("us |pres", class))) %>%
    imap_dfr(
      ~ tibble(Platform = .y, perc = sum((.x)$choices == 1) / nrow(.x) * 100)
    ),
  `One, Others` = dl %>%
    map(~ .x %>% filter(!grepl("us |pres", class))) %>%
    imap_dfr(
      ~ tibble(Platform = .y, perc = sum((.x)$choices == 1) / nrow(.x) * 100)
    ),
  `One, Single-entity` = dl["actblue"] %>%
    map(~ .x %>% filter(multi_entity == 0)) %>%
    imap_dfr(
      ~ tibble(Platform = .y, perc = sum((.x)$choices == 1) / nrow(.x) * 100)
    ),
  `One, Multi-entity` = dl["actblue"] %>%
    map(~ .x %>% filter(multi_entity == 1)) %>%
    imap_dfr(
      ~ tibble(Platform = .y, perc = sum((.x)$choices == 1) / nrow(.x) * 100)
    )
) %>%
  bind_rows(.id = "type") %>%
  platform_names() %>%
  pivot_wider(id_cols = "Platform", names_from = "type", values_from = "perc")

# Export to xtable =============================================================
print(
  xtable(temp), file = here("tab", "no_single_prompts.tex"),
  booktabs = TRUE, include.rownames = FALSE, floating = FALSE
)

# Multi-entity cases for ActBlue? 0.927305 vs. 0.3641329 so about 2.5 more time
# for single-entity fundraisers, but very little anyway
nrow(
  dl$actblue %>%
    filter(
      !is.na(amount) & !grepl("-", amount) &
        !is.na(url) & url != "" & multi_entity == 0
    )
) /
  nrow(dl$actblue %>% filter(!is.na(url) & url != "" & multi_entity == 0)) * 100
# [1] 0.927305

nrow(
  dl$actblue %>% filter(
    !is.na(amount) & !grepl("-", amount) &
      !is.na(url) & url != "" & multi_entity > 0
  )
) /
  nrow(dl$actblue %>% filter(!is.na(url) & url != "" & multi_entity > 0)) * 100
# [1] 0.3641329

# Modal menu percentage ========================================================
prop(dl$actblue, "amount", sort = TRUE, head = 1)
prop(dl$winred, "amount", sort = TRUE, head = 1)
prop(dl$rightus, "amount", sort = TRUE, head = 1)

# Frequency of maximum value by platform =======================================
max(dl$actblue$max) # 250,000
min(dl$actblue$max) # 1
prop(dl$actblue, "max", sort = TRUE, head = 5)
# max
# 1000  500  250  100 2800
# 57.7  9.7  6.2  5.4  2.2

max(dl$winred$max) # 250,000
min(dl$winred$max) # 1
prop(dl$winred, "max", sort = TRUE, head = 5)

max(dl$rightus$max)
min(dl$rightus$max)
prop(dl$rightus, "max", sort = TRUE, head = 5)

# Frequency of maximum value by platform =======================================
max(dl$actblue$min)
min(dl$actblue$min)
prop(dl$actblue, "min", sort = TRUE, head = 5)
