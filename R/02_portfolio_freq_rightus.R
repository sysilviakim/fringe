categories <- "rightus"
source(here::here("R", "01_data_import.R"))

assert_that(all(!is.na(df_ls$rightus$url)))

temp <- df_ls[[categories]] %>%
  # To kick out duplicates
  mutate(
    url = gsub(
      "\\?utm_source=Right.us&utm_medium=web&utm_campaign=directory&", "", url
    ),
    race = trimws(race)
  ) %>%
  filter(!is.na(portfolio)) %>%
  # 0 for "Other" 
  filter(portfolio > 0) %>%
  portfolio_summ(., exclude_cols = c("name", "race", "year", "url"))

head(sort(table(temp$amount), decreasing = TRUE), 10)

# Top 5 Most Frequent Distributions ============================================
p <- prop(temp, "amount", sort = TRUE, head = 5, print = FALSE) %>%
  unlist() %>%
  set_names(., nm = names(.)) %>%
  imap(~ tibble(label = .y, freq = as.numeric(.x))) %>%
  bind_rows() %>%
  mutate(
    label = gsub("-", "\n", label),
    label = factor(
      label,
      levels = gsub(
        "-", "\n",
        names(
          prop(temp, "amount", sort = TRUE, head = 5, print = FALSE)
        )
      )
    )
  ) %>%
  ggplot(aes(x = label, y = freq)) +
  geom_bar(stat = "identity") +
  # xlab("\nSolicitation Amounts, Top 5, Right.us Directory") +
  xlab(NULL) +
  ylab("Percentage (%)") +
  scale_y_continuous(limits = c(0, 50))

pdf(here("fig/portfolio_freq_top_5_rightus.pdf"), width = 3.5, height = 3.5)
pdf_default(p)
dev.off()

# Save Output (Check for No-Prompt Referrals) ==================================
entities <- df_ls[[categories]] %>%
  select(!!c("name", "race", "year")) %>%
  mutate(race = trimws(race)) %>%
  dedup()

View(anti_join(entities, temp))
nrow(full_join(temp, entities))

write_fst(
  full_join(temp, entities), here("data/tidy/portfolio_summ_rightus.fst")
)
