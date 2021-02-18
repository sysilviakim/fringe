categories <- "actblue"
source(here::here("R", "01_data_import.R"))

assert_that(all(!is.na(df_raw$url)))

temp <- df_raw %>%
  rename(name = fundraiser) %>%
  portfolio_summ(., exclude_cols = c("name", "year", "url"))

head(sort(table(temp$amount), decreasing = TRUE), 10)

# Top 5 Most Frequent Distributions ============================================
p <- prop(temp, "amount", sort = TRUE, head = 5, print = FALSE) %>%
  unlist() %>%
  set_names(., nm = names(.)) %>%
  imap(~ tibble(label = .y, freq = as.numeric(.x))) %>%
  bind_rows() %>%
  mutate(
    label = gsub("-", "\n", label),
    # So that length would match with WinRed/Right.us
    label = paste0(label, "\n"),
    label = factor(
      label,
      levels = gsub(
        "-", "\n",
        names(prop(temp, "amount", sort = TRUE, head = 5, print = FALSE))
      ) %>%
        paste0(., "\n")
    )
  ) %>%
  ggplot(aes(x = label, y = freq)) +
  geom_bar(stat = "identity") +
  # xlab("\nSolicitation Amounts, Top 5, ActBlue Directory") +
  xlab(NULL) + 
  ylab("Percentage (%)") + 
  scale_y_continuous(limits = c(0, 50))

pdf(here("fig/portfolio_freq_top_5_actblue.pdf"), width = 3, height = 3)
print(pdf_default(p))
dev.off()

# Save Output (Check for No-Prompt Referrals) ==================================
entities <- df_raw %>%
  rename(name = fundraiser) %>%
  select(!!c("name", "year")) %>%
  dedup()

View(anti_join(entities, temp))
nrow(full_join(temp, entities))

write_fst(
  full_join(temp, entities), 
  here("data/tidy/portfolio_summ_actblue_incomplete.fst")
)
