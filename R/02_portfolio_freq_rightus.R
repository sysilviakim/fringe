categories <- "rightus"
source(here::here("R", "01_data_import.R"))

assert_that(all(!is.na(df_raw$url)))

# Turn to portfolio patterns ===================================================
temp <- df_raw %>%
  # To kick out duplicates
  mutate(
    url = gsub(
      "\\?utm_source=Right.us&utm_medium=web&utm_campaign=directory&", "", url
    ),
    race = trimws(race),
    race_orig = race
  ) %>%
  # filter(!is.na(portfolio)) %>%
  # 0 for "Other"
  filter(portfolio > 0) %>%
  mutate(
    race = str_remove_all(
      race, c(seq(2018, 2026), "\\(.*?\\)|,") %>% paste0(collapse = "|")
    ),
    race = trimws(str_replace_all(race, "\\s{2,}", " "))
  ) %>%
  mutate(
    name = case_when(
      name == "Bob Cupp" & race == "State House Ohio District 4" &
        grepl("http://bobcupp.org/donate/", url) ~ "Robert Cupp",
      name == "Christine Bish" & race == "US House California District 6" &
        grepl("bish-for-congress/c4ebd2fa51523d54a7dc9", url) ~ "Chris Bish",
      name == "Bret Guthrie" & race == "US House Kentucky District 2" &
        grepl("brett-guthrie-for-congress/c2f680721e268126f6a08", url) ~
      "Brett Guthrie",
      name == "Daniel Crenshaw" & race == "US House Texas District 2" &
        grepl("crenshaw/c89f699a7ac209c4a0d2f", url) ~ "Dan Crenshaw",
      name == "Ronda Baldwin-Kennedy" &
        race == "US House California District 26" &
        grepl("ronda-kennedy-for-congress/c1c126c031f520d67376e", url) ~
      "Ronda Kennedy",
      name == "Virginia House GOP" & race == "Caucus Committee Virginia" &
        grepl("vahousegop/c9f894490d6e3ffb63ea7", url) ~
      "Virginia House Republican Campaign Committee",
      TRUE ~ name
    ),
    url = case_when(
      grepl("new-hampshire-senate-republican-nominee-fund-2020", url) &
        name == "New Mexico Senate Republican Nominee Fund 2020" ~
      gsub("hampshire", "mexico", url),
      TRUE ~ url
    )
  ) %>%
  mutate(
    race = case_when(
      name == "Brett Kokinadis" & race == "US House New Mexico District 3" ~ 
        "US House New Mexico District 1",
      name == "Butch Miller" & race == "State Senate Georgia District 50" ~ 
        "State Senate Georgia District 49",
      name == "Danny Malouf" & race == "US House Illinois District 17" ~ 
        "US House Illinois District 14",
      name == "James Burchett" & race == "State House Georgia District 178" ~ 
        "State House Georgia District 176",
      name == "James P. Bradley" & race == "US House California District 28" ~ 
        "US House California District 33",
      name == "Jim Marter" & race == "US House Illinois District 17" ~ 
        "US House Illinois District 14",
      name == "Joe Profit" & race == "US House Georgia District 7" ~ 
        "US House Georgia District 6",
      name == "John LaHood" & race == "State House Georgia District 172" ~ 
        "State House Georgia District 175",
      name == "John Wilkinson" & race == "State Senate Georgia District 49" ~
        "State Senate Georgia District 50",
      name == "Jon Burns" & race == "State House Georgia District 158" ~ 
        "State House Georgia District 159",
      name == "Kathie Hess Crouse" & 
        race == "State Senate West Virginia District 13" ~ 
        "State Senate West Virginia District 8",
      name == "Sean Feucht" & race == "US House California District 1" ~
        "US House California District 3",
      name == "Tom Killion" & race == "State Senate Pennsylvania District 7" ~
        "State Senate Pennsylvania District 9",
      TRUE ~ race
    )
  ) %>%
  mutate(
    race = case_when(
      grepl("US House", race) & 
        grepl(
          state_df %>% filter(al_2010 == 1) %>% .$stname %>%
            paste0(" ", ., " ", collapse = "|"),
          race
        ) ~
        gsub("District 1$", "District AL", race),
      TRUE ~ race
    )
  )
write_fst(temp, here("data/tidy/rightus_temp_race.fst"))

temp <- temp %>%
  select(-race_orig) %>%
  portfolio_summ(., exclude_cols = c("name", "race", "year", "url"))

head(sort(table(temp$amount), decreasing = TRUE), 10)

# Check for within-URL changes =================================================
temp %>%
  group_by(url, seq, amount) %>%
  filter(n() > 1) %>%
  arrange(url, seq) %>%
  # filter(first(name) != last(name)) %>%
  View()

# Top 5 Most Frequent Distributions ============================================
rightus <- temp %>% filter(grepl("anedot", url))
p <- prop(
  rightus %>%
    group_by(name, url, amount) %>%
    slice(1),
  "amount",
  sort = TRUE, head = 5, print = FALSE
) %>%
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

pdf(here("fig/portfolio_freq_top_5_rightus.pdf"), width = 3, height = 3)
print(pdf_default(p))
dev.off()

# No need to merge with entities ===============================================
save(rightus, file = here("data/tidy/portfolio_summ_rightus.Rda"))
